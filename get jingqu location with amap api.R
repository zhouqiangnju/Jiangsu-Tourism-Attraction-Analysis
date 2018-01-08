library("httr") 
library("magrittr") 
library("jsonlite")
library("leafletCN")
library("leaflet")
library('reshape2')
library("dplyr")
library('plyr')
library('sf')
library('Rgctc')
library(maptools)
library(rgdal)
#data input
homefile<-'C:/Users/zhouq/Documents/GitHub/Jiangsu-Tourism-Attraction-Analysis/JVTnew.csv'
workfile<-"F:/Administrator/Documents/GitHub/Jiangsu-Tourism-Attraction-Analysis/JVTnew.csv"
jqdata<-read.csv(homefile,stringsAsFactors = FALSE)[,-1]
jqname<-as.character(unique(jqdata$Name))

testnames<-c('大丰知青纪念馆','无锡江苏学政文化旅游区')
realname<-c('大丰上海知青纪念馆','江阴江苏学政文化旅游区')
GetJD <- function(address){
  url = "http://restapi.amap.com/v3/geocode/geo"
  header  <- c("User-Agent"="Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.79 Safari/537.36")
  payload = list(
    'output' = 'json', 
    'key' ='7c6b6c0d1b641f4aa9cdb7d2229ae728'  )
  addinfo <- data.frame()
  for (i in address){
    payload[["address"]]=i
    tryCatch({
      web <- GET(url,add_headers(.headers = header),query = payload)
      content <- web %>% content(as="text",encoding="UTF-8") %>% fromJSON(flatten = TRUE)
      geoinfo<-content$geocodes
      geoinfo$lng<-as.numeric(substr(geoinfo$location,1,10))
      geoinfo$lat<-as.numeric(substr(geoinfo$location,12,19))
      geoinfo<-geoinfo[,c(1,2,3,4,5,7,16,17)]
      geoinfo$Name<-i
      addinfo<-rbind(addinfo,geoinfo)
    },error = function(e){
      cat(sprintf("任务【%s】处理失败!",i),sep = "/n")
      addfail<-c(rep("NA",8),i)
      addinfo <- rbind(addinfo,addfail)
    })
    Sys.sleep(runif(1))
    print(sprintf("正在抓取【%s】地址",i))
  }
  print("所有数据全部抓取完毕!!!")
  return(addinfo)
}

myresult<-GetJD(jqname)
failresult<-GetJD(testnames)
jqgeo<-rbind(myresult,failresult)
jqgeo$Name[c(230,231)]<-realname
#coordination transform
myresult2<-myresult
x_PI <- 3.14159265358979324 * 3000.0 / 180.0
x<-myresult$lng-0.0065
y<-myresult$lat-0.006
z<-sqrt(x^2+y^2)-0.00002*sin(y*x_PI)
theta<-atan2(y,x)-0.000003 * cos(x * x_PI)
myresult2$lng<-z*cos(theta)
myresult2$lat<-z*sin(theta)



#coordination transform(GCJ02-WGS84)
jqgeo$lng_wgs84<-gcj02_wgs84_lng(jqgeo$lng,jqgeo$lat)
jqgeo$lat_wgs84<-gcj02_wgs84_lat(jqgeo$lng,jqgeo$lat)


#correction
jqinfo<-jqdata[,c(2,6)]
jqchengshi<-as.data.frame(table(jqinfo))
jqchengshi<-jqchengshi[which(jqchengshi$Freq>0),][,-3]

correction<-join(jqgeo,jqchengshi,by='Name')
correction$city<-sub('市',"",correction$city)
correction$match<-correction$city==correction$City

correction[which(correction$match=='FALSE'),c('lng','lat')][1,]<-c('118.9743','33.808995')

sname<-jqinfo2$sname
#add markers to amap
p <- leaflet() %>%
  addTiles(
    'http://webrd0{s}.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}',
    options=tileOptions(tileSize=256, minZoom=9, maxZoom=17, subdomains="1234"),
    attribution = '&copy; <a href="http://ditu.amap.com/">高德地图</a>',
    group="Road Map"
  ) %>% 
  setView(118.788815,32.020729, zoom = 10)%>%
  addMarkers(jqgeo,lng=as.numeric(jqgeo$lng),lat=as.numeric(jqgeo$lat),popup=jqgeo$Name)
p
#format and output


for(i in 1:length(jqgeo$citycode)){
  jqgeo$geo[[i]]<-st_point(c(jqgeo$lng[i],jqgeo$lat[i]))
}

jqgeo$geo<-st_sfc(jqgeo$geo,crs = 4326)
jqgeo<-st_sf(jqgeo)
jqgeo$district<-as.character(jqgeo$district)
jqgeo<-as(jqgeo,"Spatial")
st_write(jqgeo,"coord.shp")
writeOGR(jqgeo,'coord',driver="ESRI Shapefile")
writeSpatialShape(jqgeo,"coord.shp")
