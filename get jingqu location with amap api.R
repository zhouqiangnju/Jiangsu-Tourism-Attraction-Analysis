library("httr") 
library("magrittr") 
library("jsonlite")
library("leafletCN")
library("leaflet")
library('reshape2')
library("dplyr")
library('plyr')
library('sf')
library('Rgctc2')
#data input

jqdata<-read.csv("F:/Administrator/Documents/GitHub/Jiangsu-Tourism-Attraction-Analysis/JVTnew.csv",stringsAsFactors = FALSE)[,-1]
jqname<-as.character(unique(jqdata$Name))

testnames<-c('大丰知青纪念馆','无锡江苏学政文化旅游区')
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
      addfail<-c(rep("NA",length(names(addinfo))-1),i)
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

#coordination transform(GCJ02-WGS84)
jqgeo$lng_wgs84<-gcj02_wgs84_lng(jqgeo$lng,jqgeo$lat)
jqgeo$lat_wgs84<-gcj02_wgs84_lat(jqgeo$lng,jqgeo$lat)

#correction
jqinfo<-jqdata[,c(2,6)]
jqchengshi<-as.data.frame(table(jqinfo))
jqchengshi<-jqchengshi[which(jqchengshi$Freq>0),][,-3]

correction<-join(finalresult,jqchengshi,by='Name')
correction$city<-sub('市',"",correction$city)
correction$match<-correction$city==correction$City

correction[which(correction$match=='FALSE'),c('lng','lat')][1,]<-c('118.9743','33.808995')
correctinfo<-correction[,c(-11)]
#add markers to amap
p <- leaflet() %>%
  addTiles(
    'http://webrd0{s}.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}',
    options=tileOptions(tileSize=256, minZoom=9, maxZoom=17, subdomains="1234"),
    attribution = '&copy; <a href="http://ditu.amap.com/">高德地图</a>',
    group="Road Map"
  ) %>% 
  setView(118.788815,32.020729, zoom = 10)%>%
  addMarkers(correctinfo,lng=as.numeric(correctinfo$lng),lat=as.numeric(correctinfo$lat),popup=correctinfo$Name)
p
#format and output

correctinfo<-as.data.frame(correctinfo)
correctinfo$geo<-list()

for(i in 1:length(correctinfo$citycode)){
  geo[[i]]<-st_point(c(correctinfo$lng[i],correctinfo$lat[i]))
}
correctinfo$geo<-st_as_sfc(correctinfo$geo)
st_crs(correctinfo)<-"+proj=longlat +datum=NAD27 +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat"
geo<-st_as_sfc(geo)
st_write(geo,"co.shp")
