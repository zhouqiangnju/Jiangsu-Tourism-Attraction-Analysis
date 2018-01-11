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
library(maptools)
library(rgdal)

homefile<-'C:/Users/zhouq/Documents/GitHub/Jiangsu-Tourism-Attraction-Analysis'
workfile<-"F:/Administrator/Documents/GitHub/Jiangsu-Tourism-Attraction-Analysis"
cityorder<-c('NJ','WX','XZ','CZ','SZ','NT','LYG','HA','YC','YZ','ZJ','TZ','SQ')
cityorder<-factor(cityorder,levels = cityorder)
setwd(homefile)
#data input
jqdata<-read.csv('JVTnew.csv',stringsAsFactors = FALSE)[,-1]
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
#read jq location file
jqgeo<-read.csv('jqgeo.csv',stringsAsFactors = FALSE)[,-1]

jqgeo$city<-factor(jqgeo$city,levels = cityorder)
jqcity<-split(jqgeo,jqgeo$city)
#coordination transform(GCJ02-WGS84)
jqgeo$lng_wgs84<-gcj02_wgs84_lng(jqgeo$lng,jqgeo$lat)
jqgeo$lat_wgs84<-gcj02_wgs84_lat(jqgeo$lng,jqgeo$lat)
#correction
update<-read.csv('update.csv',stringsAsFactors = FALSE)
jqgeo$Name[match(update$Name,jqgeo$Name)]
jqgeo[,c(7,8)][match(update$Name,jqgeo$Name),]<-update[,c(2,3)]
jqinfo<-jqdata[,c(2,6)]
jqchengshi<-as.data.frame(table(jqinfo))
jqchengshi<-jqchengshi[which(jqchengshi$Freq>0),][,-3]

<<<<<<< HEAD
correction<-join(jqgeo,jqchengshi,by='Name')
correction$city<-sub('市',"",correction$city)
correction$match<-correction$city==correction$city

correction[which(correction$match=='FALSE'),c('lng','lat')][1,]<-c('118.9743','33.808995')
jqgeo<-as.data.frame(jqgeo)
#map of js
js_jq_map <- leaflet() %>%
  addTiles(
    'http://webrd0{s}.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}',
    options=tileOptions(tileSize=256, minZoom=9, maxZoom=17, subdomains="1234"),
    attribution = '&copy; <a href="http://ditu.amap.com/">高德地图</a>',
    group="Road Map"
  ) %>% 
  setView(118.788815,32.020729, zoom = 10)%>%
  addMarkers(jqgeo,lng=jqgeo$lng,lat=jqgeo$lat,popup=jqgeo$Name)
#map by city
citymap<-list()
length(citymap)<-13
names(citymap)<-as.character(cityorder)

=======
update<-read.csv('updata location.csv')
jqgeo$Name[match(update$Name,jqgeo$Name)]
jqgeo[,c(7,8)][match(update$Name,jqgeo$Name),]<-update[,c(2,3)]
#add markers to amap
>>>>>>> 2f523e284a08cf1d2e0b89360bb48f7cb126e8ae
add_jq_to_map<-function(x){
  p<-leaflet() %>%
  addTiles(
    'http://webrd0{s}.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}',
    options=tileOptions(tileSize=256, minZoom=9, maxZoom=17, subdomains="1234"),
    attribution = '&copy; <a href="http://ditu.amap.com/">高德地图</a>',
    group="Road Map"
  ) %>% 
  setView(x$lng[1],x$lat[1], zoom = 10)%>%
  addMarkers(x,lng=x$lng,lat=x$lat,popup=x$Name)
  return(p)}
js_jq_map<-add_jq_to_map(jqgeo)
js_jq_map

citymap<-lapply(jqcity, add_jq_to_map)
<<<<<<< HEAD
citymap$SZ
=======
citymap$YC
>>>>>>> 2f523e284a08cf1d2e0b89360bb48f7cb126e8ae

#format and output
for(i in 1:length(jqgeo$citycode)){
  jqgeo$geo[[i]]<-st_point(c(jqgeo$lng[i],jqgeo$lat[i]))
}
#output
jqgeo$geo<-st_sfc(jqgeo$geo,crs = 4326)
jqgeo.sf<-st_sf(jqgeo)
jqgeo$district<-as.character(jqgeo$district)
jqgeo.sp<-as(jqgeo,"Spatial")
st_write(jqgeo,"coord.shp")
writeOGR(jqgeo.sp,'coord1',driver="ESRI Shapefile")
writeSpatialShape(jqgeo.sp,"coord2.shp")
coord2<-readShapeSpatial("coord2.shp")
str(coord2)
write.csv(jqgeo,'jqgeo2.csv')

