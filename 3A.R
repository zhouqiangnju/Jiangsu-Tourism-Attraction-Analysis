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
library(readxl)
jq_3a<-read_xls('C:/Users/zhouq/Desktop/江苏省a级景区情况（2017年12月31日）.xls',sheet='3A',range='B3:E224')
names(jq_3a)<-c('Name','City','Rate','Date')
jq_3a$Year<-substr(jq_3a$Date,1,4)
jq_3a$Name<-sub('江苏省','',jq_3a$Name)
write.csv(jq_3a,'jingqu3A.csv')
jq_3A<-read.csv('jingqu3A.csv')[,-1]
jqlist<-as.character(unique(jq_3A$Name))
#GetJD函数在直接读取excel中文编码时会出错，需先写成csv文件后读取
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
      geoinfo<-geoinfo[,c(16,17)]
      geoinfo$Name<-i
      addinfo<-rbind(addinfo,geoinfo)
    },error = function(e){
      cat(sprintf("任务【%s】处理失败!",i),sep = "/n")
      addfail<-c("NA",'NA',i)
      addinfo <- rbind(addinfo,addfail)
    })
    Sys.sleep(runif(1))
    print(sprintf("正在抓取【%s】地址",i))
  }
  print("所有数据全部抓取完毕!!!")
  return(addinfo)
}
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

  location_3a<-GetJD(jqlist)
  map_3A<-add_jq_to_map(location_3a)
  map_3A
  jqlist[1]
  GetJD('南京市石头城遗址公园')
  