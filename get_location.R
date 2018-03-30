library(dplyr)
library(openxlsx)
library("httr") 

library("jsonlite")
library("leafletCN")
library("leaflet")

library(rlist)
library('plyr')
library('sf')
library('Rgctc2',lib.loc='~/GitHub/R_coordination_transformation')
library(maptools)
library('rgdal')
library(stringr)
library(leaflet)

jqinfo<-read.xlsx('F:/Administrator/Documents/R/Jiangsu Tourist Attractions/江苏省A级旅游景区名录(截至2017年12月31日）.xlsx')
names(jqinfo)<-c('No.','Rate','City','Name','URL')
jq3A<-filter(jqinfo,Rate!='4A',Rate!='5A') %>% select(-URL)
null_table<-data.frame('formatted_address'=0,'city'=0,'district'=0,'location'=0,'level'=0,'wgs84_lng'=0,'wgs84_lat'=0)
get_location <- function(address,city){
  key<-'7c6b6c0d1b641f4aa9cdb7d2229ae728'
  adcode<-320000
  api<-str_glue(
    'http://restapi.amap.com/v3/geocode/geo?address={address}&output=json&key={key}&city={city}')
  
    info <-GET(api) %>% content(as='text',encoding = 'UTF-8') %>% fromJSON(flatten=TRUE)
    if(info$count!='0'){
    info<- info%>%  '[['('geocodes') %>% select('formatted_address','city','district','location','level')
    center<-info$location %>% str_split(',') %>% lapply(as.numeric) %>% lapply(gcj02_wgs84_point) %>% unlist
    info<-mutate(info,wgs84_lng=center[1],wgs84_lat=center[2],name=address)
    }
    else{
    info<-mutate(null_table,name=address,city=city)
    }
  
  return(info)   
}
  
 
setwd('F:/Administrator/Documents/GitHub/Jiangsu-Tourism-Attraction-Analysis')
write.csv(jq_3A,'jq3a.csv')
jq3A<-read.csv('jq3a.csv',stringsAsFactors = FALSE)

jq3A$Name[grep('钱穆',jq_3A$Name)]<-'钱穆钱伟长故居'
jq3A$Name<-jq3A$Name %>% str_trim 
vacum<-list()
for(i in 1:length(search_name)) {vacum[[i]]<-get_location(jq3A$Name[i],jq3A$City[i])  }
center<-table$location %>% str_split(',')  %>% lapply(sapply,as.numeric) %>% list.rbind
class(center[,1])
table<- list.rbind(vacum)
table<-mutate(table,amap_lng=center[,1],amap_lat=center[,2])

add_jq_to_map<-function(x){
  p<-leaflet() %>%
    addTiles(
      'http://webrd0{s}.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}',
      options=tileOptions(tileSize=256, minZoom=9, maxZoom=17, subdomains="1234"),
      attribution = '&copy; <a href="http://ditu.amap.com/">高德地图</a>',
      group="Road Map"
    ) %>% 
    setView(x$amap_lng[1],x$amap_lat[1], zoom = 10)%>%
    addMarkers(x,lng=x$amap_lng,lat=x$amap_lat,popup=x$name)
  return(p)}
t<-add_jq_to_map(table)
t
class(table$amap_lng)
t<-'大丰丹顶鹤珍禽园景区' %>% get_location
search_name<-unique(jq_3A$Name)
Name<-sample(Name,100)
table<-lapply(Name,get_location)
table_table<-list.rbind(table)
jqlist<-as.character(unique(jq_3A$Name))
test<-'xjsdl'
write.csv(table,'geo3a.csv')
