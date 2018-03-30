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

jqinfo<-read.xlsx('F:/Administrator/Documents/R/Jiangsu Tourist Attractions/½­ËÕÊ¡A¼¶ÂÃÓÎ¾°ÇøÃûÂ¼(½ØÖÁ2017Äê12ÔÂ31ÈÕ£©.xlsx')
names(jqinfo)<-c('No.','Rate','City','Name','URL')
jq_3A<-filter(jqinfo,Rate!='4A',Rate!='5A') %>% select(-URL)
null_table<-data.frame('formatted_address'=0,'city'=0,'district'=0,'location'=0,'level'=0,'wgs84_lng'=0,'wgs84_lat'=0)
get_location <- function(address){
  key<-'7c6b6c0d1b641f4aa9cdb7d2229ae728'
  adcode<-320000
  api<-str_glue(
    'http://restapi.amap.com/v3/geocode/geo?address={address}&output=json&key={key}&city={adcode}')
  
    info <-GET(api) %>% content(as='text',encoding = 'UTF-8') %>% fromJSON(flatten=TRUE)
    if(info$count!='0'){
    info<- info%>%  '[['('geocodes') %>% select('formatted_address','city','district','location','level')
    center<-info$location %>% str_split(',') %>% lapply(as.numeric) %>% lapply(gcj02_wgs84_point) %>% unlist
    info<-mutate(info,wgs84_lng=center[1],wgs84_lat=center[2],name=address)
    }
    else{
    info<-mutate(null_table,name=address)
    }
  
  return(info)   
}
  
 
setwd('F:/Administrator/Documents/GitHub/Jiangsu-Tourism-Attraction-Analysis')
write.csv(jq_3A,'jq3a.csv')
jq_3A<-read.csv('jq3a.csv',stringsAsFactors = FALSE)
test<-c(jq_3A$Name[1:2])
jq_3A$Name[grep('Ç®ÄÂ',jq_3A$Name)]<-'Ç®ÄÂÇ®Î°³¤¹Ê¾Ó'
search_name<-jq_3A$Name %>% unique
vacum<-list()
for(i in 1:length(search_name))
vacum[[i]]<-get_location(search_name[i])  

t<-'´ó·áµ¤¶¥º×ÕäÇÝÔ°¾°Çø' %>% get_location
search_name<-unique(jq_3A$Name)
Name<-sample(Name,100)
table<-lapply(Name,get_location)
table_table<-list.rbind(table)
jqlist<-as.character(unique(jq_3A$Name))
test<-'xjsdl'
