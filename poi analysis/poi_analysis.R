library(sf)
library(raster)
library(tidyverse)
library(rgdal)
library(gdalUtils)
library(purrr)
library(tmap)
library(sp)
library(rgdal)
library(maptools)
#import data
jqgeo<-read.csv('C:/Users/zhouq/Documents/GitHub/jingqu-comfort-index/jqgeo2018_new_utf-8_rated.csv',stringsAsFactors = F)
nj_poi<-readRDS('C:/Users/zhouq/Documents/Mapdata/map/20个城市的POI数据/POI数据整理城市/南京/CSV版本/nj_poi_sf.rds')
nj_poi<-nj_poi%>% st_transform(crs=4498)
nj<-readRDS('C:/Users/zhouq/Documents/Mapdata/map/frame/nj_town.rds')%>% 
  st_transform(4498)

#create sf object
for(i in 1:nrow(jqgeo))
  jqgeo$geo[[i]]<-st_point(c(jqgeo$lng_wgs84[i],jqgeo$lat_wgs84[i]))
jqgeo$geo <-st_sfc(jqgeo$geo,crs=4326)
jqgeo<-jqgeo %>% st_sf 
jqgeo.sp<-as(jqgeo,'Spatial')
writeSpatialShape(jqgeo.sp,"jqgeo2.shp")


#get nj jq buffer
nj_jq<-filter(jqgeo,city=='NJ') %>% st_transform(crs=4498) %>%
       select('district','Name','keyword','height','Rate')
nj_jq_buffer<-st_buffer(nj_jq,dist=500)
#intersects
jq_poi<-nj_poi[nj_jq_buffer,]
plot(jq_poi$geo)
jq_poi_zone<-st_join(jq_poi,nj_jq_buffer)
