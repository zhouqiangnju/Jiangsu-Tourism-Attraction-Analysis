library(raster)
library(sf)

library(tidyverse)
library(tmap)
library(maptools)

#import data
getwd()

jqgeo_4326<-readRDS('js_jq(4A+).rds')
jqgeo<-st_transform(jqgeo_4326,crs=4498)
saveRDS(jqgeo_4326,'js_jq(4A+).rds')
nj_poi<-readRDS('~/GitHub/Jiangsu-Tourism-Attraction-Analysis/poi analysis/nj_poi_2018_sf.rds')%>% 
  st_transform(4498)
nj_town<-readRDS('~/GitHub/Jiangsu-Tourism-Attraction-Analysis/poi analysis/nj_town_sf.rds')%>% 
  st_transform(4498)
nj_frame<-readRDS('~/GitHub/Jiangsu-Tourism-Attraction-Analysis/poi analysis/nj_district_sf.rds')%>% 
  st_transform(4498)


#get nj jq buffer
nj_jq<-jqgeo%>% filter(city=='NJ') %>% 
       dplyr::select('district','Name','keyword','height','Rate')
nj_jq_buffer<-st_buffer(nj_jq,dist=1000)

#intersects
jq_poi<-nj_poi[nj_jq_buffer,]
plot(jq_poi$geo)
jq_poi_zone<-st_join(jq_poi,nj_jq_buffer)
tm_shape(nj_frame) +tm_polygons() +tm_shape(nj_jq_buffer)+tm_polygons()+qtm(nj_jq)
