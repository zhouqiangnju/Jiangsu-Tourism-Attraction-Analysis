library(pacman)
p_load('sf',"tidyverse",'rgdal','maptools','raster','showtext')

library('tmap')

#import data
getwd()
js_jq_poly =readRDS('F:/Administrator/Documents/Map/旅游数据/boundry_of_js_highrate_jq/boundry_of_js_highrate_jq/jq_poly_0810.rds')
nj_jq_poly=filter(js_jq_poly,grepl('南京',Name)) %>% st_transform(4498)
nj_jq=unclass(nj_jq_poly) %>% as.data.frame() %>%dplyr::select(Name,Rate)


nj_poi<-readRDS('~/GitHub/Jiangsu-Tourism-Attraction-Analysis/poi analysis/nj_poi_2018_modify.rds')%>% st_transform(4498) %>% dplyr::select(-c(地址,市,WGS84_经度,WGS84_纬度))

nj_town<-readRDS('~/GitHub/Jiangsu-Tourism-Attraction-Analysis/poi analysis/nj_town_sf.rds')
nj_frame<-readRDS('~/GitHub/Jiangsu-Tourism-Attraction-Analysis/poi analysis/nj_district_sf.rds')%>% st_transform(4498)


nj_jq_poly=select(nj_jq_poly,-Description)
nj_jq_poly$area = st_area(nj_jq_poly)
jq_poi_info = st_join(jq_poi,nj_jq_poly)
poi_n = jq_poi_info %>% group_by(Name) %>% tally()
buffer_poi = nj_poi[nj_jq_poly_buffer,] %>% st_join(nj_jq_poly_buffer)
poi_buffer_n = buffer_poi %>% group_by(Name)%>% tally()
tm_shape(jq_poi)+tm_dots()
poi_buffer_n
#get nj jq buffer
jq_poi= nj_poi[nj_jq_poly,]
jq_poi_info = st_join(jq_poi,nj_jq_poly)
summary=group_by(jq_poi_info,Name)%>% tally()
buffer=list()
buffer[[1]]=nj_jq_poly
for(i in 2:6)
  buffer[[i]]=st_buffer(nj_jq_poly,dist=(i-1)*100)
buffer_belt=list()
buffer_long=nj_jq_poly[,-3]
buffer_long$Buff=0
buffer_belt[[1]]=buffer_long
for(i in 2:6){
  buffer_inner=buffer[[i-1]] 
  buffer_outer=buffer[[i]]
  belt=map2(buffer_outer$geo,buffer_inner$geo,st_difference)%>% st_sfc(crs=4498)
  
  buffer_belt[[i]]=st_sf(Name=as.character(nj_jq$Name),Rate=nj_jq$Rate,Buff=(i-1)*100,geo=belt)
  buffer_belt[[i]]
  buffer_long=rbind(buffer_long,buffer_belt[[i]])
}
buffer_long
zsl=filter(buffer_long,Name=='南京中山陵园风景区')
saveRDS(buffer_long,'buffer_long.rds')
tm_shape(buffer_belt[[1]])+tm_polygons()+
  tm_shape(buffer_belt[[2]])+tm_polygons(col='blue')+
  tm_shape(buffer_belt[[3]])+tm_polygons(col='red')+
  tm_shape(buffer_belt[[4]])+tm_polygons(col='yellow')+
  tm_shape(buffer_belt[[5]])+tm_polygons(col='green')+
  tm_shape(buffer_belt[[6]])+tm_polygons(col='pink')

#intersects
x=filter(buffer_poi,Name=='南京中山陵园风景区')
tm_shape(buffer_long[buffer_long$Name=='南京中山陵园风景区',])+tm_polygons()+
  tm_shape(x)+tm_dots()
tm_shape(zsl[6,])+tm_polygons()
buffer_poi=nj_poi[buffer_long,] %>% st_join(buffer_long)
saveRDS(buffer_poi,'buffer_poi.rds')
poi = st_join(nj_poi,buffer_long)
rim_n= group_by(buffer_poi,Name,大类)%>% tally()
?group_by
?aggregate
poi_dup=poi%>% na.omit()
plot(jq_poi$geo)
jq_poi_zone<-st_join(buffer,nj_jq_buffer)
tm_shape(nj_frame) +tm_polygons() +tm_shape(nj_jq_buffer)+tm_polygons()+qtm(nj_jq)

#retail spatial model analysis
nj_retail= filter(nj_poi,大类=='购物服务')
tm_shape(nj_frame) +tm_polygons() +qtm(nj_retail)
nj_raster_template<-raster(extent(nj_frame),resolution=1000,
                           crs=st_crs(nj_frame)$proj4string,vals=-1)

nj_raster_template=mask(nj_raster_template,mask=nj_frame)
plot(nj_raster_template)
nj_retail_density=rasterize(nj_retail,nj_raster_template,field=1,fun='count')
plot(nj_retail_density)
nj_retail_density
library(splancs)
library(spatstat)
library(spatstat.data)
poly<-as.points(list(x=c(nj_poly$xmin,nj_poly$xmin,nj_poly$xmax,nj_poly$xmax),y=c(nj_poly$ymin,nj_poly$ymax,nj_poly$ymax,nj_poly$ymin)))
data(spred)
sG=Sobj_SpatialGrid(spred,maxDim=100)$SG
nj_points=list(x=st_coordinates(nj_frame$poly)[,1],y=st_coordinates(nj_frame$poly)[,2])
nj_poly =st_bbox(nj_frame)             
?Sobj_SpatialGrid
#kernel density analysis 
  mserwq=mse2d(as.points(st_coordinates(nj_retail)),as.points(nj_points),100,0.15)
plot(mserwq$h[1:100],mserwq$mse[1:100], type="l")
st_coordinates(nj_frame$poly)
bwq=mserwq$h[which.min(mserwq$mse)]
bwq
mserw=bw.diggle(as(nj_retail_sp,'ppp')) %>% as.numeric
nj_retail_sp=as(nj_retail,'Spatial')

x<-as(nj_retail_sp,'ppp')
x$x
x$n
x$window

 