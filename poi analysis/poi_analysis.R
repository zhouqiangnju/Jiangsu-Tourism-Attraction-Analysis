library(pacman)
p_load('sf',"tidyverse",'tmap','rgdal','maptools','raster')

st_crs(4498)
#import data
getwd()
nj_jq_poly = st_read('F:/Administrator/Documents/Tencent Files/10323671/FileRecv/南京市高等级旅游景区(1).kml')%>%st_transform(4498)
nj_jq_poly_buffer = st_buffer(nj_jq_poly,dist=200)
tm_shape(nj_jq_poly_buffer)+tm_polygons()
jqgeo_4326<-readRDS('js_jq(4A+).rds')
jqgeo<-st_transform(jqgeo_4326,crs=4498)

saveRDS(jqgeo_4326,'js_jq(4A+).rds')
nj_poi<-readRDS('~/GitHub/Jiangsu-Tourism-Attraction-Analysis/poi analysis/nj_poi_2018_modify.rds')%>% 
  st_transform(4498)
nj_town<-readRDS('~/GitHub/Jiangsu-Tourism-Attraction-Analysis/poi analysis/nj_town_sf.rds')%>% 
  st_transform(4498)
nj_frame<-readRDS('~/GitHub/Jiangsu-Tourism-Attraction-Analysis/poi analysis/nj_district_sf.rds')%>% st_transform(4498)

jq_poi= nj_poi[nj_jq_poly,]
nj_jq_poly=select(nj_jq_poly,-Description)
nj_jq_poly$area = st_area(nj_jq_poly)
jq_poi_info = st_join(jq_poi,nj_jq_poly)
poi_n = jq_poi_info %>% group_by(Name) %>% tally()
buffer_poi = nj_poi[nj_jq_poly_buffer,] %>% st_join(nj_jq_poly_buffer)
poi_buffer_n = buffer_poi %>% group_by(Name)%>% tally()
tm_shape(jq_poi)+tm_dots()
poi_buffer_n
#get nj jq buffer
nj_jq<-jqgeo%>% filter(city=='NJ') %>% 
       dplyr::select('district','Name','keyword','height','Rate')
nj_jq_buffer<-st_buffer(nj_jq,dist=1000)

#intersects
jq_poi<-nj_poi[nj_jq_buffer,]
plot(jq_poi$geo)
jq_poi_zone<-st_join(jq_poi,nj_jq_buffer)
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


 