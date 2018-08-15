library(pacman)
p_load('sf',"tidyverse",'tmap','rgdal','maptools','raster','showtext')

cgcs2000_geo=st_crs(4490)
cgcs2000_proj_lon0117 =st_crs(4498)
crs=make_EPSG()
#import data
getwd()
js_jq_poly =readRDS('F:/Administrator/Documents/Map/旅游数据/boundry_of_js_highrate_jq/boundry_of_js_highrate_jq/jq_poly_0810.rds')
nj_jq_poly=filter(js_jq_poly,grepl('南京',Name)) %>% st_transform(cgcs2000_proj_lon0117)





nj_poi<-readRDS('~/GitHub/Jiangsu-Tourism-Attraction-Analysis/poi analysis/nj_poi_2018_modify.rds')%>% st_transform(4498)

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
dist=1:5*100

for (i in 1:5)
{nj_jq_poly_buffer[[i]]=st_buffer(nj_jq_poly,dist=buff_dist[i])
}

x=st_difference(buffer_100,nj_jq_poly)
plot(x$geo)
buffer_500 =nj_jq_poly_buffer[[5]]
buffer_collect=list(buffer_100,buffer_200,buffer_300,buffer_400,buffer_500)
buffer_poi=list()
for(i in 1:5)
buffer_poi[[i]]=nj_poi[buffer_collect[[i]],] %>% st_join(buffer_collect[[i]])


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


 