mapprepare<-function(){
library(maptools)
library(ggplot2)
library(plyr)
library(sp)
library(rgdal)
library(sf)
library(leafletCN)
library(leaflet)
}
mapprepare()

#read data
jingqu<-st_read("F:/Administrator/Documents/R/Map/2011dian.shp")
city<-st_read("F:/Administrator/Documents/Mapproject/shi_region_Project_1.shp")
lake<-st_read("F:/Administrator/Documents/R/Map/空间信息数据/background/湖泊.shp")

#data preparation
jingqu<-jingqu[grep("4|5",jingqu$DENGJI),]

#Visualization
p<-ggplot()+geom_sf(data=city)+geom_sf(data=lake,colour="blue")+geom_sf(data=jingqu,colour="red")+coord_sf()
p

p<-ggplot()+geom_path(data=citylocation,aes(x=long,y=lat,group=group))+coord_map()

theme_clean <- function(base_size=12){
  
  require(grid)
  
  theme_grey(base_size)
  
  theme(
    
    axis.title = element_blank(),
    
    axis.text = element_blank(),
    
    panel.background = element_blank(),
    
    panel.grid = element_blank(),
    
    axis.ticks.length = unit(0, "cm"),
    

    
    plot.margin = unit(c(0,0,0,0), "lines"),
    
    complete = TRUE
    
  )
  
}

p+theme_clean()
## Loading required package: grid
#Map
regionNames("????")
demomap("????")

