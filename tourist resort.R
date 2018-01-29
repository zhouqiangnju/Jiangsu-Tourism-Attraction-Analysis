tr_xian<-st_read('F:/Administrator/Documents/R/Map/空间信息数据/度假区/touris_resorts.shp')
st_crs(tr)<-2437
class(tr)
st_crs(tr)
st_crs(tr,wkt)
tr<-st_transform(tr,4326)
library(ggplot2)
ggplot(tr)+