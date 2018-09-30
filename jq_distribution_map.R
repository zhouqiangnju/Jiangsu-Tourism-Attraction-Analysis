library(Cairo)
js_frame=readRDS('~/R/Mapproject/JSframe/js_city_sf.rds')
showtext.auto()
cairo_pdf()
tm_shape(js_frame)+tm_polygons(col='adcode',legend.show = F) +
  tm_shape(js_jq_4A)+tm_dots(shape = 'Rate',size=0.25,col='red')+
  tm_shape(jq_5A)+tm_text('Name',size=0.7,remove.overlap=T)
dev.off()
js_jq_4A= readRDS('~/GitHub/Jiangsu-Tourism-Attraction-Analysis/poi analysis/js_jq(4A+).rds') %>% filter(Rate=='5A'|Rate=='4A')
jq_5A = filter(js_jq_4A,Rate=='5A',remove.overlap=T)
?tm_text
