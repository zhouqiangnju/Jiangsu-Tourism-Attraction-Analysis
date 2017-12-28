#environment setup
library(sf)
library(dplyr)
library(maptools)

#data input
jqdata<-read.csv("F:/Administrator/Documents/R/Jiangsu Tourist Attractions/Data/JVT.csv",stringsAsFactors = FALSE)[,-1]
jingqu<-st_read("F:/Administrator/Documents/R/Map/2011dian.shp",stringsAsFactors = FALSE)

#substract 
jq2017<-jqdata[which(jqdata$Year==2017&jqdata$Month==10),]
jq2011<-jqdata[which(jqdata$Year==2011&jqdata$Month==12),]
name2017<-as.character(jq2017$Name)
name2011<-as.character(jq2011$Name)

x<-!match(name2017,name2011)
diff<-name2017[is.na(x)]

jingqu$DENGJI[grep("A",jingqu$DENGJI[match(jingqu$JINGQU,name2017)])]<-jq2017$Rate[grep("A",jqdata$Rate[match(name2017,jingqu$JINGQU)])]

#jingqu name uniform
jingqusub<-substr(jingqu$JINGQU,1,6)
jq2017sub<-substr(jq2017$Name,1,5)
jingqusub<-sub("县","",jingqusub)
jq2017sub<-sub("市","",jq2017sub)
jingqu$Name<-"jiangsu"
x<-c()
for(i in 1:203){
  jingqu$Name[grep(jq2017sub[i],jingqu$JINGQU)]<-name2017[grep(jq2017sub[i],name2017)]
  }
  

jingqu$Name[grep("红山",jingqu$JINGQU)]<-name2017[grep("红山",name2017)]

grep(jq2017sub[97],jingqu$JINGQU)
grep("亭林园",jingqu$JINGQU)
grep("亭林园",jq2017sub)

jingqu$Name[164]<-jingqu$JINGQU[164]
jingqu$Name[160]<-"常州天目湖旅游区(御水温泉)"

#file format transformation and output
jingqusp<-as(jingqu,"Spatial")
writeSpatialShape(jingqusp,"jingquwen.shp")
