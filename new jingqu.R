library(sf)
library(dplyr)
jqdata<-read.csv("F:/Administrator/Documents/R/Jiangsu Tourist Attractions/Data/JVT.csv",stringsAsFactors = FALSE)[,-1]
jq2017<-jqdata[which(jqdata$Year==2017&jqdata$Month==10),]
jq2011<-jqdata[which(jqdata$Year==2011&jqdata$Month==12),]
name2017<-as.character(jq2017$Name)
name2011<-as.character(jq2011$Name)

x<-!match(name2017,name2011)
diff<-name2017[is.na(x)]

jingqu<-st_read("F:/Administrator/Documents/R/Map/2011dian.shp",stringsAsFactors = FALSE)

match(jingqu$JINGQU,name2017)
jingqu$DENGJI[grep("A",jingqu$DENGJI[match(jingqu$JINGQU,name2017)])]<-
  jq2017$Rate[grep("A",jq2017$Rate[match(name2017,jingqu$JINGQU)])]
  length(jq2017$Rate[grep("A",jq2017$Rate[match(name2017,jingqu$JINGQU)])])
                       

jingqu$DENGJI[grep("A",jingqu$DENGJI[match(jingqu$JINGQU,name2017)])]<-jq2017$Rate[grep("A",jqdata$Rate[match(name2017,jingqu$JINGQU)])]
X<-jingqu[grep("4|5",jingqu$DENGJI),]
st_write(jingqu,"js-jingqu-2017.shp")
y<-join(jingqu,name2017,)
?join
