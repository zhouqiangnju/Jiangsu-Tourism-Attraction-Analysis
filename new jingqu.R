#environment setup
library(sf)
library(dplyr)
library(maptools)
library("httr") 
library("magrittr") 
library("jsonlite")

#data input
jqdata<-read.csv("C:/Users/zhouq/Documents/GitHub/Jiangsu-Tourism-Attraction-Analysis/JVT.csv",stringsAsFactors = FALSE)[,-1]
jingqu<-st_read("C:/Users/zhouq/Documents/GitHub/Jiangsu-Tourism-Attraction-Analysis/jingquwen.shp",stringsAsFactors = FALSE)

#substract 
jq2017<-jqdata[which(jqdata$Year==2017&jqdata$Month==10),]
jqname<-as.character(unique(jqdata$Name))
name2017<-as.character(jq2017$Name)


x<-!match(jqname,name2017)
diff<-jqname[is.na(x)]
diff


jingqu$DENGJI[grep("A",jingqu$DENGJI[match(jingqu$JINGQU,name2017)])]<-jq2017$Rate[grep("A",jqdata$Rate[match(name2017,jingqu$JINGQU)])]

#jingqu name uniform
jingqusub<-substr(jingqu$JINGQU,1,6)
jq2017sub<-substr(jq2017$Name,1,5)
jingqusub<-sub("市","",jingqusub)
jq2017sub<-sub("市","",jq2017sub)
jingqu$Name<-"jiangsu"
 
jqdata$Name[grep("花果山",jqdata$Name)]<-"连云港花果山景区"
jqdata$Name[grep("马陵山",jqdata$Name)]<-"新沂马陵山景区"
jqdata$Name<-sub("苏州昆山","昆山",jqdata$Name)
jqdata$Name<-sub("无锡宜兴|无锡市宜兴","宜兴",jqdata$Name)
jqdata$Name<-sub("泰州姜堰","姜堰",jqdata$Name)
jqdata$Name<-sub("南京市","南京",jqdata$Name)

jqdata$Name[grep("寒山寺",jqdata$Name)]<-"苏州寒山寺"
jqdata$Name[grep("团氿",jqdata$Name)]<-"宜兴团氿景区"
jqdata$Name[grep("铁山寺",jqdata$Name)]<-"盱眙铁山寺国家森林公园"
jqdata$Name[grep("扬州博物",jqdata$Name)]<-"扬州博物馆"
jqdata$Name[grep("艾山九龙景区",jqdata$Name)]<-"邳州艾山九龙景区"
jqdata$Name[grep("恐龙",jqdata$Name)]<-"常州环球恐龙城"



jqdata$Name[grep("羽泉",jqdata$Name)]<-"连云港东海羽泉景区"
jqdata$Name[grep("太仓现代农业园",jqdata$Name)]<-"太仓现代农业园"
jqdata$Name[grep("灌云县大伊山风景区",jqdata$Name)]<-"灌云大伊山风景区"
jqdata$Name[grep("盐城市阜宁金沙湖旅游区",jqdata$Name)]<-"阜宁金沙湖旅游区"

jingqu$Name[grep("红山",jingqu$JINGQU)]<-name2017[grep("红山",name2017)]


jingqu$Name[164]<-jingqu$JINGQU[164]
jingqu$Name[160]<-"常州天目湖旅游区(御水温泉)"

#get jingqu address
name2017
GetJD <- function(address){
  url = "http://api.map.baidu.com/geocoder/v2/"
  header  <- c("User-Agent"="Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.79 Safari/537.36")
  payload = list(
    'output' = 'json', 
    'ak' = 'ahnETWIMdIcErB7vxPt7on8sDP8vydBi'
  )
}

#file format transformation and output
jingqusp<-as(jingqu,"Spatial")
writeSpatialShape(jingqusp,"jingquwen.shp")
write.csv(jqdata,"F:/Administrator/Documents/GitHub/Jiangsu-Tourism-Attraction-Analysis/JVT.csv")
