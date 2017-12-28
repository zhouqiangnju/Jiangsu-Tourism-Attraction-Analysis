jqdata<-read.csv("F:/Administrator/Documents/R/Jiangsu Tourist Attractions/Data/JVT.csv",stringsAsFactors = FALSE)[,-1]
yearsum<-jqdata[which(jqdata$Year>=2011&jqdata$Month==12),]
y2011