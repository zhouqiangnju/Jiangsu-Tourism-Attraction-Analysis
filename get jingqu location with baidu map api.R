library("httr") 
library("magrittr") 
library("jsonlite")
library("devtools")
library("leafletCN")
library("leaflet")
install.packages("baidumap")
library("dplyr")
#data input
jqdata<-read.csv("F:/Administrator/Documents/GitHub/Jiangsu-Tourism-Attraction-Analysis/JVT.csv",stringsAsFactors = FALSE)[,-1]
jqname<-as.character(unique(jqdata$Name))

GetJD <- function(address){
  url = "http://api.map.baidu.com/geocoder/v2/"
  header  <- c("User-Agent"="Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.79 Safari/537.36")
  payload = list(
    'output' = 'json', 
    'ak' ='ahnETWIMdIcErB7vxPt7on8sDP8vydBi'  )
  addinfo <- data.frame()
  for (i in jqname){
    payload[["address"]]=i
    tryCatch({
      web <- GET(url,add_headers(.headers = header),query = payload)
      content <- web %>% content(as="text",encoding="UTF-8") %>% fromJSON(flatten = TRUE) %>% `[[`(2) %>% `[[`(1)
      addinfo <- rbind(addinfo,content)
    },error = function(e){
      cat(sprintf("任务【%s】处理失败!",i),sep = "\n")
      addinfo <- rbind(addinfo,list("lng" = NA ,"lat" = NA))
    })
    Sys.sleep(runif(1))
    print(sprintf("正在抓取【%s】地址",i))
  }
  print("所有数据全部抓取完毕!!!")
  return(addinfo) 
}
myresult<-GetJD(jqname)
myresult$Name<-jqname

#coordination transform
myresult2<-myresult
x_PI <- 3.14159265358979324 * 3000.0 / 180.0
x<-myresult$lng-0.0065
y<-myresult$lat-0.006
z<-sqrt(x^2+y^2)-0.00002*sin(y*x_PI)
theta<-atan2(y,x)-0.000003 * cos(x * x_PI)
myresult2$lng<-z*cos(theta)
myresult2$lat<-z*sin(theta)
#correction
m <- leaflet() %>%
  addTiles(
    'http://webrd0{s}.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}',
    options=tileOptions(tileSize=256, minZoom=9, maxZoom=17, subdomains="1234"),
    attribution = '&copy; <a href="http://ditu.amap.com/">高德地图</a>',
    group="Road Map"
  ) %>% 
  setView(118.788815,32.020729, zoom = 10)%>%
  addMarkers(myresult2,lng=myresult2$lng,lat=myresult2$lat,popup=myresult$Name)
m
baidulocation<-myresult
gaodelocation<-myresult2
