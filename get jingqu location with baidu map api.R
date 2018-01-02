library("httr") 
library("magrittr") 
library("jsonlite")
#data input
jqdata<-read.csv("F:/Administrator/Documents/R/Jiangsu Tourist Attractions/Data/JVT.csv",stringsAsFactors = FALSE)[,-1]
jqname<-unique(jqdata$Name)

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
