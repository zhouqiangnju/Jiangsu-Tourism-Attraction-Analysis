
monthreport<-function(month){
  
  library(dplyr)
  library(readxl)

  #自动生成日期
  year<-gsub("\\D","",month) %>% substr(1,4)
  yue<-gsub("\\D","",month) %>% substr(5,6)%>% as.integer()
  if(yue>12) yue<-floor(yue/10)
  
  #read data from excel files 
  monthdata<-read_xls(month,sheet="4A1",range="A3:B3")

  sheet<-excel_sheets(month)
  sheet4A<-grep("4A",sheet)
  
  for(j in 1:length(sheet4A)){
    newdata<-read_xls(month,sheet=sheet4A[j],range=cell_cols("A:B"),col_names=FALSE)
    newdata<-newdata[-c(1:4),]
    monthdata<-rbind(monthdata,newdata)
  }
  #clean and restore data
  names(monthdata)<-c("Name","Visitor")
  monthdata$Name<-sub("合//s*计","全省",monthdata$Name)
  monthdata<-monthdata[!is.na(monthdata$Name),]
  monthdata<-monthdata[-1,]
  
  #Rate
  monthdata$Rate<-"4A"
  monthdata$Rate[grep("5A",monthdata$Name)]<-"5A"
  monthdata$Name<-sub(".5A.$","",monthdata$Name)
  #City
  monthdata$City<-substr(monthdata$Name,1,2)
  monthdata$City<-sub("侵华","南京",monthdata$City)
  monthdata$City<-sub("宜兴|江阴","无锡",monthdata$City)
  monthdata$City<-sub("新沂|邳州|沛县|睢宁","徐州",monthdata$City)
  monthdata$City<-sub("溧阳","常州",monthdata$City)
  monthdata$City<-sub("吴江|太仓|张家|常熟|昆山|虞山","苏州",monthdata$City)
  monthdata$City<-sub("如皋|海门|海安","南通",monthdata$City)
  monthdata$City<-sub("连云|东海|灌云|灌南","连云港",monthdata$City)
  monthdata$City<-sub("盱眙|涟水|新四|金湖","淮安",monthdata$City)
  monthdata$City<-sub("大丰|阜宁|射阳|东台","盐城",monthdata$City)
  monthdata$City<-sub("高邮","扬州",monthdata$City)
  monthdata$City<-sub("姜堰|兴化","泰州",monthdata$City)
  monthdata$City<-sub("泗洪|泗阳","宿迁",monthdata$City)
  monthdata$City[grep("江苏大阳山国家森林公园",monthdata$Name)]<-"苏州"
  monthdata$City[grep("江苏东台黄海森林公园景区",monthdata$Name)]<-"盐城"
  monthdata$City[grep("江苏茶博园",monthdata$Name)]<-"镇江"
  monthdata$City[grep("中国泗阳杨树博物馆",monthdata$Name)]<-"宿迁"
  monthdata$City[grep("中国东海水晶博物馆",monthdata$Name)]<-"连云港"
  monthdata$chengshi<-'JS'
  monthdata$chengshi[grep("南京",monthdata$City)]<-"NJ"
  monthdata$chengshi[grep("无锡",monthdata$City)]<-"WX"
  monthdata$chengshi[grep("徐州",monthdata$City)]<-"XZ"
  monthdata$chengshi[grep("常州",monthdata$City)]<-"CZ"
  monthdata$chengshi[grep("苏州",monthdata$City)]<-"SZ"
  monthdata$chengshi[grep("南通",monthdata$City)]<-"NT"
  monthdata$chengshi[grep("淮安",monthdata$City)]<-"HA"
  monthdata$chengshi[grep("连云港",monthdata$City)]<-"LYG"
  monthdata$chengshi[grep("盐城",monthdata$City)]<-"YC"
  monthdata$chengshi[grep("扬州",monthdata$City)]<-"YZ"
  monthdata$chengshi[grep("镇江",monthdata$City)]<-"ZJ"
  monthdata$chengshi[grep("泰州",monthdata$City)]<-"TZ"
  monthdata$chengshi[grep("宿迁",monthdata$City)]<-"SQ"
  #Month

  monthdata$Year<-year
  monthdata$Month<-yue
  

  #景区名称归一化
  monthdata$Name[grep("中山陵",monthdata$Name)]<-"南京中山陵园风景区"
  monthdata$Name[grep("团氿",monthdata$Name)]<-"宜兴团氿景区"
  monthdata$Name[grep("恐龙",monthdata$Name)]<-"常州环球恐龙城"
  monthdata$Name[grep("西津",monthdata$Name)]<-"镇江西津渡历史文化街区"
  monthdata$Name[grep("瘦西湖",monthdata$Name)]<-"扬州瘦西湖风景区"
  monthdata$Name[grep("东关",monthdata$Name)]<-"扬州东关历史文化旅游区"
  monthdata$Name[grep("麋鹿",monthdata$Name)]<-"大丰中华麋鹿园景区"
  monthdata$Name[grep("龙背山",monthdata$Name)]<-"宜兴龙背山森林公园"
  monthdata$Name[grep("新四军纪念馆",monthdata$Name)]<-"盐城新四军纪念馆"
  monthdata$Name[grep("云龙湖",monthdata$Name)]<-"徐州云龙湖景区"
  monthdata$Name[grep("海盐",monthdata$Name)]<-"盐城海盐历史文化风景区"
  monthdata$Name[grep("第一山",monthdata$Name)]<-"盱眙第一山景区"
  monthdata$Name[grep("洪泽湖湿地",monthdata$Name)]<-"泗洪洪泽湖湿地公园"
  monthdata$Name[grep("湖滨公园",monthdata$Name)]<-"宿迁湖滨公园景区"
  monthdata$Name[grep("鸿山泰伯",monthdata$Name)]<-"无锡鸿山泰伯景区"
  monthdata$Name[grep("天德湖",monthdata$Name)]<-"泰州天德湖公园"
  monthdata$Name[grep("溱湖",monthdata$Name)]<-"姜堰溱湖旅游区"
  monthdata$Name[grep("天宁",monthdata$Name)]<-"常州天宁寺"
  monthdata$Name[grep("周庄",monthdata$Name)]<-"苏州周庄古镇游览区"
  monthdata$Name[grep("同里",monthdata$Name)]<-"苏州同里古镇游览区"
  monthdata$Name[grep("无锡博物",monthdata$Name)]<-"无锡博物院"
  monthdata$Name[grep("苏州园林",monthdata$Name)]<-"苏州园林(拙政园、虎丘、留园）"
  monthdata$Name[grep("穹窿山",monthdata$Name)]<-"苏州穹窿山风景区"
  monthdata$Name[grep("千灯",monthdata$Name)]<-"昆山千灯古镇游览区"
  monthdata$Name[grep("杨树",monthdata$Name)]<-"泗阳杨树博物馆"
  monthdata$Name[grep("凤城河",monthdata$Name)]<-"泰州凤城河风景区"
  monthdata$Name[grep("濠河",monthdata$Name)]<-"南通濠河旅游区"
  monthdata$Name[grep("狼山",monthdata$Name)]<-"南通狼山旅游区"
  monthdata$Name[grep("苏州虎丘",monthdata$Name)]<-"苏州虎丘风景名胜区"
  monthdata$Name[grep("天目湖旅游",monthdata$Name)]<-"常州天目湖旅游区"
  monthdata$Name[grep("吴承恩",monthdata$Name)]<-"淮安吴承恩故居景区"
  monthdata$Name[grep("国际水晶",monthdata$Name)]<-"连云港市东海县国际水晶珠宝城"
  monthdata$Name[grep("窑湾",monthdata$Name)]<-"新沂窑湾古镇景区"
  monthdata$Name[grep("雪枫",monthdata$Name)]<-"宿迁雪枫公园"
  monthdata$Name[grep("学政",monthdata$Name)]<-"江阴江苏学政文化旅游区"
  monthdata$Name[grep("淹城",monthdata$Name)]<-"常州中国春秋淹城旅游区"
  monthdata$Name[grep("滨江要塞",monthdata$Name)]<-"江阴滨江要塞旅游区"
  monthdata$Name[grep("雨花台",monthdata$Name)]<-"南京雨花台景区"
  monthdata$Name[grep("栖霞山",monthdata$Name)]<-"南京栖霞山风景名胜区"
  monthdata$Name[grep("水绘园",monthdata$Name)]<-"如皋水绘园景区"
  monthdata$Name<-sub("苏州常熟","常熟",monthdata$Name)
  monthdata$Name[grep("鼋头渚",monthdata$Name)]<-"无锡太湖鼋头渚风景区"
  monthdata$Name[grep("三国水浒",monthdata$Name)]<-"无锡三国水浒景区"
  monthdata$Name[grep("淮海战役",monthdata$Name)]<-"徐州淮海战役烈士纪念塔园林"
  monthdata$Name[grep("宜兴市竹海景区",monthdata$Name)]<-"宜兴竹海景区"
  monthdata$Name[grep("镇江市焦山风景区",monthdata$Name)]<-"镇江焦山风景区"
  monthdata$Name[grep("镇江市北固山风景区",monthdata$Name)]<-"镇江北固山风景区"
  monthdata$Name[grep("道天下",monthdata$Name)]<-"常州东方盐湖城·道天下景区"
  
  
  #output
  filepath<-paste('F:/Administrator/Documents/R/Jiangsu Tourist Attractions/Data/',year,'年',yue,'月',"(processed).csv",sep="")
  write.csv(monthdata,filepath)
  return(filepath)
}

#merge
library('dplyr')
newdata<-monthreport("F:/Administrator/Documents/R/Jiangsu Tourist Attractions/RAW/2018年2月5A、4A景区接待情况.xls")%>%read.csv(stringsAsFactors = FALSE)
jqdata<-read.csv("F:/Administrator/Documents/R/Jiangsu Tourist Attractions/Data/Jiangsu_Jingqu_Vistordata_in_Time-series(2018-03-06).csv",stringsAsFactors = FALSE)[,-1]

jqdatanew<-rbind(jqdata,newdata)
write.csv(jqdatanew,paste("F:/Administrator/Documents/R/Jiangsu Tourist Attractions/Data/Jiangsu_Jingqu_Vistordata_in_Time-series(",Sys.Date(),').csv',sep = ''))

