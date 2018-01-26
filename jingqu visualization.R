library("httr") 
library("magrittr") 
library("jsonlite")
library("leafletCN")
library("leaflet")
library('reshape2')
library("dplyr")
library('plyr')
library('sf')
library('Rgctc2')
library(maptools)
library(rgdal)

homefile<-'C:/Users/zhouq/Documents/GitHub/Jiangsu-Tourism-Attraction-Analysis'
workfile<-"F:/Administrator/Documents/GitHub/Jiangsu-Tourism-Attraction-Analysis"
cityorder<-c('NJ','WX','XZ','CZ','SZ','NT','LYG','HA','YC','YZ','ZJ','TZ','SQ')
cityorder<-factor(cityorder,levels = cityorder)
setwd(workfile)

jqgeo<-read.csv('jqgeo.csv',stringsAsFactors = FALSE)[,-1]
