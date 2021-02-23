library(arules)
#install.packages("tokenizers")
library(tokenizers)
library(tidyverse)
#library(plyr)
library(dplyr)
library(ggplot2)
#install.packages("syuzhet")  ## sentiment analysis
library(syuzhet)
library(stringr)
#install.packages("arulesViz")
library(arulesViz)
library(stopwords)
library(lubridate)
library(OneR)
library(sqldf)
library(ggthemes)
library(data.table)

setwd("~/Projects/ist-707")
set.seed(1345)

stockData <- read.csv("stock-data/bar.csv")

stockData$date<-as.Date(stockData$time)
stockData$time<-parse_date_time(stockData$time,"%Y-%m-%d %T%z")
stockData$symbol<-as.factor(stockData$symbol)

#boxplot(stockData[stockData$volume<100000,]$volume)

#groupedSymbol = sqldf("select date,count(time) as count, symbol from stockData group by date,symbol" )

#ggplot(groupedSymbol,aes(x=date,y=count,group=symbol,color=symbol))+ geom_point()

#volume_split<-bin(stockData$volume, nbins=3, method="clusters")
#str(volume_split)

stockData %>%
  group_by(date) %>%
  summarise(total = sum(volume)) %>%
  ggplot(aes(x=date,y=total))+ geom_line()


#How many stickers per day we have , 
stockData %>%
  group_by(date) %>%
  summarise(total=n_distinct(symbol)) %>%
  ggplot(aes(x=date,y=total))+ geom_line() + geom_point(size=5)



#filter data u til aug 14, since after it we are missing data.
stockData<- stockData[stockData$date<date("2020-08-15"),]

bySymbol<-stockData %>%
  group_by(date,symbol) %>%
  summarise(total=n()) #%>%
  #ggplot(aes(x=symbol))+ geom_boxplot()



stockDataDaily<- sqldf('select date,symbol,sum(volume) as volume, 
                        min(open_price) as open_price_min, max(open_price) as open_price_max,
                        min(close_price) as close_price_min, max(close_price) as close_price_max,
                        min(low_price) as low_price, max(high_price) as high_price , avg(average_price) as average_price from stockData 
                        group by date,symbol')

summary(stockDataDaily)

stickerData <- read.csv("stock-data/constituents.csv")

stockDataDaily<-merge(stockDataDaily,stickerData,by.x="symbol",by.y="Symbol")

stockDataDaily$Sector<-as.factor(stockDataDaily$Sector)



targetData <- read.csv("stock-data/target.csv")
targetData$updatedDate<-as.Date(targetData$updatedDate)
unique(targetData$symbol)
targetData<-setDT(targetData)[,.SD[which.max(updatedDate)],keyby=symbol]


stockDataDaily<-merge(stockDataDaily,targetData,by.x="symbol",by.y="symbol")
stockDataDaily<- stockDataDaily %>% rename(tragetUpdatedDate=updatedDate )



ratingData <- read.csv("stock-data/rating.csv")
ratingData$consensusStartDate<-as.Date(ratingData$consensusStartDate)
ratingData$consensusEndDate<-as.Date(ratingData$consensusEndDate)
unique(targetData$symbol)

ratingData<-setDT(ratingData)[,.SD[which.max(consensusStartDate)],keyby=symbol]

stockDataDaily<-merge(stockDataDaily,ratingData,by.x="symbol",by.y="symbol")

summary<- sqldf("select symbol, count(*) as count from stockDataDaily group by symbol")

write.csv(stockDataDaily, "stock_data_daily_merged.csv")

#volume_split<-bin(stockData$volume, nbins=3, method="clusters")
#print(volume_split)

stockData$priceChange=(stockData$close_price-stockData$open_price)/stockData$open_price
stockData$priceChangeBin<-bin(stockData$priceChange, nbins=8, method="clusters")
unique(stockData$priceChangeBin)
ggplot(stockData,aes(x=priceChangeBin,fill=priceChangeBin)) + geom_bar() + theme_few() + xlab("Change in Price") + theme_void

#grouped = sqldf("select date,priceChangeBin, group_concat(Name) as grouped from stockData where priceChangeBin != '(-0.000287,0.000209]' group by time,priceChangeBin" )
grouped = sqldf("select date,priceChangeBin, group_concat(Name) as grouped from stockData where priceChangeBin == '(-0.000287,0.000209]' group by time,priceChangeBin" )
withCluster<-paste(grouped$grouped,",",str_replace(grouped$priceChangeBin,",","-"))
withCluster[1]
items <- strsplit(as.character(withCluster), ",")
items[1]
trans <- as(items, "transactions")


arules.model<-apriori(trans,parameter = list(support=.03, confidence=.5, minlen=2))
summary(arules.model)
plot(arules.model,method="grouped")

by_support<-sort(arules.model, by="support", decreasing=TRUE)
plot(by_support[1:20],method="grouped")

by_lift<-sort(arules.model, by="lift", decreasing=TRUE)
plot(by_lift[1:20],method="grouped")

rules_highlift<-subset(arules.model, subset = lift > 6.2)
summary(rules_highlift)
plot(rules_highlift,method="graph", engine='interactive', max=300)



