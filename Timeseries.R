#Homework 6 - working with timeseries data 
library("lubridate")

setwd("C:\\Users\\Steven\\Google Drive\\1. MOT\\1) Fall 2016\\1. BA\\Assignments\\Homework 6")
sp <- read.csv("sp.csv", stringsAsFactors=FALSE, header=TRUE)

sp$Date <- as.Date(sp$Date, "%d-%B-%y")
#sort by date, starting with the oldest dates
sp <- sp[order(as.Date(sp$Date, format="%Y/%m/%d", decreasing=FALSE)),]

#sort in decreasing fashion using reverse function (decreasing=TRUE does not work for dates)


sp_descending <- sp[rev(order(as.Date(sp$Date))),]


gold <- read.csv("gold2016.csv", stringsAsFactors=FALSE, header=TRUE)
gold$Date <- as.Date(gold$Date, "%d-%B-%y")
gold <- gold[order(as.Date(gold$Date, format="%y/%m/%d")),]
gold

#create monthly intervals by just third day 
gold['day']<- strftime(gold$Date, "%d")
gold['day_year'] <- strftime(gold$Date, "%m/%y")
head(gold)
gold_month <- subset(gold, gold$day == "03")
gold_month['gold_month_price'] <- gold_month$Price
gold_month <- subset(gold_month, select=c("gold_month_price"))
gold_month
gold['avg_price'] <- 0
for (i in 1:nrow(gold)) {
  submonth <- subset(gold, day_year == day_year[i])
    gold$avg_price[i] <- sum(submonth$Price) 
  }
  
 
gold
  
test <- subset(gold, day_year == "12/10")
sum(test$Price)

test2 <- subset(gold, day_year == "02/10")
sum(test2$Price)
oil <- read.csv("oil2016.csv", stringsAsFactors=FALSE, header=TRUE)
oil$Date <- as.Date(oil$Date, "%d-%B-%y")
oil <- oil[order(as.Date(oil$Date, format="%y/%m/%d")),]

#merge
help(date)

oil['oil'] <- oil$Price 
sp['sp'] <- sp$Price
gold['gold'] <- gold$Price
timeseries <- merge(oil, sp, by="Date")
timeseries <- merge(timeseries, gold, by="Date")
timeseries <- subset(timeseries, select=c("Date", "oil", "sp", "gold"))
timeseries_all <- merge(timeseries, gold_month, by="Date", all.x=TRUE)
timeseries_gold <- ts()
summary(timeseries_all$gold_month_price.x)
timeseries
#create time series
timeseries <- subset(timeseries_all, select=c("oil", "sp", "gold", "gold_month_price.x"))
ts.all <- ts(data=timeseries)
ts.oil<-ts(timeseries$oil) 
ts.gold <- ts(timeseries$gold)
ts.sp <- ts(timeseries$sp)
ts.gold.month <- ts(timeseries$gold_month_price)


#set frequency 
ts.gold.month<-ts(timeseries$gold_month_price) 
ts.gold.month
help(ts)
plot(ts.all)
plot(ts.gold.month1)

#decompose 
ts.gold.1<-ts(timeseries$gold_month_price)
plot.ts(ts.gold.1)

ts.gold.1.d <- decompose(ts.gold.1)
plot(ts.gold.1.d)
