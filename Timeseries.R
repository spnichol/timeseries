#Homework 6 - working with timeseries data 
library("lubridate")

setwd("C:\\Users\\Steven\\Google Drive\\1. MOT\\1) Fall 2016\\1. BA\\Assignments\\Homework 6")
#read in S&P 500 data 
sp <- read.csv("sp.csv", stringsAsFactors=FALSE, header=TRUE)

sp$Date <- as.Date(sp$Date, "%d-%B-%y")
#sort by date, starting with the oldest dates
sp <- sp[order(as.Date(sp$Date, format="%Y/%m/%d", decreasing=FALSE)),]

#sort in decreasing fashion using reverse function (decreasing=TRUE does not work for dates)


sp_descending <- sp[rev(order(as.Date(sp$Date))),]

#read in gold 
gold <- read.csv("gold2016.csv", stringsAsFactors=FALSE, header=TRUE)
gold$Date <- as.Date(gold$Date, "%d-%B-%y")
gold <- gold[order(as.Date(gold$Date, format="%y/%m/%d")),]
gold

#create monthly intervals by just third day 
gold['day']<- strftime(gold$Date, "%d")

#create snapshot monthly price of Gold on third of month 
gold_month <- subset(gold, gold$day == "03")
gold_month['gold_month_price'] <- gold_month$Price
gold_month <- subset(gold_month, select=c("gold_month_price"))

#calculate average monthly price of Gold 
gold['day_year'] <- strftime(gold$Date, "%m/%y")
gold['avg_price'] <- 0
for (i in 1:nrow(gold)) {
  submonth <- subset(gold, day_year == day_year[i])
    gold$avg_price[i] <- (sum(submonth$Price))/length(submonth$Price)
  }
  
 
head(gold)
  
#read in oil 
oil <- read.csv("oil2016.csv", stringsAsFactors=FALSE, header=TRUE)
oil$Date <- as.Date(oil$Date, "%d-%B-%y")
oil <- oil[order(as.Date(oil$Date, format="%y/%m/%d")),]

#merge

oil['oil'] <- oil$Price 
sp['sp'] <- sp$Price
gold['gold'] <- gold$Price
gold['gold_avg'] <-gold$avg_price

timeseries <- merge(oil, sp, by="Date")
timeseries <- merge(timeseries, gold, by="Date")
timeseries <- subset(timeseries, select=c("Date", "oil", "sp", "gold", "gold_avg"))

#unique TS for monthly gold 
timeseries_month <- unique(timeseries$gold_avg, incomparables=FALSE)

#create time series
timeseries_all <- subset(timeseries, select=c("oil", "sp", "gold"))
ts.all <- ts(data=timeseries_all)
ts.oil<-ts(timeseries_all$oil) 
ts.gold <- ts(timeseries_all$gold)
ts.gold_avg <- ts(timeseries_month)
ts.sp <- ts(timeseries_all$sp)



#set frequency 
ts.gold_avg<-ts(timeseries_month, frequency= 12) 
ts.gold_avg

#check out in plots 
par(mfrow=c(4, 1))
plot(ts.all)
plot(ts.gold_avg)


#decompose 
ts.gold_avg.d <- decompose(ts.gold_avg)
plot(ts.gold_avg.d)

ts.gold.1.d <- decompose(ts.gold.1)
plot(ts.gold.1.d)
