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



#create training and test data for to gague forecast accuracy 
gold_train <- subset(gold, Date > as.Date("2016-01-01") )
gold_test <- subset(gold, Date < as.Date("2016-01-01"))


#read in oil 
oil <- read.csv("oil2016.csv", stringsAsFactors=FALSE, header=TRUE)
oil$Date <- as.Date(oil$Date, "%d-%B-%y")
oil <- oil[order(as.Date(oil$Date, format="%y/%m/%d")),]

#oil monthly 
oil['day_year'] <- strftime(oil$Date, "%m/%y")
oil['avg_price'] <- 0
for (i in 1:nrow(oil)) {
  submonth <- subset(oil, day_year == day_year[i])
  oil$avg_price[i] <- (sum(submonth$Price))/length(submonth$Price)
}

#create test/train data for oil 
oil_train <- subset(oil, Date > as.Date("2016-01-01") )
oil_test <- subset(oil, Date < as.Date("2016-01-01"))
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
#same for oil 
timeseries_month_oil <- unique(oil$avg_price, incomparables=FALSE)

#create time series
timeseries_all <- subset(timeseries, select=c("oil", "sp", "gold"))
ts.all <- ts(data=timeseries_all)
ts.oil<-ts(timeseries_all$oil, start=c(2010))
ts.oil_avg <- ts(timeseries_month_oil, start=c(2010), frequency=12)

ts.gold <- ts(timeseries_all$gold)
ts.gold_avg <- ts(timeseries_month, start=c(2010), frequency=12)
ts.sp <- ts(timeseries_all$sp)

#combine oil and gold monthly 
ts.combo <- cbind(ts.oil_avg, ts.gold_avg)
plot.ts(ts.combo)
ts.gold_avg

#check out in plots 
par(mfrow=c(4, 1))
plot(ts.all)
plot(ts.gold_avg)


#decompose 
ts.gold_avg.d <- decompose(ts.gold_avg)
plot(ts.gold_avg.d)

ts.oil_avg.d <- decompose(ts.oil_avg)

plot(ts.oil_avg.d)

plot(ts.oil)
ts.gold.1.d <- decompose(ts.gold.1)
plot(ts.gold.1.d)

#begin forecast 
library(forecast)
gold.holt <- HoltWinters(ts.gold_avg, gamma=FALSE)
gold.holt
plot(gold.holt)

oil.holt <- HoltWinters(ts.oil_avg, gamma=FALSE)
oil.holt
plot(oil.holt)


#check if data is stationary 

require(tseries)
adf.test(diff(diff(log(ts.gold_avg))))
adf.test(diff(diff(log(ts.oil_avg))))
adf.test(diff(diff(log(ts.oil))))
