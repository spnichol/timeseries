#Homeowrk 6 - working with timeseries data 

sp <- read.csv("table.csv", stringsAsFactors=FALSE, header=TRUE)


#sort by date, starting with the oldest dates
sp_sorted <- sp[order(as.Date(sp$Date, format="%Y/%m/%d", decreasing=FALSE)),]

#sort in decreasing fashion using reverse function (decreasing=TRUE does not work for dates)

sp_descending <- sp[rev(order(as.Date(sp$Date))),]
