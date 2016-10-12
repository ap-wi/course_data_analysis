## --------------------------------------------------
## R Coding 
## Plot 1 Histogramm
## File "exdata_data_household_power_consumption.zip"
## 10.10.2016 A.Paul
## --------------------------------------------------

## path
setwd("C:/Users/Paul/Desktop/coursera")

## file import
rawdata <- read.table("household_power_consumption.txt",header=TRUE,sep=";",dec=".")

helpdate <- substring(rawdata$Date,1,10)

## subdata
mydata <- subset(rawdata, helpdate=="1/2/2007" | helpdate=="2/2/2007")

## graphic device
png(filename = "plot1.png", width = 480, height = 480)

## histogramm at screen
hist(as.numeric(mydata$Global_active_power)/1000, 
     main="Global Active Power",
     xlim=c(0,6),
     ylim=NULL,
     xlab="Global Active Power (kilowatts)",
     col="red")

## device back to screen
dev.off()

## Histogramm at screen
hist(as.numeric(mydata$Global_active_power)/1000, 
     main="Global Active Power",
     xlim=c(0,6),
     ylim=NULL, 
     xlab="Global Active Power (kilowatts)",
     col="red")
