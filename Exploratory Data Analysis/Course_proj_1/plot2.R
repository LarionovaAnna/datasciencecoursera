##get data
data <- read.csv("household_power_consumption.txt", header = TRUE, sep=';', 
                 na.strings="?", nrows=2075259)
data$Date <- as.Date(data$Date, format="%d/%m/%Y")

##subset data
sub <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]
globalActivePower <- as.numeric(sub$Global_active_power)
datetime <- strptime(paste(sub$Date, sub$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 

##plot2
png("plot2.png", width=480, height=480)
plot(datetime, globalActivePower, type="l", 
     xlab="", ylab="Global Active Power (kilowatts)")
dev.off()