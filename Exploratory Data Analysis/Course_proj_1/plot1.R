##get data
data <- read.csv("household_power_consumption.txt", header = TRUE, sep=';', 
                 na.strings="?", nrows=2075259)
data$Date <- as.Date(data$Date, format="%d/%m/%Y")

##subset data
sub <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]
globalActivePower <- as.numeric(sub$Global_active_power)

##plot1
png("plot1.png", width=480, height=480)
hist(globalActivePower, col="red", 
     main="Global Active Power", xlab="Global Active Power (kilowatts)")
dev.off()