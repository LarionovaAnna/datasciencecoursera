# Loads RDS
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Computes sums of emissions by every year
aggregatedTotalByYear <- aggregate(Emissions ~ year, NEI, sum)

# Create plot
png('plot1.png')
barplot(height = aggregatedTotalByYear$Emissions, 
        names.arg = aggregatedTotalByYear$year, 
        xlab="years", ylab=expression('total PM'[2.5]*' emission'),
        main=expression('Total PM'[2.5]*' emissions at various years'))
dev.off()