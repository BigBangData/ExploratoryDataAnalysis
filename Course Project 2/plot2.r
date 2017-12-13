## Plot 2

# 1. read in data 
NEI <- readRDS("summarySCC_PM25.rds")

# 2. subset Baltimore City data on emissions and year only 
BCity <- NEI[NEI$fips == "24510", ][,c(4,6)]

# 3. split into a list of data frames for each year 
BCity <- split(BCity, as.factor(BCity$year))

# 4. calculate total emissions for each data frame
totals <- vector("numeric", 4)
for(i in 1:4) totals[i] <- sum(BCity[[i]][,1])

# 5. plot points of sums, segments to connect
plot(c(1999,2002,2005,2008), totals, pch=24, type = "b", col =  "steelblue", lwd = 1, ylim = range(0,4000),
	 main = "Baltimore City - Total" ~PM[2.5]~ "Emissions", xlab = "Year", ylab = ~PM[2.5]~ "Emissions (tons)")

# save plot to a PNG file in working directory
dev.copy(png, file = "plot2.png", width = 600, height = 480) 
dev.off()

