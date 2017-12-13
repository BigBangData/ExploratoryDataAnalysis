## Plot 1

# 0. unzip folder   ## only in plot 1 
unzip("./exdata_data_NEI_data.zip") 

# 1. read in data 
NEI <- readRDS("summarySCC_PM25.rds")

# 2. subset by year 
pm1 <- NEI[NEI$year == "1999", ]
pm2 <- NEI[NEI$year == "2002", ]
pm3 <- NEI[NEI$year == "2005", ]
pm4 <- NEI[NEI$year == "2008", ] 

# 3. calculate totals per year in millions of tons
w <- sum(pm1$Emissions)/1e+06
x <- sum(pm2$Emissions)/1e+06
y <- sum(pm3$Emissions)/1e+06
z <- sum(pm4$Emissions)/1e+06

# 4. plot trend using calculated totals
plot(c(1999,2002,2005,2008), c(w,x,y,z), type="l", col = "firebrick", lwd = 1, ylim = range(0, 8),
	 main = "Total U.S." ~PM[2.5]~ "Emissions", xlab = "Year", ylab = ~PM[2.5]~ "Emissions (millions of tons)")

# 5. save plot to a PNG file in working directory
dev.copy(png, file = "plot1.png", width = 600, height = 480) 
dev.off()

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 