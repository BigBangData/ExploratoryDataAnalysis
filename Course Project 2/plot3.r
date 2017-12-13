## Plot 3

# 1. read in data 
NEI <- readRDS("summarySCC_PM25.rds")

# 2. subset Baltimore City data on emissions, type and year only 
BCity <- NEI[NEI$fips == "24510", ][,c(4:6)]

# 3. group by year and type; sum emissions for each category
library(dplyr)
BCity <- BCity %>%
	group_by(year, type) %>%
	summarize(count = sum(Emissions))

# 4. plot by type and year 
library(ggplot2)
ggplot(BCity, aes(year, count)) +
	geom_line(aes(color = count), size=1) + 
	facet_wrap(~type) +
	labs(x="Year", y=~PM[2.5]~ "Emissions (tons)", title="Baltimore City" ~PM[2.5]~ "Emissions by Type")

# 5. save plot to a PNG file in working directory
dev.copy(png, file = "plot3.png", width = 740, height = 480)
dev.off()