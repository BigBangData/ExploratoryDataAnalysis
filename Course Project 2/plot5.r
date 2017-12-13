## Plot 5

# 1. read in data 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# 2. subset Baltimore City data in NEI, choosing SCC, Emissions, type, and year variables
BCity <- NEI[NEI$fips == "24510", ][,c(2, 4:6)]

# 3. subset SCC on variables of interest (as per EDA in plot 4); merge with Baltimore City subset 
SCC <- SCC[, c(1, 3, 4, 7:10)]
mergedData <- merge(BCity, SCC, by = "SCC")

## EDA: defined 'motor vehicle' as highway vehicles; excluding marine vessels, pleasure creafts, and railroad equipment
## EDA: 'Short.Name' variable is sufficient for subsetting all highway motor vehicle observations

# 4. subset motor vehicle observations, remove levels variables with redundant info 
Hwy <- which(grepl("Highway Veh", mergedData$Short.Name) == TRUE)  
MotorVehicles <- mergedData[Hwy, -c(7:10)]

## EDA: noticed the "totals" observations, verified that including them would not double-count emissions numbers  

# 5. change 'EI.Sector' values into more descriptive and shorter names
MotorVehicles$EI.Sector <- gsub("Mobile - On-Road Gasoline", "Gasoline -"  , MotorVehicles$EI.Sector)
MotorVehicles$EI.Sector <- gsub("Mobile - On-Road Diesel", "Diesel -", MotorVehicles$EI.Sector)
MotorVehicles$EI.Sector <- gsub(" Vehicles", "", MotorVehicles$EI.Sector)

## EDA: Urban and Rural definitions in Short.Name - decided to create factor variable with two levels: Rural, Urban

# 6. organize data by rural and urban observations
Rural <- which(grepl("Rural", MotorVehicles$Short.Name) == TRUE)
Urban <- which(grepl("Urban", MotorVehicles$Short.Name) == TRUE)
MotorVehicles <- MotorVehicles[c(Rural,Urban), ]
MotorVehicles$Rural.Urban <- factor(c(rep("Rural", 279), rep("Urban", 840)))

## EDA: after plotting, results show that rural observations are negligible 

# 7. remove rural observations and subset by Emissions, year, and EI.Sector
MotorVehicles <- MotorVehicles[c(280:1119), c(2,4,6)]

# 8. tidy EI.Sector variable by splitting into two: one for Class (light/heavy duty) and other for Fuel type
library(dplyr)
library(tidyr)
MotorVehicles <- MotorVehicles %>%
	separate(EI.Sector, c("Fuel", "Class"), " - ")

# 9. group by year, fuel, and class; sum emissions for each group
final_data <- MotorVehicles %>%
	group_by(year, Fuel, Class) %>%
	summarize(count = sum(Emissions)) %>%  
    na.omit() 
	
options('tibble.print_max' = 50) 
final_data

# 10. plot data by fuel type and class 
library(ggplot2)
ggplot(final_data, aes(year, count)) +
	geom_line(aes(color = Class), size = 1) + 
	facet_grid(. ~ Fuel) + 
	labs(x="Year", y=~PM[2.5]~ "Emissions (tons)", 
	title=~PM[2.5]~ "Emissions from Motor Vehicles in Baltimore City, by Class and Fuel Type")
	
# 11. save PNG file to working directory
dev.copy(png, file = "plot5.png", width = 740, height = 480)
dev.off()