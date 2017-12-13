## Plot 6

# 1. read in data 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# 2. subset Baltimore City and Los Angeles data from NEI, choosing variables: fips, SCC, Emissions, type, and year
BCLA <- NEI[NEI$fips == "24510" | NEI$fips == "06037", ][, c(1:2, 4:6)]

# 3. subset SCC on variables of interest (as per EDA in plot 5); merge with previous NEI subset
SCC <- SCC[, c(1, 3:4)]
mergedData <- merge(BCLA, SCC, by = "SCC")

# 4. subset motor vehicle observations as in plot 5
Hwy <- which(grepl("Highway Veh", mergedData$Short.Name) == TRUE)  
MotorVehicles <- mergedData[Hwy, ]

## EDA: noticed two "POINT" observations for variable type, probably typos 

# 5. change 'EI.Sector' values into more descriptive and shorter names
MotorVehicles$EI.Sector <- gsub("Mobile - On-Road Gasoline", "Gasoline -"  , MotorVehicles$EI.Sector)
MotorVehicles$EI.Sector <- gsub("Mobile - On-Road Diesel", "Diesel -", MotorVehicles$EI.Sector)
MotorVehicles$EI.Sector <- gsub(" Vehicles", "", MotorVehicles$EI.Sector)

# 6. organize data by rural and urban observations for each city, create factor variable "Area" with four levels
BCRural <- which(MotorVehicles$fips == "24510" & grepl("Rural", MotorVehicles$Short.Name) == TRUE)
BCUrban <- which(MotorVehicles$fips == "24510" & grepl("Urban", MotorVehicles$Short.Name) == TRUE)
LARural <- which(MotorVehicles$fips == "06037" & grepl("Rural", MotorVehicles$Short.Name) == TRUE)
LAUrban <- which(MotorVehicles$fips == "06037" & grepl("Urban", MotorVehicles$Short.Name) == TRUE)
MotorVehicles <- MotorVehicles[c(BCRural, BCUrban, LARural, LAUrban), ]
MotorVehicles$Area <- factor(c(rep("BC Rural", 279), rep("BC Urban", 840), rep("LA Rural", 499), rep("LA Urban", 480)))

# 7. remove unnecessary variables: SCC, fips, type and Short.Name 
MotorVehicles <- MotorVehicles[ , -c(1:2, 4, 6)]

# 8. tidy EI.Sector variable by splitting into two: one for Class (light/heavy duty) and other for Fuel type
library(dplyr)
library(tidyr)
MotorVehicles <- MotorVehicles %>%
	separate(EI.Sector, c("Fuel", "Class"), " - ")

# 9. group by area, year, fuel, and class; sum emissions for each group
final_data <- MotorVehicles %>%
	group_by(Area, year, Fuel, Class) %>%
	summarize(count = sum(Emissions)) %>%  
    na.omit() 
options('tibble.print_max' = 100) 
final_data

## EDA: noticed there is no Los Angeles Light Duty Urban Emissions numbers for 2008
## went back to previous tables (MotorVehicles, NEI) and checked that this was indeed the case

# 10. add a City variable to clarify plot (will serve as background color for panels)
final_data$City <- c(rep("Baltimore City", 28), rep("Los Angeles", 30))

# 11. plot data by area, fuel type and class 
library(ggplot2)
ggplot(final_data, aes(year, count)) +
	geom_line(aes(color = Class), size = 1) + 
	geom_rect(aes(fill = City),xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf,alpha = 0.01, show.legend = FALSE) +
	facet_grid(Fuel ~ Area) + 
	labs(x="Year", y=~PM[2.5]~ "Emissions (tons)", 
	title=~PM[2.5]~ "Emissions from Motor Vehicles in Baltimore City and Los Angeles Rural and Urban Areas, by Class and Fuel Type")
	
# 12. save PNG file to working directory
dev.copy(png, file = "plot6.png", width = 840, height = 480)
dev.off()