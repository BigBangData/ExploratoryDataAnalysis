## Plot 4
# NOTE: much of the EDA was not included in this plot, brief explanations given

# 1. read in data 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## EDA revealed 5385 SCC codes in NEI and 11717 SCC codes in SCC
## Decision: merge data sets to reduce number of codes right away

## Studied SCC at: https://ofmpub.epa.gov/sccsearch/docs/SCC-IntroToSCCs.pdf
## Variables of interest: SCC, Short.Name, EI.Sector, and description levels one through four

# 2. subset SCC with variables of interest; merge NEI and SCC by matching SCC codes 
SCC <- SCC[, c(1, 3, 4, 7:10)]
mergedData <- merge(NEI, SCC, by = "SCC")

# 3. create vectors of indices for coal sources, one for each variable of interest 
coal_index1 <- which(grepl("[Cc][Oo][Aa][Ll]", mergedData$Short.Name) == TRUE)   
coal_index2 <- which(grepl("[Cc][Oo][Aa][Ll]", mergedData$EI.Sector) == TRUE)   
coal_index3 <- which(grepl("[Cc][Oo][Aa][Ll]", mergedData$SCC.Level.One) == TRUE) # zero results  
coal_index4 <- which(grepl("[Cc][Oo][Aa][Ll]", mergedData$SCC.Level.Two) == TRUE)   # zero results
coal_index5 <- which(grepl("[Cc][Oo][Aa][Ll]", mergedData$SCC.Level.Three) == TRUE)    
coal_index6 <- which(grepl("[Cc][Oo][Aa][Ll]", mergedData$SCC.Level.Four) == TRUE)    

# 4. create a master vector of unique SCC coal-containing codes; subset of the data using this vector 
master_coal_index <- unique(c(coal_index1, coal_index2, coal_index5, coal_index6))
coalData <- mergedData[master_coal_index, ]

# 5. comb (get it...) for combustion-related sources
comb_index1 <- which(grepl("[Cc][Oo][Mm][Bb]", coalData$Short.Name) == TRUE)   
comb_index2 <- which(grepl("[Cc][Oo][Mm][Bb]", coalData$EI.Sector) == TRUE)   
comb_index3 <- which(grepl("[Cc][Oo][Mm][Bb]", coalData$SCC.Level.Three) == TRUE)   # zero results
comb_index4 <- which(grepl("[Cc][Oo][Mm][Bb]", coalData$SCC.Level.Four) == TRUE)   

# 6. create a master vector as before, subsetting data to reflect combustion-related sources 
master_comb_index <- unique(c(comb_index1, comb_index2, comb_index4))
comb_coal <- coalData[master_comb_index, ]

# 7. arrange by year, extractive industry sector, and type; sum emissions in those categories
library(dplyr)
final_data <- comb_coal %>%
	group_by(year, EI.Sector, type) %>%
	rename(Sector = EI.Sector) %>%
	summarize(count = sum(Emissions)) %>%  
    na.omit() 
options('tibble.print_max' = 50) 
final_data

## EDA to get a sense of the values and names, research categories of EI.Sector with "Other"
## Decision: remove category containing a few innocuous observations of gasified coal 

# 8. remove "Fuel Comb - Electric Generation - Other" sector (gasified coal) 
final_data <- final_data[-c(11, 19, 26), ]

# 9. simplify names for ploting
final_data$Sector <- gsub("Fuel Comb - | - Coal| - Other", "", final_data$Sector)
final_data$Sector <- sub("Comm", "Commercial", final_data$Sector)

# 10. plot data by extractive industry sector and type 
library(ggplot2)
ggplot(final_data, aes(year, log2(count))) +
	geom_line(aes(color = Sector), size = 1) + 
	facet_grid(. ~ type) + 
	coord_cartesian(ylim = c(9, 21)) +
	labs(x="Year", y=~PM[2.5]~ "Emissions ("~Log[2]~ "tons)", 
	title=~PM[2.5]~ "Emissions from Coal Combustion-Related Sources in the U.S., by Type and Extraction Industry Sector")

# 11. create PNG file of appropriate dimensions to visualize data 
dev.copy(png, file = "plot4.png", width = 840, height = 480)
dev.off()
