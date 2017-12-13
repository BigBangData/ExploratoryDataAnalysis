## The following four functions load, prepare, and execute summary statistics plots on air pollution data from the EPA.gov website.
## Feel free to suggest improvements, fork the repo and adapt them to your own needs.

## The Load() function downloads the data, unzips the folders and renames files for easier reference (pm1 through pm4).
## The four data sets consist of pm2.5 emissions measurements for the entire U.S. for the years 2007, 2010, 2013 and 2016.
## This function stores these data sets into the working directory for future use, and only need to be called once.

## The Prep() function is a preparatory function for the main EDA function, reading the data into R and getting it ready for analysis.

## The AirQuality() function returns, for a given county, four scatterplots of pm2.5 emissions, one for each year observed. 
## The two arguments, state and county codes, are provided by the user. Codes can be found at: 
## https://aqs.epa.gov/aqsweb/documents/codetables/states_and_counties.html 

## The AirQuality plots are intended for quick EDA. Since it is hard to visualize the data when plotting the full range of observed values, 
## only the bottom 70% of the range is shown, cutting off the few outliers in the top 30% of the range. The Outliers() function returns plots 
## for those few, extremeobservations in the top 30% of the range.

## NOTE: The AirQuality and Outliers functions might not work for certain counties. For example: Houston, TX (State.Code == 48 and County.Code == 225). 
## (A quick inspection revealed no county code 225 in the data.) 

## CREDITS: I wrote this function after completing 'Exploratory Data Analysis,' Course 4 in the Johns Hopkins University Data Science 
## Specialization in Coursera. The coding was inspired by Roger Peng's "Air Pollution Case Study" lesson in week 4 of the course. 
## Credit also goes to data analyst Alan Gao for the nifty trick of evaluating parsed text in 'for' loops.


Load <- function() {

	# download data
	download.file("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_88101_2007.zip", destfile = "./data07.zip")
	download.file("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_88101_2010.zip", destfile = "./data10.zip")
	download.file("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_88101_2013.zip", destfile = "./data13.zip")
	download.file("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_88101_2016.zip", destfile = "./data16.zip")
	
	# unzip files
	sapply(list.files(pattern=".zip", full.names = TRUE), unzip)
	
	# rename files 
	file.rename(list.files(pattern="*.csv"), paste0("pm", 1:4))
}
	
	
Prep <- function() {	

	# read in files 
	files <<- list.files(pattern="pm")  
	for (i in 1:4) assign(files[i], read.csv(files[i]), .GlobalEnv) 
	
	# subset appropriate variables  
	for (i in 1:4) {
		eval(parse(text = paste("pm", i, " <<- pm", i, "[,c(1,2,12,17,25,26)]", sep = ""))) 
	}
	# create vectors of negative values in Arithmetic.Mean 
	for (i in 1:4) {
		eval(parse(text = paste("neg", i, " <<- ", "which(pm", i, "$Arithmetic.Mean < 0)", sep = "")))
	}
	# remove negative values from Arithmetic.Mean
	for (i in 1:4) {																						
		if (length(eval(parse(text = paste("neg",i,sep="")))) != 0) {  	
			eval(parse(text = paste("pm",i," <<- pm",i,"[-neg",i,", ]", sep="")))
		}
	}
}
  
  
AirQuality <- function(state, county) {

	# subset by chosen state and county 
	for (i in 1:4) eval(parse(text = paste("pm",i,"sub <<- subset(pm",i,", State.Code == state & County.Code == county)", sep="")))

	# create vectors of dates d1 to d4 and of emissions e1 to e4
	for (i in 1:4) eval(parse(text = paste("d",i," <<- as.Date(pm",i,"sub$Date.Local); e",i," <<- pm",i,"sub$Arithmetic.Mean", sep="")))	
	
	# create vector naming county and state 
	name <<- paste(pm1sub[1, 6],",",pm1sub[1, 5])
	
	# set up an AirQualityPlots folder
	output_dir <- file.path("./AirQualityPlots")
	if (!dir.exists(output_dir)) {
		dir.create(output_dir)
	} 
	
	# plot PNG to this folder
	png(file = paste("./AirQualityPlots/",name," - AirQuality.png", sep=""), width = 1100, height = 480)
	R <- range(e1, e2, e3, e4) 
	par(mfrow = c(1, 4), mar = c(4, 5, 4, 1))

	# 2007
	plot(d1, e1, ylim = R*0.7, ylab = ~PM[2.5]~"EMISSIONS (in micrograms per cubic meter)", xlab = "2007", type = "n")
	abline(h = median(e1), col = "black", lwd = 2)
	points(d1[which(e1 > 12)], e1[which(e1 > 12)], col = rgb(1, 0, 0, alpha = 0.3), pch = 19)
	points(d1[which(e1 < 12)], e1[which(e1 < 12)], col = rgb(0, 0, 1, alpha = 0.3), pch = 19)

	# 2010
	plot(d2, e2, ylim = R*0.7, ylab = "", xlab = "2010", type = "n")
	abline(h = median(e2), col = "black", lwd = 2)
	points(d2[which(e2 > 12)], e2[which(e2 > 12)], col = rgb(1, 0, 0, alpha = 0.3), pch = 19)
	points(d2[which(e2 < 12)], e2[which(e2 < 12)], col = rgb(0, 0, 1, alpha = 0.3), pch = 19)

	# 2013
	plot(d3, e3, ylim = R*0.7, ylab = "", xlab = "2013", type = "n")
	abline(h = median(e3), col = "black", lwd = 2)
	points(d3[which(e3 > 12)], e3[which(e3 > 12)], col = rgb(1, 0, 0, alpha = 0.3), pch = 19)
	points(d3[which(e3 < 12)], e3[which(e3 < 12)], col = rgb(0, 0, 1, alpha = 0.3), pch = 19)

	# 2016
	plot(d4, e4, ylim = R*0.7, ylab = "", xlab = "2016", type = "n")
	abline(h = median(e4), col = "black", lwd = 2)
	points(d4[which(e4 > 12)], e4[which(e4 > 12)], col = rgb(1, 0, 0, alpha = 0.3), pch = 19)
	points(d4[which(e4 < 12)], e4[which(e4 < 12)], col = rgb(0, 0, 1, alpha = 0.3), pch = 19)
	legend("topright", "Failed National Standard", pch = 19, col = rgb(1, 0, 0, alpha = 0.4))

	# title and legend 
    par(mfrow = c(1, 1))
	title(paste(name, "- Air Pollution Monitor Measurements"))
	dev.off()
}


Outliers <- function(state, county) {
	
	# subset, prep variables
	for (i in 1:4) eval(parse(text = paste("pm",i,"sub <<- subset(pm",i,", State.Code == state & County.Code == county)", sep="")))
	for (i in 1:4) eval(parse(text = paste("d",i," <<- as.Date(pm",i,"sub$Date.Local); e",i," <<- pm",i,"sub$Arithmetic.Mean", sep="")))	
		name <<- paste(pm1sub[1,6],",",pm1sub[1,5])
	
	# set up an AirQualityPlots folder
	output_dir <- file.path("./AirQualityPlots")
	if (!dir.exists(output_dir)) {
		dir.create(output_dir)
	} 
	
	# plot PNG to this folder
	png(file = paste("./AirQualityPlots/",name," - AirQualityOutliers.png", sep=""), width = 1100, height = 480)
	R <- range(e1, e2, e3, e4) 
	par(mfrow = c(1, 4), mar = c(4, 5, 4, 1))
	
	# 2007
	plot(d1, e1, ylim = c(max(R*0.7), max(R)), ylab = ~PM[2.5]~"EMISSIONS (in micrograms per cubic meter)", xlab = "2007", type = "n")
	abline(h = median(e1), col = "black", lwd = 2)
	points(d1[which(e1 > 12)], e1[which(e1 > 12)], col = rgb(1, 0, 0, alpha = 0.3), pch = 19)
	points(d1[which(e1 < 12)], e1[which(e1 < 12)], col = rgb(0, 0, 1, alpha = 0.3), pch = 19)
	
	# 2010
	plot(d2, e2, ylim = c(max(R*0.7), max(R)), ylab = "", xlab = "2010", type = "n")
	abline(h = median(e2), col = "black", lwd = 2)
	points(d2[which(e2 > 12)], e2[which(e2 > 12)], col = rgb(1, 0, 0, alpha = 0.3), pch = 19)
	points(d2[which(e2 < 12)], e2[which(e2 < 12)], col = rgb(0, 0, 1, alpha = 0.3), pch = 19)
	
	# 2013
	plot(d3, e3, ylim = c(max(R*0.7), max(R)), ylab = "", xlab = "2013", type = "n")
	abline(h = median(e3), col = "black", lwd = 2)
	points(d3[which(e3 > 12)], e3[which(e3 > 12)], col = rgb(1, 0, 0, alpha = 0.3), pch = 19)
	points(d3[which(e3 < 12)], e3[which(e3 < 12)], col = rgb(0, 0, 1, alpha = 0.3), pch = 19)
	
	# 2016
	plot(d4, e4, ylim = c(max(R*0.7), max(R)), ylab = "", xlab = "2016", type = "n")
	abline(h = median(e4), col = "black", lwd = 2)
	points(d4[which(e4 > 12)], e4[which(e4 > 12)], col = rgb(1, 0, 0, alpha = 0.3), pch = 19)
	points(d4[which(e4 < 12)], e4[which(e4 < 12)], col = rgb(0, 0, 1, alpha = 0.3), pch = 19)
	legend("topright", "Failed National Standard", pch = 19, col = rgb(1, 0, 0, alpha = 0.4))
    	
	# title and legend
	par(mfrow = c(1, 1))
	title(paste(name, "- Air Pollution, Extreme Observations"))
	dev.off()
}
