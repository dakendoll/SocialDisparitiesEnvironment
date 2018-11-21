#Geog 418/518 Final Project

library(plyr)
library(dplyr)
library(spdep)
library(GISTools)
library(raster)
library(maptools)
library(rgdal)
library(spatstat)
library(sp)
library(spatstat)
#Set working directory
setwd("/Users/christopherbone/Desktop/FinalProject")

#Reading in particulate matter data and postal code file
pm25 <- read.csv("PM25.csv") #Read in PM2.5 data
pm25 <- pm25[,1:2]
colnames(pm25) <- c("POSTALCODE", "PM25")
postalcodes <- shapefile("BC_Postal_Codes") #Read in related postal code data

#Join PM2.5 data with postal code data
pm25.spatial <- merge(postalcodes,pm25,by = "POSTALCODE")
plot(pm25.spatial)
head(pm25.spatial)


#Reading in dissemination tract and income data
income <- read.csv("Income.csv") #Read in census income data  
colnames(income) <- c("DAUID", "Income") #Select only ID and Income columns
census.tracts <- shapefile("BC_DA.shp") #Read in dissemination tract shapefile
income.tracts <- merge(census.tracts,income, by = "DAUID") #Merge income and dissemination data
nrow(income.tracts) #Determine the number of columns in the dataframe
income.tracts <- income.tracts[!is.na(income.tracts$Income),]

#Create choropleth map of income
med.income <- income.tracts$Income
shades <- auto.shading(med.income, n=6, cols = brewer.pal(6, 'Oranges'))
choropleth(income.tracts, med.income, shades) #map the data with associated colours
choro.legend(3864000, 1965000, shades) #add a legend (you might need to change the location)

#Join the PM2.5 and the income spatial datasets together. Notice how the output differs
#given the order that you put the datasets in the intersect function. Keep in mind that you need
#to have both income and PM2.5 for each census tract.
intersect.result <- intersect(pm25.spatial,income.tracts)
intersect.result2 <- intersect(income.tracts,pm25.spatial)
intersect.result
intersect.result2

#Use the class function to determine if your values are stored as numbers or factors
class(intersect.result$PM25)
#Change them to something you can use in the analysis
PM_25 <- as.double(intersect.result$PM25)/10
PM_25
income <- as.double(intersect.result$Income)
income

#Plot the data
plot(PM_25~income)
plot(income~PM_25)
