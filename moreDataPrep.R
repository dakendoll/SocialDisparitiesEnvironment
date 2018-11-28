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
library(spgwr)

#Set working directory
setwd("/Users/christopherbone/Desktop/FinalProject")


#Reading in particulate matter dataset
pm25 <- read.csv("PM25.csv") #Read in PM2.5 data
#Select only columns 1 and 2
pm25 <- pm25[,1:2]
#Change the column names 
colnames(pm25) <- c("POSTALCODE", "PM25")

#Reading in postal code shapefile
postalcodes <- shapefile("BC_Postal_Codes") #Read in related postal code data
#Join PM2.5 data with postal code data using the POSTALCODE column
pm25.spatial <- merge(postalcodes,pm25,by = "POSTALCODE")
#Plot the points on a map
plot(pm25.spatial)
#Examine the first several rows
head(pm25.spatial)
#You should notice that the dataset contains NA's, so these need to be removed.
pm25.spatial <- pm25.spatial[!is.na(pm25.spatial$PM25),]


#Reading in the income dataset
income <- read.csv("Income.csv") #Read in census income data  
#Change the column names
colnames(income) <- c("DAUID", "Income") #Select only ID and Income columns

#Read in the dissemination tract shapefile
census.tracts <- shapefile("BC_DA.shp") 
#Merge the income dataset and the DA shapefile
income.tracts <- merge(census.tracts,income, by = "DAUID") 
#Remove any NA's from the merged dataset
income.tracts <- income.tracts[!is.na(income.tracts$Income),]

#Create choropleth map of income
med.income <- income.tracts$Income
shades <- auto.shading(med.income, n=6, cols = brewer.pal(6, 'Oranges'))
choropleth(income.tracts, med.income, shades) #map the data with associated colours
choro.legend(3864000, 1965000, shades) #add a legend (you might need to change the location)

#Perform a spatial intersection on the PM2.5 and Income data
pm.income <- intersect(pm25.spatial,income.tracts)
#Observe the result
head(pm.income)
#Aggregate the the multiple PM2.5 values for each DA. Here the mean function is used.
pm.income <- aggregate(pm.income$PM25~pm.income$DAUID,FUN=mean)
#Change the column names
colnames(pm.income) <- c("DAUID", "PM25")
#Remove any NA's
pm.income <- na.omit(pm.income)

#Seeing as how the datasets are not properly merged, perform another merge to have PM and income together
pm.income.poly <- merge(income.tracts,pm.income,by = "DAUID")
pm.income.poly <- na.omit(pm.income.poly)
#Remove unwanted columns
pm.income.poly <- pm.income.poly[,-(2:23)]
#Observe the result. Are there still NA's? If so, apply following line to get rid of them.
pm.income.poly <- pm.income.poly[!is.na(pm.income.poly$PM25),]

#Create choropleth map of PM25
avg.pm <- pm.income.poly$PM25
shades <- auto.shading(avg.pm, n=6, cols = brewer.pal(6, 'Greens'))
choropleth(income.tracts, avg.pm, shades) #map the data with associated colours
choro.legend(3864000, 1965000, shades) #add a legend (you might need to change the location)