#Geog 418 Fall 2018 Final Project

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

#Reading in particulate matter data and postal code file
pm25 <- read.csv("PM25.csv") #Read in PM2.5 data
pm25 <- pm25[,1:2] #extract the first two columns only
colnames(pm25) <- c("POSTALCODE", "PM25") #change the column heading so that you can join it with a spatial dataset later
postalcodes <- shapefile("BC_Postal_Codes") #Read in related postal code data

#Reading in dissemination tract and income data
income <- read.csv("Income.csv") #Read in census income data  
colnames(income) <- c("DAUID", "Income") #Select only ID and Income columns
census.tracts <- shapefile("BC_DA.shp") #Read in dissemination tract shapefile
income.tracts <- merge(census.tracts,income, by = "DAUID") #Merge income and dissemination data
nrow(income.tracts) #Determine the number of columns in the dataframe
income.tracts <- income.tracts[!is.na(income.tracts$Income),] #Remove NAs from the dataset

#Create choropleth map of income (needs projection fix)
med.income <- income.tracts$Income #create an object for income
shades <- auto.shading(med.income, n=6, cols = brewer.pal(6, 'Oranges')) #set the colours you want to use
choropleth(income.tracts, med.income, shades) #map the data with associated colours
choro.legend(3864000, 1965000, shades) #add a legend (you might need to change the location)

#Select postal codes that fall within dissemination tracts)
postalcodes <- intersect(postalcodes,income.tracts)

#Join PM2.5 data with postal code data
pm25.spatial <- merge(postalcodes,pm25,by = "POSTALCODE")
plot(pm25.spatial) #see what the data looks like
