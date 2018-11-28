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
#setwd("/Users/christopherbone/Desktop/FinalProject")

#Reading in particulate matter data and postal code file
pm25 <- read.csv("PM25.csv") #Read in PM2.5 data
pm25 <- pm25[,1:2] #select columns
#give unique ID for variables for future data join/merging
colnames(pm25) <- c("POSTALCODE", "PM25") 
#Read in related postal code data
postalcodes <- shapefile("BC_Postal_Codes")

#Join PM2.5 data with postal code data
pm25.spatial <- merge(postalcodes,pm25,by = "POSTALCODE") ##GIS join
plot(pm25.spatial)
head(pm25.spatial)

pm25.spatial <- [!is.na] #remove any NA values

#Reading in dissemination tract and income data
income <- read.csv("Income.csv") #Read in census income data  
colnames(income) <- c("DAUID", "Income") #Select only ID and Income columns
census.tracts <- shapefile("BC_DA.shp") #Read in dissemination tract shapefile
income.tracts <- merge(census.tracts,income, by = "DAUID") #Merge income and dissemination data
nrow(income.tracts) #Determine the number of columns in the dataframe
income.tracts <- income.tracts[!is.na(income.tracts$Income),]
## [1] 7582
#Spatial point data frame is like a shape file that allows spatial analysis
class(income.tracts) #check data format

#=====Methods======#
#Exploratory analysis
#Create choropleth map of income
med.income <- income.tracts$Income
shades <- auto.shading(med.income, n=6, cols = brewer.pal(6, 'Oranges'))
choropleth(income.tracts, med.income, shades) #map the data with associated colours
choro.legend(3864000, 1965000, shades) #add a legend (you might need to change the location)

#Perfor a spatial intersection on the PM2.5 and Income data
pm.income <- intersect(pm25.spatial, income.tracts)
#observe result
head(pm.income)
#Aggregate the multiple PM2.5 values for each DA. Here...
#get all rows with same DAUID (postal codes for each DA) and take the mean
pm.income <- aggregate(pm.income$PM25~pm.income$DAUID,FUN=mean) #average PM2.5 for each DA
#Now a table, no longer a shapefile -> we have DAUID as a column, so link back to 
# shapefile so every polygon has an income and PM2.5 value
#Change column names
colnames(pm.income) <- c("DAUID", "PM25")
#Remove any NAs
pm.income <- na.omit(pm.income)
## POLY merge...
#...
#TODO: create choropleth, zoom in to diff areas & remove black lines

#Join the PM2.5 and the income spatial datasets together. Notice how the output differs
#given the order that you put the datasets in the intersect function. Keep in mind that you need
#to have both income and PM2.5 for each census tract.
intersect.result <- intersect(pm25.spatial,income.tracts)
intersect.result2 <- intersect(income.tracts,pm25.spatial)
intersect.result
intersect.result2

head(intersect.result2)
plot(intersect.result2)
#TODO: Use previous assignment dissemination code
#TODO: one value of pm25 in each polygon (previous assignment used avg) because analysis will be at polygon level

#Use the class function to determine if your values are stored as numbers or factors
class(intersect.result$PM25) #[1] "factor" --> cannot apply numeric function on a factor
#Change them to something you can use in the analysis
PM_25 <- as.double(intersect.result$PM25)/10 #this function changes pm25 to double
head(PM_25) #numeric now
income <- as.double(intersect.result$Income) #this function changes income to double
head(income) #numeric now

#Plot the data
plot(PM_25~income)
plot(income~PM_25)

##TODO: Data Prep Steps
#1. Prepare census data in order to represent income at the dissemination level
#2. Create a continuous surface of PM2.5 values from sampled data. 
#3. Transform pollution data in order for each dissemination area to contain a single PM2.5 value.




