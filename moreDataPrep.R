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

library(raster)
library(gstat)
library(maptools)
library(tmap)

#Set working directory
#setwd("/Users/christopherbone/Desktop/FinalProject")

##TODO: Data Prep Steps
#1. Prepare census data in order to represent income at the dissemination level
#2. Create a continuous surface of PM2.5 values from sampled data. 
#3. Transform pollution data in order for each dissemination area to contain a single PM2.5 value.

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
head(income)

#Read in the dissemination tract shapefile
census.tracts <- shapefile("BC_DA_gvrd.shp") #shapefile("BC_DA.shp")

#Merge the income dataset and the DA shapefile
income.tracts <- merge(census.tracts,income, by = "DAUID") 
head(income.tracts)
#Remove any NA's from the merged dataset
income.tracts <- income.tracts[!is.na(income.tracts$Income),]

#Create choropleth map of income
med.income <- income.tracts$Income
shades <- auto.shading(med.income, n=6, cols = brewer.pal(6, 'Purples'))
choropleth(income.tracts, med.income, shades,border=NA) #map the data with associated colours
#choro.legend(3864000, 1965000, shades) #add a legend (you might need to change the location)
choro.legend(px='bottomleft', sh=shades, cex=0.5, title = "Median Income")
par("usr")

#Perform a spatial intersection on the PM2.5 and Income data
#Join the PM2.5 and the income spatial datasets together. Notice how the output differs
#given the order that you put the datasets in the intersect function. Keep in mind that you need
#to have both income and PM2.5 for each census tract.
pm.income.i <- intersect(pm25.spatial,income.tracts)
#Observe the result
head(pm.income.i)
class(pm.income.i$PM25)
#[1] "factor" --> cannot apply numeric function on a factor
#Change them to something you can use in the analysis
PM_25 <-as.double(pm.income.i$PM25)/10 
head(PM_25)
pm.income.i$PM25 <- PM_25
class(pm.income.i$PM25)
class(income.tracts$Income)

#TODO: Use previous assignment dissemination code
#TODO: one value of pm25 in each polygon (previous assignment used avg) because analysis will be at polygon level

#Aggregate the the multiple PM2.5 values for each DA. Here the mean function is used.
pm.income <- aggregate(pm.income.i$PM25~pm.income.i$DAUID,FUN=mean)
#Change the column names
colnames(pm.income) <- c("DAUID", "PM25")
head(pm.income)
#Remove any NA's
pm.income <- na.omit(pm.income)

#Seeing as how the datasets are not properly merged, perform another merge to have PM and income together
pm.income.poly <- merge(income.tracts,pm.income,by = "DAUID")
pm.income.poly <- na.omit(pm.income.poly)
#Remove unwanted columns
pm.income.poly <- pm.income.poly[,-(2:23)]
#Observe the result.
head(pm.income.poly)
#Are there still NA's? If so, apply following line to get rid of them.
pm.income.poly <- pm.income.poly[!is.na(pm.income.poly$PM25),]

#Create choropleth map of PM25
avg.pm <- pm.income.poly$PM25
shades <- auto.shading(avg.pm, n=6, cols = brewer.pal(6, 'Greens'))
#create choropleth, zoom in to diff areas & remove black lines
choropleth(income.tracts, avg.pm, shades, border=NA) #map the data with associated colours
#choro.legend(3864000, 1965000, shades) #add a legend (you might need to change the location)
choro.legend(px='bottomleft', sh=shades, cex=0.5, title = expression('Mean PM'[2.5]))

plot(pm.income.poly$Income~pm.income.poly$PM25)
plot(pm.income.poly$PM25~pm.income.poly$Income)

pm.income.poly.coords <- sp::coordinates(pm.income.poly)
#Observe the result
head(pm.income.poly.coords) #two col with lat & long
#Now add the coordinates back to the spatialpolygondataframe
pm.income.poly$X <- pm.income.poly.coords[,1]
pm.income.poly$Y <- pm.income.poly.coords[,2]
head(pm.income.poly)
colnames(pm.income.poly@data) <- c("DAUID", "Income", "value", "X", "Y") 
#Merge the the monitoring station shapefile with the ozone data using the site column.  
#mrg.tab.mean <- sp::merge(SC.monitor.t, mean.ozone, by = "site", all.x = FALSE) 
pm.poly <- pm.income.poly[,-(2)]
colnames(pm.poly@data) <- c("DAUID", "value", "X", "Y") 
head(pm.poly)

#mean.ozone = aggregate(value ~ site, ozone, mean)
#mrg.tab.mean <- sp::merge(census.tracts, mean.ozone, by = "site", all.x = FALSE) 
#pm.mrg <- sp::merge(census.tracts,avg.pm,by = "DAUID")
#pm <- pm.poly$value
# points from scratch
coords = cbind(pm.income.poly$X, pm.income.poly$Y)
#sp = SpatialPoints(coords)
pm.spdf <- SpatialPointsDataFrame(coords, pm.income.poly@data, coords.nrs = numeric(0), 
                       proj4string = CRS(as.character(NA)), match.ID = TRUE)
# back to data....as.data.frame(data) OR data@data
proj4string(pm.spdf) <- proj4string(census.tracts)

#tmmap <- tm_shape(census.tracts) + tm_polygons() +
#  tm_shape(pmsample) +
#  tm_dots(col="value", palette = "RdBu", auto.palette.mapping = FALSE,
#          title="Sampled PM25 \n(in ppm)", size=0.7) +
#  tm_text("value", just="left", xmod=.5, size = 0.7) +
#  tm_legend(legend.outside=TRUE)
#tmmap
tmmap <- tm_shape(census.tracts) + tm_polygons() +
  tm_shape(pm.spdf) +
  tm_dots(col="value", palette = "RdBu", auto.palette.mapping = FALSE,
          title=expression('Sampled PM'[2.5]*'(in ppm)'), size=0.3) + 
  tm_legend(legend.outside=TRUE) + 
  tm_layout(legend.text.size=1)
#  tm_layout(expression(atop('Sampled PM'[2.5],'(in ppm)')), 
#            title.size = 0.8, 
#            frame = FALSE, legend.text.size=1)
tmmap

