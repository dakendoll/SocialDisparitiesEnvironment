#sp.gwr install

#GWR = determine if there is spatial variabilty in the relationship between the variables
# worthwhile to look at this variabilty, is it constant across space -> because it is difficult
# to make assumptions across large areas

####Geographically Weighted Regression
#The first thing you need to do is to add the polygon coordinates to the spatialpolygondataframe.
#You can obtain the coordinates using the "coordinates" function from the sp library
pm.income.poly.coords <- sp::coordinates(pm.income.poly)
#Observe the result
head(pm.income.poly.coords) #two col with lat & long
#Now add the coordinates back to the spatialpolygondataframe
pm.income.poly$X <- pm.income.poly.coords[,1]
pm.income.poly$Y <- pm.income.poly.coords[,2]
head(pm.income.poly)

#TODO: bandwidth: small or big -> determine this by testing different parameters (large output/tedius)
# other approach, let R code do it for you: applies diff bandwidths over & over, giving a cross-validation 
# score -> chooses one that the outputs are the least sensitive to 

###Determine the bandwidth for GWR: this will take a while (ten minutes...)
GWRbandwidth <- sp::gwr.sel(pm.income.poly$Income~pm.income.poly$PM25, 
                        data=pm.income, coords=cbind(pm.income.poly$X,pm.income.poly$Y),adapt=T) 

###Perform GWR on the two variables with the bandwidth determined above
###This will take a looooooong while (30 minutes...)
gwr.model = sp::gwr(pm.income.poly$Income~pm.income.poly$PM25, 
                data=pm.income.poly, coords=cbind(pm.income.poly$X,pm.income.poly$Y), 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 

#Print the results of the model
gwr.model

#Look at the results in detail
results<-as.data.frame(gwr.model$SDF)
head(results)

#Now for the magic. Let's add our local r-square values to the map
pm.income.poly$localr <- results$localR2
# look at R2 values -> they are highly variable!! (don't worry about negatives) tells us PM2.5 actually 
# is a good explanatory variable, but it varies across space

#Create choropleth map of r-square values
local.r.square <- pm.income.poly$localr
shades <- auto.shading(local.r.square, n=6, cols = brewer.pal(6, 'Oranges'))
choropleth(income.tracts, local.r.square, shades) #map the data with associated colours
choro.legend(3864000, 1965000, shades) #add a legend (you might need to change the location)
# "bottom left" to place legend in corner

#Time for more magic. Let's map the coefficients
pm.income.poly$coeff <- results$pm.income.poly.PM25
# look at coefficients -> also highly variable
# in results SAY: for every one unit increas in PM2.5 we are getting an increase of $$$ at this location
# this mixture of neg and pos -> neg/pos coefficients

#Create choropleth map of the coefficients
local.coefficient <- pm.income.poly$coeff
shades <- auto.shading(local.coefficient, n=6, cols = brewer.pal(6, 'Oranges'))
choropleth(income.tracts, local.coefficient, shades) #map the data with associated colours
choro.legend(3864000, 1965000, shades) #add a legend (you might need to change the location)

# look at spatially distribution of R2 and coefficients
# (keep in mind, care less about each individual DA) is there a clustering in certain areas?
# TODO: check land cover to describe areas
# on a whole, does there exist a relationship between PM2.5/income, does this vary across space
# TODO: try to make sense of WHY
# stats say if and where relationships exists -> geography student need to answer WHY -> use literature
# limitations, and how to improve for future direction
