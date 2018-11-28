linearMod <- lm(income~PM_25)
print(linearMod)
summary(linearMod)
plot(income~PM_25)
abline(linearMod)

#linear regression -> justify whether independent variables
######Linear Regression##########
#Plot income and PM2.5 from the pm.income.poly dataset you created
plot(pm.income.poly$Income~pm.income.poly$PM25)
#Notice that there are a lot of 0's in this dataset. If you decide to remove them, use the following line:
pm.income.poly <-  pm.income.poly[pm.income.poly$PM25 != 0, ]
#Now plot the data again
plot(pm.income.poly$Income~pm.income.poly$PM25) #TODO rename axis

# is PM2.5 a good indicator of spatial variability? -> no because of above plots
# so, need to test this with the following:

#Perform a linear regression on the two variables. You should decide which one is dependent.
lm.model <- lm(pm.income.poly$Income~pm.income.poly$PM25) #a simple linear model
#the object lm.model now has all results of the linear regression
#Add the regression model to the plot you created
abline(lm.model) #best fit line
#increasing slope = positive correlation between the variables
#Get the summary of the results
summary(lm.model) 
#look at a,b coefficients (estimate columns) -> for every PM2.5 increase, income increases by $1343.20
#look at p-value -> tells us that PM2.5 is a significant explanatory variable for spatial variability of income
#look at R2 value 0.0409 -> our points are really scattered far around the model thus not a very good fit
#thus considering all above together, not actually a lot going on with two variables
#pm2.5 is not nessessarily explaining income

# need to make sure you meet the 4 assumtions!!
#specifically *are the residuals independent?* -> do they influence eachother
#as geographers, do we see Tobler's first law present in the residuals
#need to add a column with residual values to pm.income.poly

#You want to determine if the model residuals are spatially clustered. 
#First obtain the residuals from the model
model.resids <- as.data.frame(residuals.lm(lm.model))
#Then add the residuals to your spatialpolygon dataframe
pm.income.poly$residuals <- residuals.lm(lm.model)
#Observe the result to make sure it looks correct
head(pm.income.poly)
#spatial polygon dataset, can now plot residuals below

#Now, create choropleth map of residuals
resids <- pm.income.poly$residuals
shades <- auto.shading(resids, n=6, cols = brewer.pal(6, 'Greens'))
choropleth(income.tracts, resids, shades) #map the data with associated colours
choro.legend(3864000, 1965000, shades) #add a legend (you might need to change the location)

#TODO: measure residuals column, apply Global/Local Moran's I to determine if residuals are significantly spatially clustered

#relationship between income and PM2.5 -> clustered? why -> find some explanatory variable
