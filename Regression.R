`Income` <- int.pm.income.poly$Income
`PM2.5` <- int.pm.income.poly$value
linearMod <- lm(`Income`~`PM2.5`)
#linearMod <- lm(pm.income.poly$Income~pm.income.poly$value)
#linearMod <- lm(pm.income.poly$Income~dat.2nd$var1.pred)
print(linearMod)
#Call:
#lm(formula = Income ~ PM2.5)
#Coefficients:
#  (Intercept)        PM2.5  
#        31107         1342  
summary(linearMod)
#Residuals:
#Min     1Q Median     3Q    Max 
#-25873  -6840   -760   6244  44188 
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   31106.9      356.4   87.27   <2e-16 ***
#      PM2.5    1342.3      158.5    8.47   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 8709 on 3193 degrees of freedom
#Multiple R-squared:  0.02197,	Adjusted R-squared:  0.02167 
#F-statistic: 71.74 on 1 and 3193 DF,  p-value: < 2.2e-16

plot(pm.income.poly$Income~pm.income.poly$value)
abline(linearMod,col="red")

#linear regression -> justify whether independent variables
######Linear Regression##########
#Plot income and PM2.5 from the pm.income.poly dataset you created
#plot(pm.income.poly$Income~pm.income.poly$value)
plot(`Income`~`PM2.5`)
#Notice that there are a lot of 0's in this dataset. If you decide to remove them, use the following line:
#pm.income.poly.x <-  pm.income.poly[pm.income.poly$value != 0, ]
#pm.income.poly.x <- pm.income.poly[ which(pm.income.poly$value!=0), ]
#Now plot the data again
#plot(pm.income.poly.x$Income~pm.income.poly.x$value) #TODO rename axis

# is PM2.5 a good indicator of spatial variability? -> no because of above plots
# so, need to test this with the following:

#Perform a linear regression on the two variables. You should decide which one is dependent.
#lm.model <- lm(pm.income.poly$Income~pm.income.poly$value) #a simple linear model
lm.model <- lm(`Income`~`PM2.5`)
#the object lm.model now has all results of the linear regression
#Add the regression model to the plot you created
abline(lm.model,col="red") #best fit line
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
model.resids <- residuals.lm(lm.model)
#Then add the residuals to your spatialpolygon dataframe
int.pm.income.poly$residuals <- model.resids
#Observe the result to make sure it looks correct
head(int.pm.income.poly)
#colnames(int.pm.income.poly@data) <- c("DAUID", "Income", "value", "X", "Y", "Residuals") 
#spatial polygon dataset, can now plot residuals below

#Now, create choropleth map of residuals
resids <- int.pm.income.poly$residuals
shades <- auto.shading(resids, n=6, cols = brewer.pal(6, 'Blues'))
choropleth(income.tracts, resids, shades, border=NA, main="Linear Regression") #map the data with associated colours
#choro.legend(3864000, 1965000, shades) #add a legend (you might need to change the location)
choro.legend(px='bottomleft', sh=shades, cex=0.5, title = 'Residuals')

#TODO: measure residuals column, apply Global/Local Moran's I to determine if residuals are significantly spatially clustered
# talk about how they are spatially clustered
#queen's neighbour
pip.nb <- poly2nb(int.pm.income.poly) ### how to specify residuals not PM25 value???
plot(int.pm.income.poly, border = "lightgrey")
plot(pip.nb, coordinates(int.pm.income.poly), add = TRUE, col = "red")

#Create the spatial weights neighbour list using the queen's case
pip.lw <- nb2listw(pip.nb, zero.policy = TRUE, style = "W")
print.listw(pip.lw, zero.policy = TRUE)
#summary(pip.lw,zero.policy = TRUE)
#vector of residuals 
Res <- int.pm.income.poly$residuals

### Global Moran's I Test ###
#Remember that Moran’s I is a global measure of correlation
#Again use the PopDen vector
mi.res <- moran.test(Res, pip.lw, zero.policy = TRUE)
mi.res
#Moran I statistic standard deviate = 63.79, p-value < 2.2e-16
#alternative hypothesis: greater
#sample estimates:
#  Moran I statistic       Expectation          Variance 
#       0.6708786194     -0.0003132832      0.0001107104 

#To contextualize your Moran's I value, retrieve range of potential Moran's I values.
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
mr <- moran.range(pip.lw)
mr #[1] -0.812967  1.051804

Coords <- cbind(int.pm.income.poly$X, int.pm.income.poly$Y)
bws <- c(1, 1.2, 1.4, 1.6, 1.7, 1.8, 1.9,2,2.5,3,4,6,10)
library(lctools)
moran.res <- moransI.v(Coords, bws, Res)

#data:  Inc, $estimate
#Moran I statistic       Expectation          Variance 
#     0.6780371895     -0.0002983294      0.0001009969
#data:  Res, sample estimates:
#Moran I statistic       Expectation          Variance 
#     0.6708786194     -0.0003132832      0.0001107104 

#Perform the Z-test
#You can get the necessary values from your mi object resulting from your Moran's I test above.
#For example, the Moran's I value is the first value in the output mi, so you call mi$estimate[1] to get the value.
mi.res$estimate[1] #0.6708786 
z.i.res=mi.res$estimate[1]-mi.res$estimate[2]/(mi.res$estimate[3])
z.i.res #3.500633

bw <- 6
mI.res <- moransI(Coords,bw,Res)
moran.table <- matrix(data=NA,nrow=1,ncol=6)
col.names <- c("Moran's I", "Expected I", "Z resampling", "P-value resampling",
               "Z randomization", "P-value randomization")
colnames(moran.table) <- col.names
moran.table[1,1] <- mI.res$Morans.I
moran.table[1,2] <- mI.res$Expected.I
moran.table[1,3] <- mI.res$z.resampling
moran.table[1,4] <- mI.res$p.value.resampling
moran.table[1,5] <- mI.res$z.randomization
moran.table[1,6] <- mI.res$p.value.randomization
moran.table
#     Moran's I   Expected I Z resampling P-value resampling Z randomization  P-value randomization
#     0.6557299 -0.000313087     67.75547                  0        67.75427                      0

int.sample <- int.pm.income.poly[sample(1:nrow(int.pm.income.poly), 280, replace=TRUE),]
Res.s <- int.sample$residuals
pip.nb.s <- poly2nb(int.sample)
pip.lw.s <- nb2listw(pip.nb.s, zero.policy = TRUE, style = "W")
#Create a Moran's I scatterplot
moran.plot(main="Local Moran's I",Res.s, pip.lw.s, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="Residuals", 
           ylab="Spatially Lagged Residuals", quiet=NULL)
#Error in summary.listw(pip.lw.s) : regions with no neighbours found, use zero.policy=TRUE
#moran.plot(main="Local Moran's I",Res.s, pip.lw.s, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Residuals", 
#           ylab="Spatially Lagged Residuals", quiet=NULL)

#relationship between income and PM2.5 -> clustered? why -> find some explanatory variable

# p-value null hyp: independent variable does not explain the variability in dependent variable
# promblem is, large number of points -> high p-value, more likely to reject null; high n drives low p-value
# low R2 value indicates that with the above -> doesnt really explain relationship btwn variables
# so there must be something going on at a more local level => GWR
