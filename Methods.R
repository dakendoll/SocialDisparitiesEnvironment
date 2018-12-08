##check factor, check file types
##plot histogram to use in report

##TODO: Analysis Steps:
#1. Perform descriptive statistics to provide a description of income and pollution in your study site.
#2. Perform an analysis to determine if there exists significant income segregation in your study site.
#3. Determine if the spatial variability in pollution level explains the spatial variability in income or visa versa. Evaluate any assumptions for conducting this analysis, ensuring that assumptions are not erroneously violated. 
#4. Evaluate the sampling structure of the pollution sample by determining if they are randomly located across the study site or not.

# keep methods description concise -> use previous assignments! BUT ONLY WHAT IS USEFUL FOR THIS PAPER
# use graphs and figures to describe, keep word count for discussion!!
# avoid language about hypothesis in a project like this

library(gridExtra)
library(ggmap)
library(maptools)
library(maps)
#Calculating descriptive statistics
head(pm.income.i$PM25)
head(pm.income.i$Income)
class(pm.income.i$PM25)
class(pm.income.i$Income)
head(income)
head(pm25$PM25)
p <- as.double(pm25$PM25)/10 
p <- na.omit(p)
i <- income
head(pm25.spatial$PM25) #pm25.spatial
head(income.tracts$Income) #income.tracts
##Mean
meanPM25 <- mean(p) #population mean
meanIncome <- mean(i) #2015 mean

# Find the median. median.result <- median(x)
medianPM25 <- median(p)
medianIncome <- median(i)

#Standard Deviation
sdPM25 <- sd(p) #population standard deviation
sdIncome <- sd(i) #mean standard deviation

#Mode
modePM25 <- as.numeric(names(sort(-table(p)))[1]) #population mode; sort the dataset and read the first row (most frequent)
modeIncome <- as.numeric(names(sort(-table(i)))[1])

#Creating table
pmtext = expression('PM'[2.5])
Samples = c('PM2.5', 'Income') #Create an object for the labels
Mean = c(meanPM25, meanIncome) #Create an object for the means
Median = (c(medianPM25, medianIncome))
Mode = (c(modePM25, modeIncome))
`Std Dev` = (c(sdPM25, sdIncome))
data.for.table = data.frame(Samples, Mean, Median, Mode, `Std Dev`)

#Printing a table (you can use the same setup for printing other types of objects
png("DescriptiveStatsTable.png") #Create an object to print the table to
grid.table(data.for.table, row.names(NULL)) #Create table
dev.off() #Print table

###############POINT PATTERN ANALYSIS
#proj4string(pm.spdf) <- proj4string(pm25.spatial)
#project to bc albers
pma <- spTransform(pm.spdf, CRS("+init=epsg:3005"))
#add coordinates to the data
pma$x <- coordinates(pma)[,1]
pma$y <- coordinates(pma)[,2]
head(pma)
#create an "extent" object which can be used to create the observation window for spatstat
pma.ext <- as.matrix(extent(pma)) 
head(pma.ext)
#observation window
window <- as.owin(list(xrange = pma.ext[1,], yrange = pma.ext[2,]))

#create ppp oject from spatstat
pma.ppp <- ppp(x = pma$x, y = pma$y, window = window)

###KERNEL DENSITY ESTIMATION
#2D (gaussian) kernel, compare how bandwidth (sigma) selection influences the point density estimates
#since data are projected, sigma is represented in metres
#eps is the width and height of the pixels (1000m X 1000m)
#coerce to a SpatialGridDataFrame for plotting
kde.100 <- density(pma.ppp, sigma = 100, at = "pixels", eps = c(1000, 1000))
kde.SG <- as(kde.100, "SpatialGridDataFrame")
kde.500 <- density(pma.ppp, sigma = 500, at = "pixels", eps = c(1000, 1000))
kde.SG <- cbind(kde.SG, as(kde.500, "SpatialGridDataFrame"))
kde.1k <- density(pma.ppp, sigma = 1000, at = "pixels", eps = c(1000, 1000)) 
kde.SG <- cbind(kde.SG, as(kde.1k, "SpatialGridDataFrame"))
kde.5k <- density(pma.ppp, sigma = 5000, at = "pixels", eps = c(1000, 1000))
kde.SG <- cbind(kde.SG, as(kde.5k, "SpatialGridDataFrame"))

names(kde.SG) <- c("kde.100m", "kde.500m", "kde.1km", "kde.5km")
#plot
x11() #opens a new plot window
spplot(kde.SG)

#can see how the bandwidth selection influences the density estimates
summary(kde.SG)

#use cross-validation to get the bandwidth that minimizes MSE
bw.d <- bw.diggle(pma.ppp)
#plot the "optimal" bandwidth
plot(bw.d, ylim=c(-10, 10), main="Cross validation for PM25 measurements")

#density using the cross-validation bandwidth
kde.bwo <- density(pma.ppp, sigma = bw.d, at = "pixels", eps = c(1000, 1000))
plot(kde.bwo)

###K-FUNCTION 
#basic k-function
k.fun <- Kest(pma.ppp, correction = "Ripley")
plot(k.fun)

#use simulation to test the point pattern against CSR
k.fun.e <- envelope(pma.ppp, Kest, nsim = 99, correction = "Ripley")
plot(k.fun.e)

###QUADRAT ANALYSIS
##First, determine the number of qusdrats 
quads <- 10

qcount = quadratcount(pma.ppp, nx = quads, ny = quads)

plot(pma.ppp, pch = "+", cex = 0.5)
plot(qcount, add = T, col = "red")

qcount.df <- as.data.frame(qcount)

##Second, count the number of quadrats with a distinct number of points.
qcount.df = plyr::count(qcount.df,'Freq')
##Change the column names so that x=number of points and f=frequency of quadrats with x cells.
colnames(qcount.df) = c("x","f")

##Third, create new columns for total number of points and for fx^2.
qcount.df$TotPoints <- qcount.df$x * qcount.df$f
qcount.df$fx2 = (qcount.df$x)^2 * qcount.df$f
qcount.df$xfx2 = qcount.df$fx2 * qcount.df$f #adjusted for the count 

##Fourth, calculate the sum of each column, which you will use as inputs into the 
##formula for VMR.
f.sum = sum(qcount.df$f)
TotPoints.sum = sum(qcount.df$TotPoints) 
fx2.sum = sum(qcount.df$fx2) 

##Fifth, calculate VAR, MEAN, and VMR. ### OF WHICH VARIABLES? f.sum, TotPoints.Sum, fx2.sum?
m = sum(qcount.df$f)
VAR = sum(qcount.df$xfx2)/(m-1)
mean.points = TotPoints.sum/(m) # n/m
VMR = VAR/mean.points

##Finally, perform the test statistic to test for the existence of a random spatial pattern.
chi.square = VMR*(m-1)
p = 1 - pchisq(chi.square,nrow(qcount.df))
#The P-Value is < 0.00001. The result is significant at p < 0.05

#################################################
##Spatial Interpolation with Polynomial Trends
# Define the 1st order polynomial equation

# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(pm.income.poly, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

proj4string(grd) <- proj4string(census.tracts)

f.1 <- as.formula(value ~ X + Y) 

# Run the regression model
lm.1 <- lm( f.1, data=pm.income.poly)

# Use the regression model output to interpolate the surface
dat.1st <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.1, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.1st)
r.m <- raster::mask(r, census.tracts)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", midpoint = NA, 
            title="Predicted PM25 \n(in ppm)") +
  tm_shape(pm.income.poly) + tm_dots(size=0) +
  tm_legend(legend.outside=TRUE)

# Define the 2nd order polynomial equation
f.2 <- as.formula(value ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))

# Run the regression model
lm.2 <- lm( f.2, data=pm.income.poly)

# Use the regression model output to interpolate the surface
dat.2nd <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.2, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.2nd)
r.m <- raster::mask(r, census.tracts)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", midpoint = NA,
            title="Predicted PM25 \n(in ppm)") +
  tm_shape(pm.income.poly) + tm_dots(size=0) +
  tm_legend(legend.outside=TRUE)

summary(lm.1)
summary(lm.2)
confint(lm.1, level=0.95)
confint(lm.2, level=0.95)
plot(fitted(lm.1),residuals(lm.1),xlab="fitted",ylab="residuals",main=expression(1^st~Order~Polynomial~Regression~Model))
plot(fitted(lm.2),residuals(lm.2),xlab="fitted",ylab="residuals",main=expression(2^nd~Order~Polynomial~Regression~Model))


######SUBSAMPLING FOR SPATIAL INTERPOLATION
# take a random sample of size 50 from a dataset mydata 
# sample with replacement
#df[sample(nrow(df), 3), ]
#census.tracts.t = spTransform(census.tracts, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
pmsample <- pm.spdf[sample(1:nrow(pm.spdf), 30, replace=TRUE),]
pmsample <- na.omit(pmsample)
proj4string(pmsample) <- proj4string(census.tracts)
head(pmsample)

#################################################
##Spatial Interpolation with Kriging
# Define the 1st order polynomial equation
#f.1 <- as.formula(value ~ X + Y)
f.2 <- as.formula(value ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))
var.smpl <- variogram(f.2, pm.income.poly, cloud = FALSE) #, cutoff=1000000, width=89900)
#var.smpl.f1 <- variogram(f.1, pm.income.poly, cloud = FALSE)
#var.smpl <- variogram(f.1, pmsample, cloud = FALSE)

#plot(variogramLine(vgm(1, "Mat", 1, kappa = 4), 10), type = 'l')
###Fitting variogram models in gstat
###vgm can take a set of models, in which case the best fitting is returned
fit.variogram(var.smpl, vgm(c("Exp", "Sph")))
fit.variogram(var.smpl, vgm(c("Exp", "Gau", "Sph")))
#fit.variogram(var.smpl.f1, vgm(c("Exp", "Sph")))
#fit.variogram(var.smpl.f1, vgm(c("Exp", "Gau", "Sph")))
#plot(var.smpl.f1, dat.fit, main="Spherical Model: Sill = 1.209449 & Range = 9.069939")

dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=0.7061809, model="Sph", range=8.363199, nugget=0))
plot(var.smpl, dat.fit, main="Spherical Model")

dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=0.6404842, model="Gau", range=3.285871, nugget=0.06180516))
plot(var.smpl, dat.fit, main="Gaussian Model")
#temp <- locator(1)
#text(temp, "RMSE = 0.004075", col="grey")

#text(locator(1), "sill = 1.4e-05\nrange = 15")

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
#proj4string(pm.poly) <- proj4string(census.tracts)
#proj4string(pm.poly) = CRS(proj4string(census.tracts))
dat.krg <- krige(f.2, pm.income.poly, grd, dat.fit)
#dat.krg <- krige( f.1, pmsample, grd, dat.fit)
# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- raster::mask(r, census.tracts)

# Plot the map
krg.map <- tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", midpoint = NA, 
            title="Predicted PM2.5 \n(in ppm)") +
  tm_shape(pm.income.poly) + tm_dots(size=0) +
  tm_legend(legend.outside=TRUE)
krg.map

# Convert kriged surface to a variance raster object for clipping
r   <- raster(dat.krg, layer="var1.var")
r.m <- raster::mask(r, census.tracts)

krg.var.map <- tm_shape(r.m) + 
  tm_raster(n=7, palette ="Reds",
            title="Variance map \n(in squared ppm)") +
  tm_shape(pm.income.poly) + 
  tm_dots(size=0) +
  tm_legend(legend.outside=TRUE) + 
  tm_layout(legend.text.size=0.9)
krg.var.map

# Convert kriged surface to a confidence raster object for clipping
r   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
r.m <- raster::mask(r, census.tracts)

krg.ci.map <- tm_shape(r.m) + 
  tm_raster(n=7,
            title="95% CI map \n(in ppm)") +tm_shape(pm.income.poly) + tm_dots(size=0) +
  tm_legend(legend.outside=TRUE) + 
  tm_layout(legend.text.size=0.9)
krg.ci.map


###############INCOME SPATIAL AUTOCORRELATION
###Create Spatial Neighbourhood Weights Matrix ###
#queen's neighbour
pip.nb <- poly2nb(pm.income.poly)

plot(pm.income.poly)
plot(pip.nb, coordinates(pm.income.poly), add = TRUE, col = "red")

#rooks neighbour
pip.nb2 <- poly2nb(pm.income.poly, queen = FALSE)
#Now, create a new map that overlays the rook neighbors file (in yellow) onto the queen neighbors file (in red).
plot(pm.income.poly, border = "lightgrey")
plot(pip.nb, coordinates(pm.income.poly), add = TRUE, col = "red")
plot(pip.nb2, coordinates(pm.income.poly), add = TRUE, col = "yellow", lwd = 2, lty = "dashed")

#Create the spatial weights neighbour list using the queen's case
pip.lw <- nb2listw(pip.nb, zero.policy = TRUE, style = "W")
print.listw(pip.lw, zero.policy = TRUE)
#summary(pip.lw,zero.policy = TRUE)
#vectors of population densities & private dwelling vacancies 
Inc <- pm.income.poly$Income

### Global Moran's I Test ###
#Remember that Moran’s I is a global measure of correlation
#Again use the PopDen vector
mi <- moran.test(Inc, pip.lw, zero.policy = TRUE)
mi
#Moran I statistic       Expectation          Variance 
#     0.6824888652     -0.0003553660      0.0001228718

#To contextualize your Moran's I value, retrieve range of potential Moran's I values.
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
mr <- moran.range(pip.lw)
mr #[1] -1.000000  1.090676

Coords <- cbind(pm.income.poly$X, pm.income.poly$Y)
bws <- c(1, 1.2, 1.4, 1.6, 1.7, 1.8, 1.9,2,2.5,3,4,6,10)
library(lctools)
moran <- moransI.v(Coords, bws, Inc)

#Perform the Z-test
#You can get the necessary values from your mi object resulting from your Moran's I test above.
#For example, the Moran's I value is the first value in the output mi, so you call mi$estimate[1] to get the value.
mi$estimate[1] #0.6824889
z.i=mi$estimate[1]-mi$estimate[2]/(mi$estimate[3])
z.i #3.574657

bw <- 6
mI <- moransI(Coords,bw,Inc)
moran.table <- matrix(data=NA,nrow=1,ncol=6)
col.names <- c("Moran's I", "Expected I", "Z resampling", "P-value resampling",
               "Z randomization", "P-value randomization")
colnames(moran.table) <- col.names
moran.table[1,1] <- mI$Morans.I
moran.table[1,2] <- mI$Expected.I
moran.table[1,3] <- mI$z.resampling
moran.table[1,4] <- mI$p.value.resampling
moran.table[1,5] <- mI$z.randomization
moran.table[1,6] <- mI$p.value.randomization
moran.table
#     Moran's I    Expected I Z resampling P-value resampling Z randomization
#[1,]  0.670104 -0.0003549876     65.16051                  0         65.1596
#P-value randomization
#[1,]                0

### Local Moran's I ###
lisa.test <- localmoran(Inc, pip.lw)
lisa.test
summary(lisa.test)
lisa.test <- na.omit(lisa.test)

#Create a choropleth map of the LISA values.
lisa.shades <- auto.shading(c(lisa.test[,1],-lisa.test[,1]),cols=brewer.pal(5,"PRGn"))
choropleth(pm.income.poly, lisa.test[,1],shading=lisa.shades, main="Local Moran's I for Income", border=NA)
#legend2.text = expression(paste("LISA ValuesPopulation Density per km"^"2"," in 2016"))
choro.legend(px='bottomleft', sh=lisa.shades, cex=0.5,fmt="%6.2f", title="LISA Values")

#Create a Moran's I scatterplot
#Below is a scatterplot of county vote for Obama and its spatial lag
#(average vote received in neighboring counties). The Moran’s I
#coefficient is drawn as the slope of the linear relationship between
#the two. The plot is partitioned into four quadrants: low-low,
#low-high, high-low and high-high.

sample.inc <- pm.income.poly[sample(1:nrow(pm.income.poly), 280, replace=TRUE),]
Inc.s <- sample.inc$Income
pip.nb.s <- poly2nb(sample.inc)
pip.lw.s <- nb2listw(pip.nb.s, zero.policy = TRUE, style = "W")
moran.plot(main="Local Moran's I",Inc.s, pip.lw.s, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="Income", 
           ylab="Spatially Lagged Income", quiet=NULL)
#moran.plot(main="Local Moran's I",Inc.s, pip.lw.s, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Income", 
#           ylab="Spatially Lagged Income", quiet=NULL)
#Get P-values from last column of LISA table
#Below is a plot of Local Moran |z|-scores for the 2008 Presidential
#Elections. Higher absolute values of z scores (red) indicate the
#presence of “enclaves”, where the percentage of the vote received
#by Obama was significantly different from that in neighboring
#counties.
p.lisa=lisa.test[,5]
p.lisa.shades <- auto.shading(c(lisa.test[,5],-lisa.test[,5]),cols=brewer.pal(5,"PRGn"))
#Create a choropleth map of the LISA P-values.
choropleth(pm.income.poly,p.lisa,shading = p.lisa.shades, main = "Income LISA P-values", border=NA)
choro.legend(px='bottomleft', sh=p.lisa.shades, cex=0.5,fmt="%6.2f")

