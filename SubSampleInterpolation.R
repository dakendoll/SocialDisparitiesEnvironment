###### PM2.5 Spatial Interpolation ######

# SUBSAMPLING
# take a random sample of size 50 from a dataset mydata 
# sample with replacement
#df[sample(nrow(df), 3), ]
#census.tracts.t = spTransform(census.tracts, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
#pmsample <- pm.spdf[sample(1:nrow(pm.spdf), 100, replace=TRUE),]
#7.5% = 250 vs 10% = 335
sample.poly <- pm.income.poly[sample(1:nrow(pm.income.poly), 280, replace=TRUE),]
proj4string(sample.poly) <- proj4string(census.tracts)
head(sample.poly)
plot(sample.poly)

# points from scratch
coords = cbind(sample.poly$X, sample.poly$Y)
#sp = SpatialPoints(coords)
sample.spdf <- SpatialPointsDataFrame(coords, sample.poly@data, coords.nrs = numeric(0), 
                                         proj4string = CRS(as.character(NA)), match.ID = TRUE)
# back to data....as.data.frame(data) OR data@data
proj4string(sample.spdf) <- proj4string(census.tracts)

#map sample data
sample.tmmap <- tm_shape(census.tracts) + tm_polygons() +
  tm_shape(sample.spdf) +
  tm_dots(col="value", palette = "RdBu",  midpoint = NA,
          title=expression('SubSampled PM'[2.5]*'(in ppm)'), size=0.3) + 
  tm_legend(legend.outside=TRUE) + 
  tm_layout(legend.text.size=1)
sample.tmmap

`Subsample Income` <- sample.spdf$Income
`Subsample PM2.5` <- sample.spdf$value
#Plot income and PM2.5 from the pm.income.poly dataset you created
plot(`Subsample Income`~`Subsample PM2.5`)

f.1 <- as.formula(value ~ X + Y) 
# Run the regression model
lm.1 <- lm( f.1, data=sample.poly)

# Use the regression model output to interpolate the surface
dat.1st <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.1, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.1st)
r.m <- raster::mask(r, census.tracts)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", midpoint = NA, 
            title="Predicted PM2.5 \n(in ppm)") +
  tm_shape(sample.poly) + tm_dots(size=0.1, alpha=0.5) +
  tm_legend(legend.outside=TRUE)

# Define the 2nd order polynomial equation
f.2 <- as.formula(value ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))

# Run the regression model
lm.2 <- lm( f.2, data=sample.poly)

# Use the regression model output to interpolate the surface
dat.2nd <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.2, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.2nd)
r.m <- raster::mask(r, census.tracts)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", midpoint = NA,
            title="Predicted PM2.5 \n(in ppm)") +
  tm_shape(sample.poly) + tm_dots(size=0.1, alpha=0.5) +
  tm_legend(legend.outside=TRUE)

summary(lm.1)
summary(lm.2)
confint(lm.1, level=0.95)
confint(lm.2, level=0.95)
plot(fitted(lm.1),residuals(lm.1),xlab="fitted",ylab="residuals",main=expression(1^st~Order~Polynomial~Regression~Model))
plot(fitted(lm.2),residuals(lm.2),xlab="fitted",ylab="residuals",main=expression(2^nd~Order~Polynomial~Regression~Model))

#################################################
##Spatial Interpolation with Kriging
# Define the 1st order polynomial equation
f.1 <- as.formula(value ~ X + Y)
#f.2 <- as.formula(value ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))
var.smpl.s <- variogram(f.1, sample.poly, cloud = FALSE) #, cutoff=1000000, width=89900)
var.smpl.s.pt <- variogram(f.1, sample.spdf, cloud = FALSE)
#var.smpl.f1 <- variogram(f.1, pm.income.poly, cloud = FALSE)
#var.smpl <- variogram(f.1, sample.poly, cloud = FALSE)

#plot(variogramLine(vgm(1, "Mat", 1, kappa = 4), 10), type = 'l')
###Fitting variogram models in gstat
###vgm can take a set of models, in which case the best fitting is returned
fit.variogram(var.smpl.s, vgm(c("Exp")))
fit.variogram(var.smpl.s, vgm(c("Sph")))
fit.variogram(var.smpl.s, vgm(c("Gau")))
#fit.variogram(var.smpl.f1, vgm(c("Exp", "Sph")))
#fit.variogram(var.smpl.f1, vgm(c("Exp", "Gau", "Sph")))
#plot(var.smpl.f1, dat.fit, main="Spherical Model: Sill = 1.209449 & Range = 9.069939")

dat.fit.s  <- fit.variogram(var.smpl.s, fit.ranges = FALSE, fit.sills = FALSE,
                            vgm(psill=3.18526, model="Exp", range=19.01526, nugget=0))
plot(var.smpl.s, dat.fit.s, main="Exponential Model")

dat.fit.s  <- fit.variogram(var.smpl.s, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=2.065813, model="Sph", range=21.32746, nugget=0))
plot(var.smpl.s, dat.fit.s, main="Spherical Model")

dat.fit.s  <- fit.variogram(var.smpl.s, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=1.42987029, model="Gau", range=5.427872, nugget=0.05788712))
plot(var.smpl.s, dat.fit.s, main="Gaussian Model")

#temp <- locator(1)
#text(temp, "RMSE = 0.004075", col="grey")

#text(locator(1), "sill = 1.4e-05\nrange = 15")

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
#proj4string(pm.poly) <- proj4string(census.tracts)
#proj4string(pm.poly) = CRS(proj4string(census.tracts))
dat.krg.s <- krige( f.1, sample.poly, grd, dat.fit.s)
dat.krg.ss <- gstat::krige( f.1, sample.poly, grd, dat.fit.s)
#dat.krg.s.pt <- krige( f.1, sample.spdf, grd, dat.fit.s.pt)
#dat.krg <- krige( f.1, pmsample, grd, dat.fit)
# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg.s)
r.m <- raster::mask(r, census.tracts)

# Plot the map
krg.map.s <- tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", midpoint = NA, 
            title="Predicted PM2.5 \n(in ppm)") +
  tm_shape(sample.poly) + tm_dots(size=0.1, alpha=0.5) +
  tm_legend(legend.outside=TRUE)
krg.map.s

rs <- raster(dat.krg.ss)
r.ms <- raster::mask(rs, census.tracts)

# Plot the map
krg.map.ss <- tm_shape(r.ms) + 
  tm_raster(n=10, palette="RdBu", midpoint = NA, 
            title="Predicted PM2.5 \n(in ppm)") +
  tm_shape(sample.poly) + tm_dots(size=0.1, alpha=0.5) +
  tm_legend(legend.outside=TRUE)
krg.map.ss

# Convert kriged surface to a variance raster object for clipping
r   <- raster(dat.krg.s, layer="var1.var")
r.m <- raster::mask(r, census.tracts)

krg.var.map.s <- tm_shape(r.m) + 
  tm_raster(n=7, palette ="Reds",
            title="Variance map \n(in squared ppm)") +
  tm_shape(sample.poly) + tm_dots(size=0.1, alpha=0.5) +
  tm_legend(legend.outside=TRUE) + 
  tm_layout(legend.text.size=0.9)
krg.var.map.s

# Convert kriged surface to a confidence raster object for clipping
r   <- sqrt(raster(dat.krg.s, layer="var1.var")) * 1.96
r.m <- raster::mask(r, census.tracts)

krg.ci.map.s <- tm_shape(r.m) + 
  tm_raster(n=7,
            title="95% CI map \n(in ppm)") +
  tm_shape(pmsample) + tm_dots(size=0.1, alpha=0.5) +
  tm_legend(legend.outside=TRUE) + 
  tm_layout(legend.text.size=0.9)
krg.ci.map.s
