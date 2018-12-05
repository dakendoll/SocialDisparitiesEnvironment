######SUBSAMPLING FOR SPATIAL INTERPOLATION
# take a random sample of size 50 from a dataset mydata 
# sample with replacement
#df[sample(nrow(df), 3), ]
#census.tracts.t = spTransform(census.tracts, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
pmsample <- pm.spdf[sample(1:nrow(pm.spdf), 100, replace=TRUE),]
pmsample <- na.omit(pmsample)
proj4string(pmsample) <- proj4string(census.tracts)
head(pmsample)
plot(pmsample)

# Run the regression model
lm.1 <- lm( f.1, data=pmsample)

# Use the regression model output to interpolate the surface
dat.1st <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.1, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.1st)
r.m <- mask(r, census.tracts)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", midpoint = NA, 
            title="Predicted PM25 \n(in ppm)") +
  tm_shape(pmsample) + tm_dots(size=0.1, alpha=0.5) +
  tm_legend(legend.outside=TRUE)

# Define the 2nd order polynomial equation
f.2 <- as.formula(value ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))

# Run the regression model
lm.2 <- lm( f.2, data=pmsample)

# Use the regression model output to interpolate the surface
dat.2nd <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.2, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.2nd)
r.m <- mask(r, census.tracts)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", midpoint = NA,
            title="Predicted PM25 \n(in ppm)") +
  tm_shape(pmsample) + tm_dots(size=0.1, alpha=0.5) +
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
#f.1 <- as.formula(value ~ X + Y)
f.2 <- as.formula(value ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))
var.smpl.s <- variogram(f.2, pmsample, cloud = FALSE) #, cutoff=1000000, width=89900)
#var.smpl.f1 <- variogram(f.1, pm.income.poly, cloud = FALSE)
#var.smpl <- variogram(f.1, pmsample, cloud = FALSE)

#plot(variogramLine(vgm(1, "Mat", 1, kappa = 4), 10), type = 'l')
###Fitting variogram models in gstat
###vgm can take a set of models, in which case the best fitting is returned
fit.variogram(var.smpl.s, vgm(c("Exp", "Sph")))
fit.variogram(var.smpl.s, vgm(c("Exp", "Gau", "Sph")))
#fit.variogram(var.smpl.f1, vgm(c("Exp", "Sph")))
#fit.variogram(var.smpl.f1, vgm(c("Exp", "Gau", "Sph")))
#plot(var.smpl.f1, dat.fit, main="Spherical Model: Sill = 1.209449 & Range = 9.069939")

dat.fit.s  <- fit.variogram(var.smpl.s, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=0.5983995, model="Sph", range=9.354014, nugget=0))
plot(s.var.smpl, dat.fit.s, main="Spherical Model")

dat.fit.s  <- fit.variogram(var.smpl.s, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=0.53503423, model="Gau", range=3.847494, nugget=0.04183449))
plot(s.var.smpl, dat.fit.s, main="Gaussian Model")
#temp <- locator(1)
#text(temp, "RMSE = 0.004075", col="grey")

#text(locator(1), "sill = 1.4e-05\nrange = 15")

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
#proj4string(pm.poly) <- proj4string(census.tracts)
#proj4string(pm.poly) = CRS(proj4string(census.tracts))
dat.krg.s <- krige( f.2, pmsample, grd, dat.fit)
#dat.krg <- krige( f.1, pmsample, grd, dat.fit)
# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg.s)
r.m <- mask(r, census.tracts)

# Plot the map
krg.map.s <- tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", midpoint = NA, 
            title="Predicted PM2.5 \n(in ppm)") +
  tm_shape(pmsample) + tm_dots(size=0) +
  tm_legend(legend.outside=TRUE)
krg.map.s

# Convert kriged surface to a variance raster object for clipping
r   <- raster(dat.krg.s, layer="var1.var")
r.m <- mask(r, census.tracts)

krg.var.map.s <- tm_shape(r.m) + 
  tm_raster(n=7, palette ="Reds",
            title="Variance map \n(in squared ppm)") +
  tm_shape(pmsample) + tm_dots(size=0.1, alpha=0.5) +
  tm_legend(legend.outside=TRUE) + 
  tm_layout(legend.text.size=0.9)
krg.var.map.s

# Convert kriged surface to a confidence raster object for clipping
r   <- sqrt(raster(dat.krg.s, layer="var1.var")) * 1.96
r.m <- mask(r, census.tracts)

krg.ci.map.s <- tm_shape(r.m) + 
  tm_raster(n=7,
            title="95% CI map \n(in ppm)") +
  tm_shape(pmsample) + tm_dots(size=0.1, alpha=0.5) +
  tm_legend(legend.outside=TRUE) + 
  tm_layout(legend.text.size=0.9)
krg.ci.map.s
