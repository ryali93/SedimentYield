#****FACTOR P****#
library(sp)
library(raster)
library(rgdal)
library(gdalUtils)
library(dplyr)

dem<-raster("C:/Users/PERSONAL/Desktop/TESIS/DATOS/DATA_FACTOR_LS/dem_fill.tif")
slope<-terrain(dem,'slope','degrees' )
hist(getValues(slope))
hist(getValues(Slope1))
median(getValues(Slope1),na.rm = T)
sd(getValues(Slope1),na.rm = T)
Slope1<-slope*0.01745*100

Slope1[Slope1>= 0 & Slope1 < 7] <- 0.55
Slope1[Slope1>= 7 & Slope1 < 11.3] <- 0.60
Slope1[Slope1>= 11.3 & Slope1 < 17.6] <- 0.80
Slope1[Slope1>= 17.6 & Slope1 < 26.8] <- 0.90
Slope1[Slope1>= 26.8] <- 0.10


writeRaster(Slope1,"C:/Users/PERSONAL/Desktop/TESIS/DATOS/DATA_FACTOR_P/FactorP2.tif",overwrite=T)
