###############################################################
############################  SDR #############################
###############################################################
library(raster)
library(sf)
library(data.table)
###############################################################

shp        <- read_sf("E:/TESIS/datos/shp/Cuenca_tesis_utm.shp")
utm17      <- "+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
wgs84      <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
shp_reproj <- st_transform(shp, crs=utm17)
shp        <- st_as_sf(shp_reproj)

dem        <- raster("D:/TESIS/DATOS/DATA_FACTOR_LS/dem_fill.tif")
slope      <- terrain(dem, 'slope', 'degrees')
Resol      <- res(dem)

template   <- raster("E:/TESIS/results/k.tif")

area       <- shp$AREA_KM/1000000


# Opcion 1: SDR (Vanoni, 1975)
SDR1 <- function(area) {
  sdr = 0.4270*(area^-0.125)
  return(sdr)
}

# Opcion 2: SDR (Boyce, 1975)
SDR2 <- function(area) {
  sdr = 0.5656*(area^-0.11)
  return(sdr)
}

# Opcion 3: SDR (USDA, 1972)
SDR3 <- function(area) {
  sdr = 0.3750*(area^-0.2382)
  return(sdr)
}

# Opcion 4: SDR (SATEEC, 2016)  
SDR4 <- function(slope) {
  sdr = 0.627*(slope^-0.403)
  return(sdr)
}


###############################################################
# Main
SDR <- function(pathOutput){
  Nfun <- substr(pathOutput, nchar(pathOutput)-3, nchar(pathOutput))
  print(paste0("Funcion: ", Nfun))
  
  if(Nfun == "sdr1"){SDRfactor = SDR1(area)}
  else if(Nfun == "sdr2"){SDRfactor = SDR2(area)}
  else if(Nfun == "sdr3"){SDRfactor = SDR3(area)}
  else if(Nfun == "sdr4"){SDRfactor = SDR4(slope)}

  SDR = mask(SDRfactor, shp)
  SDR_resample = Resamplear(SDR, template)
  writeRaster(SDR_resample,
              paste0(pathOutput, ".tif"),
              overwrite=TRUE)
}

SDR("E:/TESIS/process/sdr/sdr1")
SDR("E:/TESIS/process/sdr/sdr2")
SDR("E:/TESIS/process/sdr/sdr3")
SDR("E:/TESIS/process/sdr/sdr4")

slope1 = mask(crop(slope, shp), shp)
slope1 = Resamplear(slope1, template)
writeRaster(slope1, "E:/TESIS/datos/img/sdr/R_slope.tif")

sdr = SDR4(slope1)
sdr1 = Resamplear(sdr, template)
writeRaster(sdr1, "E:/TESIS/process/sdr/sdr4.tif", overwrite=TRUE)

sdrW   = raster("D:/TESIS/DATOS/SDR/SDR_Williams_3.tif")


mc = shapefile("E:/TESIS/datos/shp/tmp/microcuencasWS.shp")
mc$areakm2 = area(mc)/1000000
r <- crop(sdr1, extent(mc))
zone <- rasterize(mc, r, field="areakm2", dataType = "INT1U")
writeRaster(zone, "E:/TESIS/datos/img/R_WS_AREAS.tif")



zone.in   = "E:/TESIS/datos/shp/WS_Slope.shp"
raster.in = "D:/TESIS/DATOS/DATA_FACTOR_LS/dem_fill.tif"
shp.out   = "E:/TESIS/datos/shp/GPO_WS_RASTER.shp"

shp <- ZonalPipe(zone.in, raster.in, stat="mean")
writeOGR(shp, dsn = 'E:/TESIS/datos/shp', layer ='WS_SDR', driver = 'ESRI Shapefile')


r <- crop(slope1, extent(shp))
zone <- rasterize(shp, r, field="R_SLOPE_1_mean", dataType = "INT1U")
writeRaster(zone, "E:/TESIS/datos/img/R_WS_SLOPE.tif")

slope = raster("E:/TESIS/datos/img/R_WS_SLOPE.tif")
area  = raster("E:/TESIS/datos/img/R_WS_AREAS.tif")
slope = Resamplear(slope, template)
area  = Resamplear(area, template)

sdr = SDR1(area)
writeRaster(sdr, "E:/TESIS/process/sdr/sdr1.tif")
sdr = SDR2(area)
writeRaster(sdr, "E:/TESIS/process/sdr/sdr2.tif")
sdr = SDR3(area)
writeRaster(sdr, "E:/TESIS/process/sdr/sdr3.tif")
sdr = SDR4(slope)
writeRaster(sdr, "E:/TESIS/process/sdr/sdr4.tif")