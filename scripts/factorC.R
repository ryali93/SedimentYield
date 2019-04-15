###############################################################
##########################  FACTOR C ##########################
###############################################################
library(raster)
library(sf)
###############################################################
# Utilizando cobertura de suelo
# Opcion 1:
input <- "C:/Users/PERSONAL/Desktop/TESIS/DATOS/DATA_FACTOR_C/land_use.tif"
output <- "C:/Users/PERSONAL/Desktop/TESIS/DATOS/DATA_FACTOR_C/FactorC.tif"

factorC1 <- function(rutaIn, rutaOut){
  landuse<-raster(rutaIn)
  landuse[landuse == 10] <- 0.63
  landuse[landuse == 20] <- 0.003
  landuse[landuse == 30] <- 0.09
  landuse[landuse == 40] <- 0.22
  landuse[landuse == 50] <- 0
  landuse[landuse == 60] <- 0
  landuse[landuse == 70] <- NA
  landuse[landuse == 80] <- 0
  landuse[landuse == 90] <- 0
  landuse[landuse == 100] <- 0
  landuse[landuse == 255] <- NA
  writeRaster(landuse, rutaOut, overwrite=T)
  return(raster(rutaOut))
}
C1 <- factorC1(input, output)

###############################################################
# Utilizando Landsat NDVI
pathInput <- "D:/TESIS/DATOS/DATA_FACTOR_C/NDVI"
# pathOutput <- "D:/TESIS/DATOS/DATA_FACTOR_C/C6"
shp <- read_sf("E:/TESIS/datos/shp/Cuenca_tesis_utm.shp")
year <- seq(1984, 2016)
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
shp_reproj = st_transform(shp, crs=wgs84)

template = raster("E:/TESIS/results/k.tif")

# Opción 2: Van de Kniff(1999)
factorC2 <- function(ndvi){
  C2 = exp(-2*(ndvi/(1-ndvi)))
  C2[C2==2] = 1
  return(C2)
}

# Opción 3:
# factorC3 <- function(ndvi){
#   C3 = 93.07466*ndvi + 8.79815
#   return(C3)
# }

# Opción 4: De Jong(1994)
factorC4 <- function(ndvi){
  C4 = 0.431 - 0.805*ndvi
  return(C4)
}

# Opción 5:
# factorC5 <- function(ndvi){
#   C5 = exp(-2*(ndvi/(1-ndvi)))
#   C5[C5 > 1] = NA
#   return(C5)
# }

# Opción 6: Lin(2002)
factorC6 <- function(ndvi){
  C6 = ((1-ndvi)/2)^(1+ndvi)
  return(C6)
}

###############################################################
# Main
FactorC <- function(pathOutput){
  Nfun <- substr(pathOutput, nchar(pathOutput)-1, nchar(pathOutput))
  print(paste0("Funcion: ", Nfun))
  listPath = list.files(pathInput, full.names = T)
  for(i in c(1:length(listPath))){
    img_i = raster(listPath[i])
    if(Nfun == "C2"){Cfactor = factorC2(img_i)}
    else if(Nfun == "C3"){Cfactor = factorC3(img_i)}
    else if(Nfun == "C4"){Cfactor = factorC4(img_i)}
    else if(Nfun == "C5"){Cfactor = factorC5(img_i)}
    else if(Nfun == "C6"){Cfactor = factorC6(img_i)}
    
    C = raster::mask(Cfactor, st_as_sf(shp_reproj))
    C_resample = Resamplear(C, template)
    writeRaster(C_resample,
                paste0(pathOutput, "/", Nfun, "_", year[i], ".tif"),
                overwrite=TRUE)
    print(year[i])
  }
}

FactorC("D:/TESIS/DATOS/DATA_FACTOR_C/C2")
FactorC("D:/TESIS/DATOS/DATA_FACTOR_C/C3")
FactorC("D:/TESIS/DATOS/DATA_FACTOR_C/C4")
FactorC("D:/TESIS/DATOS/DATA_FACTOR_C/C5")
FactorC("D:/TESIS/DATOS/DATA_FACTOR_C/C6")

# Factor_C = stack(list.files("D:/TESIS/DATOS/DATA_FACTOR_C/FACTORC6", 
#                          full.names =T ))
