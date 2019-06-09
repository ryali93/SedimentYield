dem        = raster(file.path(IMG, "topo/dem_18s_250.tif"))
FlowAcc    = raster("D:/TESIS/DATOS/DATA_FACTOR_LS/flow_accum.tif")

template   = raster("E:/TESIS/results/k.tif")

# shp        = read_sf("E:/TESIS/datos/shp/Cuenca_tesis_utm.shp")
# utm17      = "+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
# wgs84      = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# shp_reproj = st_transform(shp, crs=utm17)
# shp        = st_as_sf(shp_reproj)

Slope      = terrain(dem, 'slope', 'degrees')
Slope1     = Slope*0.01745
Resol      = res(dem)

# Resol: Resolución del DEM (en metros)
# Slope: Porcentaje de la pendiente (en grados)
# FlowAcc: Acumulación de flujo (calculado en el entorno ArcGis)

# Opcion 1: Jiang & Zheng (2008)
factorLS1 <- function(Resol, Slope) {
  LS = 1.07*((Resol/20)^0.28)*((Slope/10)^1.45)
  return(LS)
}

# Opcion 2: Arnoldus (1977)
factorLS2 <- function(Resol, Slope) {
  LS = ((Resol/22.1)^0.6)*((Slope1/9)^1.4)
  return(LS)
}

# Opcion 4: McCool (1989) 
factorLS3 <- function(FlowAcc, Slope) {
  B = (sin(Slope*0.01745)/0.0896)/(3*((sin(Slope*0.01745))^0.8)+0.56)
  m = B/(1+B)
  LS = (m + 1)*(((FlowAcc*Resol)/22.13)^m)*(sin(Slope*0.01745)/0.0899)^1.35
  return(LS)
}

# Opcion 5: Moore & Burch (1985)
# As: Area de captacion específica
# m: 0.4 - 0.6
# n: 1.2 - 1.3
# Slope: Porcentaje de la pendiente (en grados)
# FlowAcc: Acumulación de flujo

factorLS4 <- function(FlowAcc, Resol, Slope) {
  As = FlowAcc*Resol
  LS = ((As/22.13)^0.4)*(sin(Slope*0.01745)/0.0896)^1.3
  return(LS)
}


# Opcion 6: SAGA GIS
LS1 = raster("D:/TESIS/DATOS/DATA_FACTOR_LS/SAGA/LS_wischmeier.tif")
LS2 = raster("D:/TESIS/DATOS/DATA_FACTOR_LS/SAGA/LS_Desmet&Gobers.tif")
LS3 = raster("D:/TESIS/DATOS/DATA_FACTOR_LS/SAGA/LS_Moore&Nieber.tif")

output1 = "E:/TESIS/process/ls/ls1.tif"
output2 = "E:/TESIS/process/ls/ls2.tif"
output3 = "E:/TESIS/process/ls/ls3.tif"


LS1 = mask(crop(LS1, shp), shp)
LS1res = Resamplear(hasta = LS1, desde = template)
writeRaster(LS1res, output1, overwrite = T)

LS2 = mask(crop(LS2, shp), shp)
LS2res = Resamplear(hasta = LS2, desde = template)
writeRaster(LS2res, output2, overwrite = T)

LS3 = mask(crop(LS3, shp), shp)
LS3res = Resamplear(hasta = LS3, desde = template)
writeRaster(LS3res, output3, overwrite = T)



###############################################################

a = c(min(getValues(mask(crop(LS_Moore,cuenca_tesis),cuenca_tesis)),na.rm = T),max(getValues(mask(crop(LS_Moore,cuenca_tesis),cuenca_tesis)),na.rm = T),mean(getValues(mask(crop(LS_Moore,cuenca_tesis),cuenca_tesis)),na.rm = T),sd(getValues(mask(crop(LS_Moore,cuenca_tesis),cuenca_tesis)),na.rm = T))
b = c(min(getValues(mask(crop(LS_Desmet,cuenca_tesis),cuenca_tesis)),na.rm = T),max(getValues(mask(crop(LS_Desmet,cuenca_tesis),cuenca_tesis)),na.rm = T),mean(getValues(mask(crop(LS_Desmet,cuenca_tesis),cuenca_tesis)),na.rm = T),sd(getValues(mask(crop(LS_Desmet,cuenca_tesis),cuenca_tesis)),na.rm = T))
c = c(min(getValues(mask(crop(LS_Wischmeier,cuenca_tesis),cuenca_tesis)),na.rm = T),max(getValues(mask(crop(LS_Wischmeier,cuenca_tesis),cuenca_tesis)),na.rm = T),mean(getValues(mask(crop(LS_Wischmeier,cuenca_tesis),cuenca_tesis)),na.rm = T),sd(getValues(mask(crop(LS_Wischmeier,cuenca_tesis),cuenca_tesis)),na.rm = T))
d = c(min(getValues(mask(crop(LS_Jiang,cuenca_tesis),cuenca_tesis)),na.rm = T),max(getValues(mask(crop(LS_Jiang,cuenca_tesis),cuenca_tesis)),na.rm = T),mean(getValues(mask(crop(LS_Jiang,cuenca_tesis),cuenca_tesis)),na.rm = T),sd(getValues(mask(crop(LS_Jiang,cuenca_tesis),cuenca_tesis)),na.rm = T))
e = c(min(getValues(mask(crop(LS_Arnoulds,cuenca_tesis),cuenca_tesis)),na.rm = T),max(getValues(mask(crop(LS_Arnoulds,cuenca_tesis),cuenca_tesis)),na.rm = T),mean(getValues(mask(crop(LS_Arnoulds,cuenca_tesis),cuenca_tesis)),na.rm = T),sd(getValues(mask(crop(LS_Arnoulds,cuenca_tesis),cuenca_tesis)),na.rm = T))

f = round(c(d,e),2)

