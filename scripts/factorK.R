Sand = raster(file.path(IMG, "soil/sand_250.tif"))
Clay = raster(file.path(IMG, "soil/clay_250.tif"))
Silt = raster(file.path(IMG, "soil/silt_250.tif"))
Orgc = raster(file.path(IMG, "soil/orgc_250.tif"))
Estr = raster(file.path(IMG, "soil/estr_250.tif"))
Perm = raster(file.path(IMG, "soil/perm_250.tif"))
MO <- Orgc * 0.172

# shp   = read_sf(file.path(SHP, "Cuenca_tesis_utm.shp"))
# wgs84 = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# shp_reproj = st_transform(shp, crs=wgs84)
# 
# template = raster("E:/TESIS/results/k.tif")


# Opcion 1: Wischmeier and Smith (1978)
factorK1 <- function(Sand, Clay, Silt, MO, Estr, Perm) {
  K1 = 2.77 * (10 ^ -5) * (((Silt * Sand / 100) * (100 - Clay)) ^ 1.14)
  # ?????? (Clay+Sand), ¿por qué usa limo mas arena muy fina?
  K2 = (12 - MO) / 10
  K = K1 * K2 + 0.043 * (Estr - 2) + 0.033 * (Perm - 3)
  return(K)
}

# Opcion 2: Wischmeier and Smith (1978)
factorK2 <- function(Sand, Clay, Silt, MO, Estr, Perm) {
  M = (Silt + Sand) * (100 - Clay)
  K = (2.1 * (10 ^ -4) * (12 - (MO / 10)) * (M ^ 1.14) + 3.25 * (Estr - 2) + 2.3 * (Perm - 3)) / 100
  return(K)
  # Algunas fuentes modifican esta ecuación
  # 1. Panagos (2014) multiplica el resultado con 0.1317, para adecuarlo al SI-unit
  # 2. K. Zhang (2015) cambia la división de 100 a 759, para adecuarlo al SI-unit
}

# Opcion 3: Chaves et. al. (1996)
factorK3 <- function(Sand, Clay, Silt, MO, Estr, Perm) {
  K <- (0.00043 * ((Sand * Silt / 100) + Silt) / (Orgc / 10)) + 0.000437 * Sand + 0.000863 * Silt
  return(K)
}

# Opcion 4: SATEEC, Williams (1975)
factorK4 <- function(Sand, Clay, Silt, MO, Estr, Perm) {
  SN1 = 1 - Sand / 100
  K <- (0.2 + 0.3 * exp(-0.0256 * Sand * (1 - (Silt / 100)))) * (1 - (0.25 * Clay / (Clay + exp(3.72 - 2.95 * Clay)))) * (1 - (0.7 * SN1 / (SN1 + exp(-5.51 + 22.9 * SN1))))
  return(K)
  # para resultados en el SI, multiplicar entre 0.1317
}

# Opcion 5: Zhu et. al. (2014) ##### Utilizado en Tesis
factorK5 <- function(Sand, Clay, Silt, MO, Estr, Perm) {
  SN1 = 1 - Sand / 100
  K = (0.2 + 0.3 * exp(-0.0256 * Sand * (1 - (Silt / 100)))) * ((Silt / (Clay + Silt)) ^ 0.3) * (1 - (0.25 * (Orgc / 10) / ((Orgc / 10) + exp(3.72 - 2.95 * (Orgc / 10) )))) * (1 - (0.7 * SN1 / (SN1 + exp(-5.51 + 22.9 * SN1))))
  return(K)
  # para resultados en el SI, multiplicar entre 0.1317
}

###############################################################
# Main
FactorK <- function(pathOutput){
  Nfun <- substr(pathOutput, nchar(pathOutput)-1, nchar(pathOutput))
  print(paste0("Funcion: ", Nfun))
  
  if(Nfun == "K1"){Kfactor = factorK1(Sand, Clay, Silt, MO, Estr, Perm)}
  else if(Nfun == "K2"){Kfactor = factorK3(Sand, Clay, Silt, MO, Estr, Perm)}
  else if(Nfun == "K3"){Kfactor = factorK3(Sand, Clay, Silt, MO, Estr, Perm)}
  else if(Nfun == "K4"){Kfactor = factorK4(Sand, Clay, Silt, MO, Estr, Perm)}
  else if(Nfun == "K5"){Kfactor = factorK5(Sand, Clay, Silt, MO, Estr, Perm)}

  K = raster::mask(Kfactor, st_as_sf(shp_reproj))
  K_resample = Resamplear(K, template)
  writeRaster(K_resample, paste0(pathOutput, "/", Nfun, ".tif"), overwrite=TRUE)
}

FactorK("D:/TESIS/DATOS/DATA_FACTOR_K/K1")
FactorK("D:/TESIS/DATOS/DATA_FACTOR_K/K2")
FactorK("D:/TESIS/DATOS/DATA_FACTOR_K/K3")
FactorK("D:/TESIS/DATOS/DATA_FACTOR_K/K4")
FactorK("D:/TESIS/DATOS/DATA_FACTOR_K/K5")