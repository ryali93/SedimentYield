###############################################################
#######################  SEDIMENT YIELD #######################
###############################################################
library(sf)
library(raster)

BDIR_RUSLE = "E:/TESIS/process/rusle"
BDIR_SY    = "E:/TESIS/process/sy"
BDIR_SDR   = "E:/TESIS/process/sdr"
trT = c()
for(t in list.files(BDIR_SDR)){
  tr = file.path(BDIR_SDR, t)
  print(tr)
  trT = c(trT, tr)
}


for(i in list.files(BDIR_RUSLE)){
  print(i)
  for(sdrFile in trT){
    # Crear directorios
    path = paste0(i, "_", substr(sdrFile, 22, 25))
    path = file.path(BDIR_SY, path)
    dir.create(path)
    # Variable sdr
    sdr = raster(sdrFile)
    # Extraer Rusle
    pathRusle = file.path(BDIR_RUSLE, i)
    setwd(pathRusle)
    for(ruslefile in list.files(pathRusle)){
      rusle = raster(ruslefile)
      sy = rusle * sdr
      pathOut = file.path(path, paste0("sy_", substr(ruslefile, 7, 10), ".tif"))
      writeRaster(sy, pathOut, overwrite=T)
      # Print
      print(pathOut)
    }
  }
}


ts = seq(as.Date("1981-01-01"), as.Date("2016-01-01"), by="year")
df = data.frame(ts)

syFiles = list.files(BDIR_SY)
for(x in syFiles){
  print(x)
  syProd = file.path(BDIR_SY, x)
  setwd(syProd)
  lista = c()
  for(sy in list.files(syProd)){
    a = sum(getValues(raster(sy)),na.rm=T)
    lista = c(lista, a)
    print(sy)
  }
  lista = data.frame(lista)
  names(lista) = x
  df = cbind(df, lista)
}

write.csv(df, "E:/TESIS/process/results.csv")
