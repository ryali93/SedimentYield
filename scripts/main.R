

path = "D:/TESIS/DATOS/EROSION/CLIP/FACTOR_R2_CLIP"
lista = list.files(path, full.names = T)
shp1 = spTransform(shp, proj4string(raster(x)))

i=1980
for(x in lista){
  i = i + 1
  R = mask(crop(raster(x), shp1), shp1)
  output = paste0("E:/TESIS/results/r/R_", i, ".tif")
  writeRaster(R, output, overwrite = T)
}



ls = raster("E:/TESIS/results/ls.tif")
c = raster("E:/TESIS/results/c.tif")
k = raster("E:/TESIS/results/k.tif")
dirR = "E:/TESIS/results/r"
listR = list.files(dirR, full.names = T)

i = 1980
for (x in listR){
  i = i+1
  r = raster(x)
  rusle = r*c*k*ls
  pathOut = paste0("E:/TESIS/results/rusle/rusle_", i, ".tif")
  writeRaster(rusle, pathOut)
}


dirSY = "D:/TESIS/DATOS/SYIELD/SY_DESMET2"
listSY = list.files(dirSY, full.names = T)
i = 1980
for (x in listSY){
  i = i+1
  sy = raster(x)
  syi = mask(crop(raster(x), shp1), shp1)
  pathOut = paste0("E:/TESIS/results/sy/SY_", i, ".tif")
  writeRaster(syi, pathOut)
}