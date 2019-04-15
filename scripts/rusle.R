###############################################################
###########################  RUSLE ############################
###############################################################
library(raster)
library(sf)
###############################################################

shp <- read_sf("E:/TESIS/datos/shp/Cuenca_tesis_utm.shp")
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
shp_reproj = st_transform(shp, crs=wgs84)
shp = st_as_sf(shp_reproj)

K  = raster("E:/TESIS/process/k/FactorK.tif")
anos = c(1984:2016)
for(ano in anos){
  pathR = paste0("E:/TESIS/process/r/R_", ano, ".tif")
  R = raster(pathR)
  for(m in c(2:6)){
    pathC = paste0("E:/TESIS/process/c/C", m, "/C", m, "_", ano, ".tif")
    C = raster(pathC)
    for(n in c(1:3)){
      ls = paste0("E:/TESIS/process/ls/ls", n, ".tif")
      LS = raster(ls)
      rusle = R*K*C*LS
      pathRusle = paste0("E:/TESIS/process/rusle/r_c", m, "_ls", n, "/rusle_", ano,".tif")
      writeRaster(rusle, pathRusle, overwrite = T)
      print(pathRusle)
    }
  }
}

lista = c()
for(i in 1:length(list.files())){
  RUSLE_i = raster(list.files()[i])
  mk = mask(RUSLE_i,cuenca_tesis)
  a = sum(getValues(mk),na.rm=T)
  lista = c(lista,a)
  print(i+1980)
}

anos = c(1981:2015)
df = data.frame(anos,lista)
#write.csv(df,"C:/Users/PERSONAL/Desktop/TESIS/DATOS/EROSION/RUSLE/LS_Moore_sum.csv")
#write.csv(df,"C:/Users/PERSONAL/Desktop/TESIS/DATOS/EROSION/RUSLE/LS_Desmet_sum.csv")
#write.csv(df,"C:/Users/PERSONAL/Desktop/TESIS/DATOS/EROSION/RUSLE/LS_Wischmeier_sum.csv")
#write.csv(df,"C:/Users/PERSONAL/Desktop/TESIS/DATOS/EROSION/RUSLE/LS_Jiang_sum.csv")
#write.csv(df,"C:/Users/PERSONAL/Desktop/TESIS/DATOS/EROSION/RUSLE/LS_Arnoulds_sum.csv")


r_c6_ls1 = raster("E:/TESIS/process/rusle/r_c6_ls1/rusle_2005.tif")
r_c5_ls3 = raster("E:/TESIS/process/rusle/r_c5_ls3/rusle_2005.tif")
r_c5_ls2 = raster("E:/TESIS/process/rusle/r_c5_ls2/rusle_2005.tif")
r_c5_ls1 = raster("E:/TESIS/process/rusle/r_c5_ls1/rusle_2005.tif")
r_c4_ls3 = raster("E:/TESIS/process/rusle/r_c4_ls3/rusle_2005.tif")
r_c4_ls2 = raster("E:/TESIS/process/rusle/r_c4_ls2/rusle_2005.tif")
r_c4_ls1 = raster("E:/TESIS/process/rusle/r_c4_ls1/rusle_2005.tif")
r_c3_ls3 = raster("E:/TESIS/process/rusle/r_c3_ls3/rusle_2005.tif")
r_c3_ls2 = raster("E:/TESIS/process/rusle/r_c3_ls2/rusle_2005.tif")
r_c3_ls1 = raster("E:/TESIS/process/rusle/r_c3_ls1/rusle_2005.tif")
r_c2_ls3 = raster("E:/TESIS/process/rusle/r_c2_ls3/rusle_2005.tif")
r_c2_ls2 = raster("E:/TESIS/process/rusle/r_c2_ls2/rusle_2005.tif")
r_c2_ls1 = raster("E:/TESIS/process/rusle/r_c2_ls1/rusle_2005.tif")
r_c6_ls3 = raster("E:/TESIS/process/rusle/r_c6_ls3/rusle_2005.tif")
r_c6_ls2 = raster("E:/TESIS/process/rusle/r_c6_ls2/rusle_2005.tif")




delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

df = data.frame(getValues(r_c6_ls1), 
           getValues(r_c5_ls3), 
           getValues(r_c5_ls2), 
           getValues(r_c5_ls1), 
           getValues(r_c4_ls3), 
           getValues(r_c4_ls2), 
           getValues(r_c4_ls1), 
           getValues(r_c3_ls3), 
           getValues(r_c3_ls2), 
           getValues(r_c3_ls1), 
           getValues(r_c2_ls3), 
           getValues(r_c2_ls2), 
           getValues(r_c2_ls1), 
           getValues(r_c6_ls3), 
           getValues(r_c6_ls2))

df2=delete.na(df)

corrplot(cor(df2), method = "circle")
corrplot.mixed(cor(df2))
