###############################################################
##########################  FACTOR R ##########################
###############################################################
library(ncdf4)
library(xts)
library(zoo)
library(rts)

setwd("C:/Users/PERSONAL/Desktop/TESIS/DATOS/PP_PISCO2")
PISCO_PREC <- brick("PiscoMENSUALfinal19812015.nc")

#Función Factor R
R_USLE = function(ppmensual,ppanual){
  pm = raster(ppmensual)
  pa = raster(ppanual)
  1.735*10^(1.5*(log10((pm^2)/pa))-0.08188)
}


# Crear directorios
directorio="C:/Users/PERSONAL/Desktop/TESIS/DATOS/PP_PISCO2/"
for(i in 1981:2015){
  dir.create(paste0(directorio,i))
}
# directorio="C:/Users/PERSONAL/Desktop/TESIS/DATOS/DATA_FACTOR_R_2/"
for(i in 1:420)
{
  mes = seq(as.Date("1981/1/1"),as.Date("2015/12/1"),"month")
  año = substr(mes[i],1,4)
  writeRaster(PISCO_PREC[[i]],paste0("C:/Users/PERSONAL/Desktop/TESIS/DATOS/PP_PISCO2/",año,"/PP_",substr(mes[i],1,7),".tif"))
}
# Sumatoria para PP anuales
directorio="C:/Users/PERSONAL/Desktop/TESIS/DATOS/PP_PISCO2/"
for(año in 1981:2015)
{
  st<-stack(list.files(paste0(directorio,año,"/"),pattern = "*.tif",all.files = T,full.names = T))
  st_sum<-sum(st)
  writeRaster(st_sum,paste0(directorio,"ANUAL/","PPan_",año,".tif"),overwrite=T)
}

# Factor R
directorio="C:/Users/PERSONAL/Desktop/TESIS/DATOS/PP_PISCO2/"
salida="C:/Users/PERSONAL/Desktop/TESIS/DATOS/DATA_FACTOR_R_2/"
for(año in 1981:2015)
{
  Pann = paste0("C:/Users/PERSONAL/Desktop/TESIS/DATOS/PP_PISCO2/ANUAL/PPan_",año,".tif")
  c = c()
  for(mes in c("01","02","03","04","05","06","07","08","09","10","11","12"))
  {
    PPmens = paste0(directorio,año,"/PP_",año,"-",mes,".tif")
    Ri = R_USLE(PPmens,Pann)
    c = append(c,Ri)
    st1 = stack(c)
    print(mes)
  }
  R_Rusle_v2 = sum(st1)
  writeRaster(R_Rusle_v2,paste0(salida,"Factor_R_",año,".tif"),overwrite=T )
  print(año)
}

# Factor R - Clip
setwd("C:/Users/PERSONAL/Desktop/TESIS/DATOS/DATA_FACTOR_R_2/")
for(i in 1:length(list.files())){
  Ri = raster(list.files()[i])
  Ri_res<-resample(Ri,FactorK)
  Ri_clip = crop(Ri_res,Area)
  writeRaster(Ri_clip,paste0("C:/Users/PERSONAL/Desktop/TESIS/DATOS/RUSLE/FACTOR_R2_CLIP/FactorR_",i+1980,".tif"),overwrite=T)
}

