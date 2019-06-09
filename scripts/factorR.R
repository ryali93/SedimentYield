PISCO_path  = file.path(NC, "PISCO_19812015.nc")
PISCO_brick = brick(PISCO_nc)

R_USLE = function(pm, pa) return(1.735*10^(1.5*(log10((pm^2)/pa))-0.08188))

n = 12
len_brick   = nlayers(PISCO_brick)
months_brick = split(1:len_brick, ceiling(seq_along(1:len_brick)/n))

RUSLE = c()
for (x in months_brick){
  PISCO_Ann = sum(PISCO_brick[[x]])
  rusle = sum(stack(lapply(x, function(mes) R_USLE(PISCO_brick[[mes]], PISCO_Ann))))
  RUSLE = c(RUSLE, rusle)
}
RUSLE = stack(RUSLE)

# for(i in 1:length(list.files())){
#   Ri = raster(list.files()[i])
#   Ri_res<-resample(Ri,FactorK)
#   Ri_clip = crop(Ri_res,Area)
#   writeRaster(Ri_clip,paste0("C:/Users/PERSONAL/Desktop/TESIS/DATOS/RUSLE/FACTOR_R2_CLIP/FactorR_",i+1980,".tif"),overwrite=T)
# }