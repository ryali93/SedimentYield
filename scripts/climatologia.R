library(raster)
library(ggplot2)
library(reshape2)
library(hydroTSM)

ppm = brick("E:/TESIS/datos/img/PiscoMENSUALfinal19812015.nc")
tmax = brick("E:/BASE_DATOS/PISCO/PISCO-TxM-v1.0.nc")
tmin = brick("E:/BASE_DATOS/PISCO/PISCO-TnM-v1.0.nc")
ppm  = mask(crop(ppm, shp), shp)
tmax = mask(crop(tmax, shp), shp)
tmin = mask(crop(tmin, shp), shp)

datosVariable = function(variable){
  vec = c()
  for(i in 1:nlayers(variable)){
    layer = mean(getValues(variable[[i]]), na.rm=T)
    vec = c(vec, layer)
    print(i)
  }
  return(vec)
}


# Climatologia mensual
ppmVec = datosVariable(ppm)
tmaxVec = datosVariable(tmax)
tminVec = datosVariable(tmin)

ts = seq(as.Date("1981-01-01"), as.Date("2015-12-01"), by = "month")
temp = data.frame(ts, tmaxVec, tminVec, ppmVec)

meltdf <- melt(temp, id="ts")
ggplot(temp, aes(x=ts)) + 
  geom_line(aes(y = tmaxVec, color="Temp. Máxima (°C)"), size=1.2)+
  geom_line(aes(y = tminVec, color="Temp. Mínima (°C)"), size=1.2)+
  geom_bar(aes(y=..ppmVec../15), width = 40, alpha=0.5)+
  scale_y_continuous(breaks = seq(0, 50, by=5), sec.axis = sec_axis(~.*15, name = "Precipitación (mm)", breaks = seq(0, 500, by=50)))+
  scale_x_date(breaks = ts)+
  scale_colour_manual(values = c("red", "blue"))+
  xlab("Años") + ylab("Temperatura (°C)")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Climtatología mensual del área de estudio")+
  theme(legend.position="bottom")+
  labs(color = "")



# Climatologia anual
ppmVecAnn = monthly2annual(data.frame(ts, ppmVec), FUN = sum)
tmaxVecAnn = monthly2annual(data.frame(ts, tmaxVec), FUN = mean)
tminVecAnn = monthly2annual(data.frame(ts, tminVec), FUN = mean)

ts = seq(as.Date("1981-01-01"), as.Date("2015-12-01"), by = "+3 year")

temp = data.frame(ts, tmaxVecAnn, tminVecAnn, ppmVecAnn)
meltdf <- melt(temp, id="ts")
ggplot(temp, aes(x=ts)) + 
  geom_line(aes(y = tmaxVecAnn, color="Temp. Máxima (°C)"), size=1.2)+
  geom_line(aes(y = tminVecAnn, color="Temp. Mínima (°C)"), size=1.2)+
  geom_bar(aes(y=..ppmVecAnn../30), width = 365, alpha=0.5)+
  scale_y_continuous(breaks = seq(0, 50, by=5), sec.axis = sec_axis(~.*30, name = "Precipitación (mm)", breaks = seq(0, 1500, by=200)))+
  scale_x_date(breaks = ts)+
  scale_colour_manual(values = c("red", "blue"))+
  xlab("Años") + ylab("Temperatura (°C)")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Climtatología anual del área de estudio")+
  theme(legend.position="bottom")+
  labs(color = "")
