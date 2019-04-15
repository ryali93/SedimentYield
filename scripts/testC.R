library(ggplot2)
library(reshape2)

# Grafico de boxplot del factor R
setwd("E:/TESIS/process/r_2/")
anos = seq(as.Date("1980-01-01"), as.Date("2015-01-01"), by = "year")
anos = as.data.frame(anos)

valano = c()
df2 = data.frame(c(1:65041))

for(i in list.files()){
  val = getValues(raster(i))
  val = na.omit(val)
  print(length(val))
  names(val) = substr(i, 3, 6)
  df2 = data.frame(df2, val, fix.empty.names = T)
  print(i)
}

names(df2) = anos
names(df2)[1] = "id"

meltdf <- melt(df2, id="id")
ggplot(meltdf, aes(x=variable, y=value, color=variable)) + 
  geom_boxplot()+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Boxplot del factor R anual") +
  ylab("Factor R") + xlab("Año")



# Grafico de boxplot del factor C
setwd("E:/TESIS/process/c/C2/")
anos = seq(as.Date("1981-01-01"), as.Date("2016-01-01"), by = "year")
a = c(1981:2016)
b = c(1981,1982,1983,1985)

anos = setdiff(a, b)

# anos = as.data.frame(anos)
# length(na.omit(getValues(raster(list.files()[15]))))

setwd("E:/TESIS/process/c/C6/")
cfac6 = c()
for(i in anos){
  path = paste0("C6_", as.character(i), ".tif")
  val = mean(getValues(raster(path)), na.rm=T)
  # val = na.omit(val)
  cfac6 = c(cfac6, val)
  print(i)
}

anos = as.Date(as.character(anos), "%Y")
cfac2 # Kniff(1999)
cfac4 # De Jong(1994)
cfac6 # Lin(2002)
cfact = data.frame(anos, cfac2, cfac4, cfac6)

names(cfact) = c("anos","Kniff(1999)", "De Jong(1994)", "Lin(2002)")

meltdf <- melt(cfact, id="anos")
ggplot(meltdf, aes(x=anos, y=value, color=variable)) + 
  geom_line(size=1)+
  geom_point(size=2)+
  theme_bw()+
  ggtitle("Serie de tiempo del factor C")+
  ylab("Factor C") + xlab("Años") +
  theme(legend.position="bottom", plot.title = element_text(hjust=0.5)) +
  scale_colour_discrete(name  = "") 






setwd("D:/TESIS/DATOS/DATA_FACTOR_C/NDVI")
st = stack(list.files())
st = mask(crop(st, shp), shp)
st1 = Resamplear(st, ROY_C)
boxplot(st1)


setwd("E:/TESIS/process/c/C2/")
stC2 = stack(list.files())

cfac2 = c()
anos = c(1984:2016)
for(i in anos){
  path = paste0("C2_", as.character(i), ".tif")
  val = mean(getValues(raster(path)), na.rm=T)
  cfac2 = c(cfac2, val)
  print(i)
}

setwd("D:/TESIS/DATOS/DATA_FACTOR_C/NDVI")
ndvi = c()
anos = c(1984:2016)
for(i in anos){
  path = paste0("NDVI_", as.character(i), ".tif")
  val = mean(getValues(raster(path)), na.rm=T)
  ndvi = c(ndvi, val)
  print(i)
}

anos = seq(as.Date("1984-01-01"), as.Date("2016-01-01"), by = "year")
anos = as.data.frame(anos)
dfC = data.frame(anos, ndvi, cfac2)
names(dfC) = c("anos", "NDVI", "Factor C")
meltdf <- melt(dfC, id="anos")
ggplot(meltdf, aes(x=anos, y=value, color=variable)) +
  geom_point() +
  geom_line(size=0.8)+
  theme_bw() +
  scale_x_date(breaks = ts) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Comparación entre el NDVI y el factor C") +
  ylab("Valor") + xlab("Años")+
  theme(legend.position="bottom")+
  labs(color = "")




R_WS_SLOPE = raster("E:/TESIS/datos/img/R_WS_SLOPE.tif")
plot(R_WS_SLOPE)
plot(0.627*(R_WS_SLOPE^-0.403))


(11^-0.403)*0.627
