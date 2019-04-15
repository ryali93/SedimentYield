library(raster)
est = read.csv("E:/TESIS/docs/tablas/estaciones.csv", sep = ";", header = T)
coordinates(est) = ~LON+LAT
proj4string(est) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

shp = shapefile("E:/TESIS/datos/shp/Cuenca_tesis_utm.shp")
shp = spTransform(shp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))



setwd("E:/TESIS/datos/img/ppanual")
st = stack(list.files("E:/TESIS/datos/img/ppanual"))
ppmin = min(st)
ppmax = max(st)

PPmin = extract(ppmin, est)
PPmax = extract(ppmax, est)
df = cbind(est, PPmin, PPmax)



tmax = brick("E:/BASE_DATOS/PISCO/PISCO-TxM-v1.0.nc")
tmin = brick("E:/BASE_DATOS/PISCO/PISCO-TnM-v1.0.nc")

Tmax = mean(tmax)
Tmin = mean(tmin)
TMAX = extract(Tmax, est)
TMIN = extract(Tmin, est)

df = cbind(df, TMAX, TMIN)


install.packages("hydroTSM", dependencies = T)
library(hydroTSM)

ppmonth = daily2monthly(pp, sum, na.rm = TRUE)
ppmonth2 = as.data.frame(ppmonth)
ppanual = monthly2annual(ppmonth3, sum, na.rm=T)

anos = c("01-01-1960","01-01-1961","01-01-1962","01-01-1963","01-01-1964","01-01-1965","01-01-1966","01-01-1967","01-01-1968","01-01-1969","01-01-1970","01-01-1971","01-01-1972","01-01-1973","01-01-1974","01-01-1975","01-01-1976","01-01-1977","01-01-1978","01-01-1979","01-01-1980","01-01-1981","01-01-1982","01-01-1983","01-01-1984","01-01-1985","01-01-1986","01-01-1987","01-01-1988","01-01-1989","01-01-1990","01-01-1991","01-01-1992","01-01-1993","01-01-1994","01-01-1995","01-01-1996","01-01-1997","01-01-1998","01-01-1999","01-01-2000","01-01-2001","01-01-2002","01-01-2003","01-01-2004","01-01-2005","01-01-2006","01-01-2007","01-01-2008","01-01-2009","01-01-2010","01-01-2011","01-01-2012","01-01-2013","01-01-2014","01-01-2015")
anos = as.Date(anos, "%d-%m-%Y")

ppanual = as.data.frame(ppanual)
ppanual = cbind(anos, ppanual)
myColors <- c("#56ddc5", "#ff3db7", "#4699dd")
meltdf <- melt(ppanual, id="anos")
ggplot(meltdf, aes(x=variable, y=value, color=variable)) + 
  geom_boxplot()+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Boxplot de precipitación - Estaciones SENAMHI") +
  ylab("Precip. (mm)") + xlab("Estaciones")

