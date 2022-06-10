library(smooth)
library(forecast)
### Leer datos ####

df=read.csv("serie_ejemplos_excel.csv",sep=";",dec=",")
names(df)="valor" ## corregir nombre variable

##### medias móviles

y=ts(df$valor, start=1950, frequency = 1)

ma_y=sma(y,h=5,silent = F,interval=T, level=0.80) ## se ajusta un modelo de medias móviles
ma_y2=sma(y,h=5,silent = F,interval=T, level=0.90,order=6) ## se ajusta un modelo de medias móviles


summary(ma_y) 
summary(ma_y2)
ma_y$forecast

