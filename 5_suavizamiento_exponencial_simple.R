library(forecast)
library(fpp)



## paquete para extraer serie de tiempo ausair


df=read.csv("serie_ejemplos_excel.csv",sep=";",dec=",")
names(df)="valor" ## corregir nombre variable

##### medias móviles

y=ts(df$valor, start=1950, frequency = 1)

se=ses(y,h=5,interval=c(0.95,0.85))

plot(se)
summary(se)

######## pronostico para airmiles

serie=airmiles

plot(airmiles)

se_air=ses(airmiles)
ma_air=sma(airmiles,silent = F,interval=T)

summary(se_air)
plot(se_air)


#####modelo SE de holt

se_h=holt(airmiles)

plot(se_h)

summary(se_h)



#####SE holt_winters #######


austourists

frequency(austourists)
start(austourists)
plot(austourists)

aus=window(austourists,start=c(2007,1))

plot(aus)

par(mfrow=c(2,1))
mod=hw(austourists) ### ajustar holt y winters aditivo
plot(mod)

mod2=hw(austourists,seasonal="multiplicative") ## ajustar holt y winters multiplicativo
plot(mod2)

summary(mod)
summary(mod2)


