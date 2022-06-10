library(forecast)


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




