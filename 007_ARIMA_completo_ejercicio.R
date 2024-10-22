
library(forecast)
library(urca) ### prueba de raiz unitaria sin valor p
library(tseries)## valor p prueba de raiz unitaria

url="https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/refs/heads/master/data/arima_ej1.csv"

datos=read.csv(url)

##convertir a serie de tiempo

ts_flujo= ts(datos$flujo, start=c(1990,2), frequency=12)

ts_cut=window(ts_flujo, end= c(2005,12))

autoplot(ts_cut)
Acf(ts_cut, lag.max=60) 

summary(ur.kpss(ts_cut))
kpss.test(ts_cut)

ndiffs(ts_cut)

###  la d del modelo rima seria 2

ts_cut_dif= diff(ts_cut, differences=2)

autoplot(ts_cut_dif)

summary(ur.kpss(ts_cut_dif))
kpss.test(ts_cut_dif)

#### para p y q Acf y Pacf


par(mfrow=c(1,2))
Acf(ts_cut_dif)
Pacf(ts_cut_dif)

###p = 3 y q=0

modelo1=Arima(ts_cut, order=c(3,2,0))
summary(modelo1)



