library(forecast) ### modelos forecast
library(urca) ### estacionariedad



cod_suc = 21
ruta_sucs='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/caso_estudio/flujo_efectivo.csv'
df_sucs=read.csv(ruta_sucs) ### serie de tiempo de todas las sucursales
df_suc=df_sucs[df_sucs$suc ==cod_suc,] ###serie de tiempo de sucursal 17 (escogida al azar)

df_suc2=df_suc[df_suc$fechas<='2022-06-01',] # filtrar el flujo hasta la fecha del saldo
ts_suc2=ts(df_suc2$flujo_efe,start=c(1,5), frequency = 7) ### corresponde a estacionalidad de 7 días, inicia en semana 1 dia 2, cada7 dias cambia de semana

####graficar serie ####

autoplot(ts_suc2) ###tiene tendencia, varianza no constante
lambda=BoxCox.lambda(ts_suc2)### identifica el lambda
ts_sucbc=BoxCox(ts_suc2, lambda) ### esta función aplica la transformación

### comparar serie transformado
par(mfrow=c(1,2))
plot(ts_sucbc) 
plot(ts_suc2)

### no se utilizara transformacio porque no cambia mucho

ndiffs(ts_suc2) ### diferenciación identifica d=1
ts_suc_d=diff(ts_sucbc, differences = 1) ### se aplica diferencia sugerida
ts_suc_d2=diff(ts_suc2, differences = 1)

#### para comparar la serie diferenciada transformada, vs la que solo se diferencia
par(mfrow=c(1,2))
plot(ts_suc_d)
plot(ts_suc_d2)

###trabjar con serie no transformada porque no mejora mucho

Acf(ts_suc_d2)
Pacf(ts_suc_d2)


modelo=Arima(ts_suc2,order=c(1,1,2)) ### modelo (p=1, d=1, q=2)
summary(modelo)
mean(ts_suc2)

modelo2=Arima(ts_suc2,order=c(0,1,2))
summary(modelo2) ## da mejor el anterior

modelo3=auto.arima(ts_suc2, seasonal = F)
summary(modelo3)

library(forecast)
arima_ej1=arima.sim( list(order=c(2,1,0),ar=c(.3,0.2)),n=200)



arima_ej2=arima.sim( list(order=c(0,1,1),ma=.7),n=200)
autoplot(arima_ej2)


write.csv(arima_ej1, 'data\\arima_ej1.csv')
write.csv(arima_ej2, 'data\\arima_ej2.csv')

plot(arima_ej2)

library(forecast)

set.seed(5)
arima_ej1=arima.sim( list(order=c(0,0,2),ma=c(-.4,-0.2)),n=200)

par(mfrow=c(1,2))
acf(arima_ej1,lag.max = 40)
pacf(arima_ej1,lag.max = 40)

arima_ej1=arima.sim( list(order=c(0,1,0)),n=100)
Acf(arima_ej1)
Pacf(arima_ej1)


autoplot(arima_ej1)


