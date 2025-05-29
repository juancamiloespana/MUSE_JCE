

### librerias

library(forecast) ## los graficos los modelos
library(urca) ### para prueba kpss

### cargar datos ####

url="https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/refs/heads/master/data/arima_ej1.csv"

datos=read.csv(url)

#### convertir a serie de tiempo ####

ts_flujo= ts(datos$flujo, start=c(1990,2), frequency= 12)
end(ts_flujo)

### separar entrenamiento y evaluación

ts_flujo_train=window(ts_flujo, end=c(2005,11))

ts_flujo_test=window(ts_flujo, start=c(2005,12)) #### solo se utiliza al final



#### analizar graficamente
autoplot(ts_flujo_train)
## tiene tendencia: requiere d>0
## tiene varianza constante entonces lamdba= 1 es decir no se usa lambda
### No estacionalidad
Acf(ts_flujo, lag.max=60)

## comprobar que no tiene estacionalidad
## comprobar si la frecuencia es correcta



###### diferenciaciones no estacionales

D=nsdiffs(ts_flujo_train) ## para confirmar que no tiene estacionalidad

d= ndiffs(ts_flujo_train)

ts_diff= diff(ts_flujo_train, differences=d)

autoplot(ts_diff)
Acf(ts_diff, lag.max = 60)

summary(ur.kpss(ts_diff)) ## las diferenciaciones funcionaron

par(mfrow=c(1,2))

Acf(ts_diff)
Pacf(ts_diff)


###### ajustar modelo 


#p= 3
#d= 2
#q= 0


modelo_320= Arima(ts_flujo_train, order=c(3,2,0))
summary(modelo_320)

### AIC 515.57
### MAE 0.73

modelo_auto=auto.arima(ts_flujo_train,stepwise = F, trace=T, seasonal=F, approximation=F, max.order= 8, ic="aic")

summary(modelo_auto)

### AIC 515.21
##mae 0.73

checkresiduals(modelo_320)
checkresiduals(modelo_auto)

### se selecciona el modelo de procedimiento manual que tiene casi el mismo desempeño que el automatico

#### pronosticos diciembre de 2006

prons=forecast(modelo_320,h= 13, level=c(0.8,0.9))

plot(prons)
### desempeño en entrenamiento: indicadores del summmary, que están basados en el error
### RMSE= 0.9272147
### MAE: 0.7307052


### desempeño en evaluación

ts_flujo_test

pron_test=forecast(modelo_320,h=12) ### pronostico de fechas que coincidan con datos de test

library(Metrics) ### para indicadores


mae(ts_flujo_test, pron_test$mean)
rmse(ts_flujo_test, pron_test$mean)

mean(ts_flujo)
