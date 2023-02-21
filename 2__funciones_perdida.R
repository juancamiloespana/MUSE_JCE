### cargar paquetes

library(Metrics)
library(forecast)

### Simular datos pib ###

t=c(1:200)
pib =200 + 4*t + rnorm(200,mean=20,sd=20)
pib_ts=ts(pib,start=1820,frequency = 1) ##Convertir a TS

## graficar serie de tiempo
plot(pib_ts)

###Separación de datos train/test

pib_train=window(pib_ts,end=1980) ## datos para entrenamiento
pib_test=window(pib_ts,start=1981) ## datos para evaluación

t_train=c(1:161)
t_test= c(162:200)

### verificar separación correcta

length(pib_train)
length(t_train)

###Crear data.frame

datos_tiempo_train=data.frame("pib"=pib_train,"t"=t_train,"t2"=t_train^2)
datos_tiempo_test=data.frame("pib"=pib_test,"t"=t_test,"t2"=t_test^2)


###### Ajustar/entrenar modelo de regresión lineal 

mod1=lm(pib~t,data=datos_tiempo_train)
mod2=lm(pib~t2,data=datos_tiempo_train)

summary(mod1)
summary(mod2)

AIC(mod1)
AIC(mod2)

BIC(mod1)
BIC(mod2)

####### MAPE de evaluación y mape de entrenamiento ###

###mape Entrenamiento modelo 1
predichos=predict(mod1)
mape(pib_train,predichos)*100
mae(pib_train,predichos)

##mape entrenamiento modelo 2

predichos2=predict(mod2)
mape(pib_train,predichos2)*100
mae(pib_train,predichos2)

###mape evaluación modelo 1

predichos_e=predict(mod1,newdata = datos_tiempo_test)
mape(pib_test,predichos_e)*100
mae(pib_test,predichos_e)
###mape evaluación modelo 2

predichos_e2=predict(mod2, newdata=datos_tiempo_test)
mape(pib_test,predichos_e2)*100
mae(pib_test,predichos_e2)



#### supuestos para residuales:
## distribuyen normal con media 0
## tienen varianza constante
## son independientes

checkresiduals(mod2)

