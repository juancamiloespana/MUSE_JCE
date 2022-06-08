### cargar paquetes
library(tseries)
library(Metrics)
library(forecast)
library(car)



## graficar serie de tiempo
data(bev)
plot(bev)
length(bev) ##verificar longitud para mirar separación
print(bev)

###Separación de datos train/test

bev_train=window(bev,end=1800) ## datos para entrenamiento
bev_test=window(bev,start=1801) ## datos para evaluación

t_train=c(1:301)
t_test= c(302:370)

### verificar separación correcta

length(bev_train)
length(t_train)

length(bev_test)
length(t_test)
###Crear data.frame

datos_tiempo_train=data.frame("bev"=bev_train,"t"=t_train)
datos_tiempo_test=data.frame("bev"=bev_test,"t"=t_test)


###### Ajustar/entrenar modelo de regresión lineal 

mod1=lm(bev~t,data=datos_tiempo_train)


summary(mod1)
AIC(mod1)
BIC(mod1)


####### MAPE de evaluación y mape de entrenamiento ###

###mape Entrenamiento modelo 1
predichos=predict(mod1)
mape(bev_train,predichos)*100
mae(bev_train,predichos)
sqrt(mse(bev_train,predichos))

###mape evaluación modelo 1

predichos_e=predict(mod1,newdata = datos_tiempo_test)
mape(bev_test,predichos_e)*100
mae(bev_test,predichos_e)



#### supuestos para residuales:
## distribuyen normal con media 0
## tienen varianza constante
## son independientes

checkresiduals(mod1)

