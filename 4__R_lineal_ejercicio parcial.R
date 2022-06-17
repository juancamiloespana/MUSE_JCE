### cargar paquetes
library(tseries)
library(Metrics)
library(forecast)
library(car)





library(Mcomp)
t1=M1$MND6$x  # esta es la serie de tiempo que debe utilizar


t1
frequency(t1)

t2=ts(t1, frequency = 1, start=1978)

## graficar serie de tiempo

plot(t2)
length(t2) ##verificar longitud para mirar separación


###Separación de datos train/test

dat_train=window(t2,end=2007) ## datos para entrenamiento
dat_test=window(t2,start=2008) ## datos para evaluación

t_train=c(1:30)
t_test= c(31:42)

### verificar separación correcta

length(dat_train)
length(t_train)

length(dat_test)
length(t_test)
###Crear data.frame

datos_tiempo_train=data.frame("y"=dat_train,"t"=t_train)
datos_tiempo_test=data.frame("y"=dat_test,"t"=t_test)


###### Ajustar/entrenar modelo de regresión lineal 

mod1=lm(y~t,data=datos_tiempo_train)

predichos=predict(mod1,newdata=datos_tiempo_test)


mape(datos_tiempo_test$y,predichos)


t2

predichos[3]-dat_test[3]


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

