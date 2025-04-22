library(forecast)
library(fpp2)


### ajustar la serie ###

data(ausair) ## aparezca la serie en memoria temporal

print(ausair) ### para ver inicio final y frecuencia


#### recortar la serie para que empiece en 1990 como indica el ejercicio
ausair_cut= window(ausair, star=1990)

autoplot(ausair) ### tiene una tendecia exponencial más dificil de modelar

autoplot(ausair_cut) ## la recortada tiene una tendencia lineal

### es una serie con tendencia, estacionaria en varianza, no es estacionaria en media, no tiene estacionalidad

modelo_holt=holt(ausair_cut, h=10, level=c(0.80, 0.90), alpha=0.5, beta=0.4)

summary(modelo_holt)
## AIC147.0974
## 1.883976

plot(modelo_holt)


#####

modelo_holt2=holt(ausair_cut, h=10, level=c(0.80, 0.90))

summary(modelo_holt2)

autoplot(modelo_holt2)


par(mfrow=c(1,2))
plot(modelo_holt)
plot(modelo_holt2)


### suavizamiento exponencial simple

model_ses=ses(ausair_cut, h=10, level=c(0.80, 0.90))

summary(model_ses)
autoplot(model_ses)

### el ganador es modelo_holt2, por AIC, por MAE y graficamente

######## modelo de holtwinters

data("austourists")

print(austourists) ## cuando no imprima caracteristicas se pueden imprimir manualemnte

start(austourists)
end(austourists)
frequency(austourists)

autoplot(austourists)

### análisis gráfico

## No es estacionario en media ni en viaranza:
## tiene tendencia
## tiene estacionalidad
### tiene varianza aumentando 

### para saber la frecuencia de la serie
## la frecuencia es el número de periodos en el que se dan estacionalidades
Acf(austourists, lag.max = 40)
findfrequency(austourists)


### ajustar modelo

model_holt=holt(austourists) ## los otros hiperparametros son ocionales
model_holt$model$aic ## para imprimier solo aic
autoplot(model_holt)


### holt winters aditivo

model_hw_ad=hw(austourists, seasonal="additive")
summary(model_hw_ad)
model_hw_ad$model$aic
autoplot(model_hw_ad)
##mae 1.810486
## mape 5.1694%


model_hw_mp=hw(austourists, seasonal="multiplicative")
summary(model_hw_mp)
### mae 1.506177
##mape 4.118462
model_hw_mp$model$aic

autoplot(model_hw_mp)

### validar supuestos ####
checkresiduals(model_hw_mp)
