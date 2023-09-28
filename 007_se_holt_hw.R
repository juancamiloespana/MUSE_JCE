library(fpp2)
library(forecast)


data('ausair', package="fpp2") ### para cargar datos

print(ausair)

ts_air=window(ausair, start=1990) ### se filtra desde 1990 porque lo indica el ejercicio

autoplot(ts_air)

mod_holt=holt(ts_air, h=20, level=c(0.80,0.95))
autoplot(mod_holt)

mod_ses=ses(ts_air, h=20, level=c(0.80,0.95))
autoplot(mod_ses) ### como se ve en la gráfica el modelo ses no ajusta bien porque no estima la tendencia.


#### analicemos los resumenes de los modelos

summary(mod_holt)

summary(mod_ses)



######### para ajustar hw

library(fpp2)
library(forecast)
data("austourists", package = 'fpp2')


autoplot(austourists)

frequency(austourists) ## frecuencia configurada
print(austourists)


findfrequency(austourists) ## detectar la frecuencia

### siquiero cambiar frecuencia 
cambio_f= ts(austourists, start=c(1999,1), frequency = 2)

### 

modelo_hw_m=hw(austourists, h=30,seasonal="multiplicative", level=0.95)
modelo_hw_a=hw(austourists, h=30,seasonal="additive", 0.95)

autoplot(modelo_hw_a)
autoplot(modelo_hw_m)

summary(modelo_hw_m) ## aic 413.6953, rmse 2.022337
summary(modelo_hw_a) ## aic 420.5278, rmse 2.339669

####









