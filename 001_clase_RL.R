
#### instalar paquetes
#install.packages("forecast")
#install.packages("fpp")
##### cargar paquetes ##########

library(forecast)
library(fpp) ### datos de series de tiempo

#### 1. Leer tabla desde github ###


###ruta pc 

ruta="D://pib.csv"

data=read.csv(ruta)


#### 2. Convertir a formato ts ####

pib_ts=ts(data$pib2, start=c(1820,1), frequency = 1 )

pib_ts ## para imprimir valores y caracteristicas
# para ver la caracteristica de la serie
start(pib_ts) 
frequency(pib_ts)
end(pib_ts)


### 3. graficar la serie y analizarla


autoplot(pib_ts)


### 3.1 ver varias gráficas diferentes

autoplot(ausbeer)
autoplot(elec)



#### 4. crear modelo de RL y analizarlo

##### 4.1 crear variable tiempo y crear data frame ###
t=time(pib_ts) ### extraer tiempo
t= as.numeric(t) ## convertir a numero
data$t= t ## guardar tiempo en data frame para facilitar modelo de regresion lineal


#### 4.3 Ajustar modelo RL

mod_rl=lm(data=data,pib2~t)

###  4.4 Generar resumen del modelo y analizarlo

summary(mod_rl)

###  5. Escribir modelo con base en salida

##  6. Graficar modelo sobre la serie 
plot(pib_ts)
lines(t, mod_rl$fitted.values, col="red", lwd=2)
##  7. Hacer predicciones de 2024 del PIB

datos_nuevos=data.frame(t=c(1850, 2024,2025, 2050))
predict(mod_rl,newdata = datos_nuevos)

window(pib_ts, start =1850, end =1850) ### valor real de 1850


### 8. COmparar valor ajustado 1850 con valor real 

### 9. Validar supuesto
checkresiduals(mod_rl)



