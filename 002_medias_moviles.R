### cargar paquetes ###

library(forecast) ### dar formato de tiempo
library(smooth)
#install.packages("smooth")
#install.packages("Metrics")
library(Metrics) ## para mape, mse, mae


#############

url= "https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/refs/heads/master/data/valor_accion_ecopetrol.csv"
datos=read.csv(url)

##### convertir tabla a serie de tiempo

serie=ts(datos$valor,end=c(2025,3), frequency=12)
print(serie)

par(mfrow=c(1,2))
plot(serie)
Acf(serie)
plot(decompose(serie))


### es una serie estacionaria entonces medias moviles funciona bien

model_mm=sma_old(serie,h=10,interval="p", level=0.95, silent = F)
summary(model_mm)
model_mm$forecast ### muestra valores pronosticos, inicia un periodo despupes a los datos reales
model_mm$lower ### intervalo de confianza inferior
 ## intevalo superior

checkresiduals(model_mm) ####


#### tabla para ver mejor pronostivos e invervalos
datos_pronostico=data.frame(forecast=model_mm$forecast)
datos_pronostico$int_inf=model_mm$lower
datos_pronostico$int_sup=model_mm$upper

model_mm2=sma_old(serie,h=10,interval="p", level=0.95, silent = F,order = 2)
summary(model_mm2)


mape(serie, model_mm$fitted) *100
mae(serie, model_mm$fitted)
sqrt(mse(serie, model_mm$fitted))

mean(model_mm$residuals)

