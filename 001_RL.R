url='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/pib.csv'

pib=read.csv(url) ### cargar datos de url, se puede cambiar url por ruta de pc 'D\\misdocumentos'

pib_ts= ts(pib$pib2, start=1820, frequency=1) ### frecuencia es el numero de datos en que cambia el tiempo
##start se puede utilizar con dos valores: ejemplo start=c(1920,3), inicia en marzo de 1920

start(pib_ts) ## muestra fecha inicio de la serie
end(pib_ts) ### muestra fecha fin
time(pib_ts) ### extra vector solo con tiempo

print(pib_ts) ###para ver serie de tiempo y su información

plot(pib_ts) 

t=as.numeric(time(pib_ts)) ### extrae el tiempo y 
#lo convierte a numerico para evitar problemas


rl=lm(pib_ts~t) ## ajustar o entrenar el modelo
summary(rl) ### analizar el modelo


plot(pib_ts) ### grafica la serie

#grafica el modelo o los pronosticos
lines(t,rl$fitted.values, col="red", lwd=2)


# Escribir modelo con base en la salida
#Hacer predicciones de 2024
#Comparar predicciones 1850 con valor real
#Validar supuestos


### crear data frame con valores de variable explicativa que quiero pronosticar
t_pron=data.frame(t=c(1850, 2021, 2022,2024))

##la funcion predict se le da el modelo ajustado y los datos para los que se quiere predecir
pred=predict(rl, newdata=t_pron)
pred[1]
### valores ajustados solo para tiempos con los que se entrenó el modelo
rl$fitted.values[31] ## valor ajustado
pib_ts[31] ### valor real


### diferencia(residual) entre valor ajustado y valor real
res=rl$fitted.values[31]-pib_ts[31] 



library(forecast)

## validar supuesto: erorres normales con media cero, varianza constante e independencia
checkresiduals(rl)

###En este parece estar bien el modelo


