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

