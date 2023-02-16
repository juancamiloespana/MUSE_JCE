

library(fpp2) ### para cojunto de datos



##### ver graficamente elec
par(mfrow=c(1,2))
plot(elec)
l=BoxCox.lambda(elec)
st1=BoxCox(elec,l)
st2=BoxCox(elec,0)
plot(st1)
plot(st2)







#### calcular lambda para elec con boxcox ###




### aplicar transformación lambda con BoxCo ###



#### probar diferentes lambdas



####calcular boxcox a serie varianza disminuyendo






#####utilizar lambda de Arima




###a10
###arrivals[,3]
### auscafe
### eggs 




######Otros ejericios ###

####### 1. Gráfica una serie con 36 números aleatorios de una normal estándar y grafíquelos, otra serie con 360 y otra serie con 1000
###### 1. cuáles son los valores críticos de cada gráfico
##### 1. son ruido blanco
##### 1. cuál es la diferencia y a qué se debe




####2.a verifique gráficamente si la serie ibmclose es estacionaria ###
####2.b realice una prueba estadística para verificar si la serie es estacionaria


y <- ts(numeric(100))
e <- rnorm(100)
for(i in 2:100)
  y[i] <- 0.6*y[i-1] + e[i]














