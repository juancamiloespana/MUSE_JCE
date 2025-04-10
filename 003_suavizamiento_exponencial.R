#install.packages("fpp2")

library(fpp2) ### para los datos oil
library(forecast) ### paquete con los modelos ses, holt, y hw

data(oil) ### serie del paquete fpp2 quede en memoria

### si los datos están en formato serie de tiempo, no se necesitan configuración con la función TS

### analisis grafico

autoplot(oil) ## no es claro si tiene tendencia o no
Acf(oil) ### si parece tener tendencia
decompose(oil) ## no funciona para series con frequency=1

## la fequencia es el argumento que define cada cuantos periodos se evalúa la estacionalidad

print(oil) ## el print muestra características
### caundo no aparecen con los nombres se pueden mirar

start(oil)
end(oil)
frequency(oil)


mod_ses=ses(oil, h=20, level=c(0.95,0.90))
## caundo no se pone alpha, el modelo calcula el alpha que minimiza el AIC

##alpha = es el peso del último periodo en el promedio ponderado

# el complemento (1-alpha) ## es el peso que se distribuyen en todos los periodos hacia atras menos último

summary(mod_ses)

mod_ses$mean ## pronosticos
mean(mod_ses$residuals) ## promedio de residuales es el me
mod_ses$upper ## intervalos de confianza superiores
mod_ses$lower ## intervalo de confianza inferiores
mod_ses$fitted ## predicciones en las mismas fechas de los datos originales 


plot(mod_ses) ## para sacar el grafico


library(smooth)
mod=sma_old(oil, silent=F, order=5)
summary(mod)
