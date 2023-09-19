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





