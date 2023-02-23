library(Metrics)

lynx

plot(lynx)
t=as.numeric(time(lynx))

rl=lm(lynx~t)
abline(rl)
summary(rl)  ### el modelo no ajusta bien, se identifica
### en el valor p del B1, es mayor a la significancia de 0.05(definida por investigador)
### esto indica que no se puede rechazar Ho, por lot tanto el modelo no es significativo
### También se indetifica en el R2, deberia ser cercano a uno para ser un buen modelo

mean(lynx)



#### ejercicio demanda ##

ruta='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/demanda.csv'
d=read.csv(ruta, dec=".")

d_ts=ts(d$d_pterminado, start=c(1820,2), frequency=12)

print(d_ts)
plot(d_ts)

### crear variables explicativas(tiempo)

t=as.numeric(time(d_ts)) ###extrae el tiempo de la serie y lo convierte a número
t2= t**2 ## eleva el tiempo al cuadrado para modelar componente cuadrática

rl1=lm(d_ts~t) ## ajusta modelo de regresion lineal
rl2=lm(d_ts~t+t2) ## ajusta modelo de regresion lineal con componente al cuadrado



plot(d_ts)
lines(x=t, y=rl1$fitted.values)### agregar modelo a la gráfica valores ajustados
lines(x=t, y=rl2$fitted.values, col='red', lwd=2)

#####
### para analizar que modelo esta mejor se mira, significancia, R2, residual estandar error es casi igual que RMSE
summary(rl1)
summary(rl2)

#### indicadores en entrenamiento
mape(d_ts, rl1$fitted.values)
mape(d_ts, rl2$fitted.values)

rmse(d_ts, rl1$fitted.values)
rmse(d_ts, rl2$fitted.values)

### medidas de comparacion de modelos no se interpretan, solo se dice que el mejor modelo es el que tiene menor valor
AIC(rl1)
AIC(rl2)
BIC(rl1)
BIC(rl2)









