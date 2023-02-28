library(Metrics)
library(forecast)

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


##### separar datos en entrenamiento y prueba base datos demanda


ruta='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/demanda.csv'
d=read.csv(ruta, dec=".")

d_ts=ts(d$d_pterminado, start=c(1820,2), frequency=12)

##### grafica serie completa

plot(d_ts)

ldts=length(d_ts) #### longitud  serie de tiempo completa
ltrain=floor(ldts*0.8) ### longitud serie de  t entrenamiento

### crear variables explicativas(tiempo)

t=as.numeric(time(d_ts)) ###extrae el tiempo de la serie y lo convierte a número
t2= t**2 ## eleva el tiempo al cuadrado para modelar componente cuadrática


df_ts=data.frame(d_ts,t, t2) ## se crea data frame para facilitar particion de datos


df_ts_train=df_ts[1:ltrain, ]  ### base de datos entrenamiento
df_ts_test=df_ts[(ltrain+1):ldts,] ### base de datos evaluacion


rl1= lm(d_ts~t, data=df_ts_train)
rl2=lm(d_ts~t+t2, data=df_ts_train)

plot(d_ts)
lines(df_ts_train$t, rl1$fitted.values, col="blue",lwd=2)
lines(df_ts_train$t, rl2$fitted.values, col="red",lwd=2)


#### calcular mape y rmse en entrenamiento ###

####para modelo 1 lineal
mape(df_ts_train$d_ts,rl1$fitted.values )
rmse(df_ts_train$d_ts,rl1$fitted.values )

###para modelo2 cuadratico
mape(df_ts_train$d_ts,rl2$fitted.values )
rmse(df_ts_train$d_ts,rl2$fitted.values )

### solo para el modelo, no se saca entrenaiento y evaluacion
AIC(rl1) 
AIC(rl2)

BIC(rl1) 
BIC(rl2)

mean(df_ts_train$d_ts) ### para saber qué tan grande es el rmse



#######Calcular mape y rmse en evaluacion

###pronósticos en evaluacion

pro_rl1=predict(rl1, newdata=df_ts_test) ## pronostico evaluacion modelo 1
pro_rl2=predict(rl2, newdata=df_ts_test) ## pronostico evaluacion modelo 2


df_ts_test['pro_rl1']=pro_rl1  ### agregar a df evaluacion los pronosticos rl1
df_ts_test['pro_rl2']=pro_rl2 ### agregar a df evaluacion los pronosticos rl2


mape(df_ts_test$d_ts, pro_rl1) ### mape evaluacion
rmse(df_ts_test$d_ts, pro_rl1) ## rmse evaluacion


mape(df_ts_test$d_ts, pro_rl2) ### mape evaluacion
rmse(df_ts_test$d_ts, pro_rl2)



### calcular pronostico para enero de 1837

##t= 1837.083
t=c(1837.083, 2023.083)
t2= t**2

df_pro_fut=data.frame(t,t2)
predict(rl2, newdata = df_pro_fut)

checkresiduals(rl2)  
### aunque la prueba de independicia se rechaza, los gráficos muestran un buen modelo


checkresiduals(rl1)



