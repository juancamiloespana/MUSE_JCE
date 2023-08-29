#Cargar paquetes
install.packages("Metrics")
install.packages("forecast")

#Cargar paquetes
library(Metrics) ### indicadores, mape, rmse
library(forecast) ### para validar supuestos

url="https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/demanda.csv"

dem=read.csv(url)

ts_demanda=ts(dem$d_pterminado, start = c(1820,2), frequency=12)

plot(ts_demanda)


### calcular la fecha para 80% de los datos
###160 datos son el 80%
### para 160 datos serían 13.3 meses
### como serie inicia en feb 1820
### 13 .3 sería aproximadamente 1833 mayo

ts_train=window(ts_demanda, end=c(1833,5))
ts_test=window(ts_demanda, start=c(1833,6))

##se debe sacar tiempo para cada serie train y test
t_train=as.numeric(time(ts_train))
t_test= as.numeric(time(ts_test))


## como se identificó en la grafica que podría tener relación cuadrática se saca el tiempo al cuadrado para usar como variable explicativa

t_train2=t_train**2
t_test2=t_test**2


###### ajustar modelos ###

### modelo con el tiempo solo
mod1=lm(ts_train~t_train)

plot(ts_demanda)
lines(t_train, mod1$fitted.values, col='blue', lwd=2)
summary(mod1)


###modelo con tiempo cuadrado
mod2=lm(ts_train~t_train+t_train2)
lines(t_train, mod2$fitted.values, col="red", lwd=2)


#### comparar AIC
AIC(mod1)
AIC(mod2)

BIC(mod1)
BIC(mod2)

###### indicadores desempeño en entrenamiento(residuales)

## se multiplica por 100 si se quiere en porcentaje
mape(ts_train, mod1$fitted.values)*100
mape(ts_train, mod2$fitted.values)*100

###se puede calcular igual para rmse mae

#####indicadores para evaluacion

### el nombre de columna de dataframe de evaluación tiene que coincidir con nombre de columna con que se entreno
df_test=data.frame(t_train=t_test, t_train2=t_test2)

pred_m1=predict(mod1,newdata=df_test )

mape(ts_test, pred_m1)*100 ### mape de evaluación modelo 1


pred_m2=predict(mod2,newdata=df_test )
mape(ts_test, pred_m2)*100

checkresiduals(mod2)
checkresiduals(mod1)




