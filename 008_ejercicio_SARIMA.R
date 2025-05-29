#### paquetes básicos

library(urca) ### prueba de estacionariedad
library(forecast) ## paquete de modelos

###cargar datos

url= "https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/refs/heads/master/data/amep.csv"


datos=read.csv(url)

### configurar la serie de tiempo

ts_elec=ts(datos$d_aus_elec, start=c(1956,1), frequency=12)
end(ts_elec)

ts_elec_train=window(ts_elec, end=c(1992,12)) #es la que se utiliza en proceso de ajuste y análisis

ts_elec_test=window(ts_elec, start=c(1993,1)) ### solo se utiliza para calcular las metricas de desempeño, cuando ya termine de ajustar el modelo

### la suma de datos de train y test debe dar el total de datos de la serie original.


### analisis gráficos ##

autoplot(ts_elec_train)


### No es estacionaria en media: porque tiene tendencia, y porque parece tener estacionalidad. d>0, D>0

#No es estacionaria en varianza: La varianza va aumentando, lambda <> 1 para transformar la serie y volverla estacionaria en varianza

Acf(ts_elec_train, lag.max=80)
## confirmamos que la frecuencia está bien en 12

m=12 ### es igual a la frecuencia


### 1 transformación

lambda=BoxCox.lambda(ts_elec_train)
lambda=round(lambda,1) # guardar lambda para ajustar modelo manual

ts_transf=BoxCox(ts_elec_train, lambda = lambda) ## se le aplica transformación con lambda


## verificar que la transformación
par(mfrow=c(1,2))
plot(ts_elec_train)
plot(ts_transf)


### 2. Volverla estacionaria en media, D y d


D=nsdiffs(ts_transf) ## calcula D, guardar para ajuste manual

ts_Diff= diff(ts_transf, differences = D, lag=m)

autoplot(ts_Diff)

summary(ur.kpss(ts_Diff))

## se rechaza hipotesis nula, entonces no estacionaria

d=ndiffs(ts_Diff) ## guardar para ajuste manual

ts_diff= diff(ts_Diff, differences = d)

autoplot(ts_diff)

summary(ur.kpss(ts_diff))
## como valor p es >10%, se aprueba ho y la serie estacionaria

par(mfrow=c(1,2))
Acf(ts_diff, lag.max=65)
Pacf(ts_diff,lag.max=65)

## q= 1 segun Acf
## p= 3 segun PACF
## Q= 2 segun ACF en periodos multiplos de 12
## P= 0 Segun PACF en periodos multiplos de 12


modelo_man= Arima(ts_elec_train, lambda=lambda, order=c(3,d, 1), seasonal=c(0,D,2))
summary(modelo_man)
##AICc= 603.73 
##RMSE: 152.3454
##MAE: 98.05

modelo_auto=auto.arima(ts_elec_train, lambda=lambda, stepwise = F, approximation = F, max.order=8, trace=T)

summary(modelo_auto)
##AIcc:-610.41
##RMSE: 148.81
##MAE: 99.16

checkresiduals(modelo_man)
checkresiduals((modelo_auto))

### trabajaremos con el manual pórque tiene mejor mae y en supuestos está un poco mejor


### pronosticos con modelo seleccionado

pronosticos=forecast(modelo_man, h=31, level=0.8 )
autoplot(pronosticos)

## costo de sobrante 5
## costo de faltante 50

### 10 veces más probable que sobre, a que falte


###     10% (80%) 10%




distancia=13072 - 13549

desv_esta=152

distancia/desv_esta


### cargar nuevos datos

url= "https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/refs/heads/master/data/amep2.csv"


datos_nuevos=read.csv(url)


ts_elec2=ts(datos_nuevos$d_aus_elec, start=c(1994,1), frequency = 12)


prod_rec=window(pronosticos$upper, start=c(1994,1))

sobrantes_prod=prod_rec-ts_elec2
