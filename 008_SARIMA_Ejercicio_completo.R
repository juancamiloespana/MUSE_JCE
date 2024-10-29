
library(forecast)
library(urca)


url="https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/refs/heads/master/data/amep.csv"

datos=read.csv(url)

plot(datos$d_aus_elec, type="l")

Acf(datos$d_aus_elec, lag.max = 70) ### para comprobar la frecuencia antes de configurar la serie

findfrequency(datos$d_aus_elec) ### función para hallar la frecuencia o estacionalidad de la serie

ts_de=ts(datos$d_aus_elec, start=c(1956, 1), frequency = 12) ### frequency se establece con Acf y con la función findfrequency

autoplot(ts_de)
plot(decompose(ts_de))


####1 transformación de box y cox para estabalizar varianza

lambda=BoxCox.lambda(ts_de)
lambda=round(lambda,1)

### primera modificación de la serie
ts_de_t=BoxCox(ts_de, lambda)

par(mfrow=c(1,2))
plot(ts_de)
plot(ts_de_t)

### dado que la transformación ayuda a estabilizar la varianza se utilizará

## 2 determinar la D y la d, iniciando por la mayúscula

nsdiffs(ts_de_t)
ts_de_t_D=diff(ts_de_t, differences = 1,lag=12)
autoplot(ts_de_t_D)

### prueba de estacionariedad después de diferenciación


summary(ur.kpss(ts_de_t_D))

### valor p es menor al 1%, para un significación del 5%, entonces la Ho se rechaza, h0 es estacionaria 

### como no se volvió estacionaria se debe verificar si se necesita diferenciacioones ordinarias

ndiffs(ts_de_t_D)

ts_de_t_D_d=diff(ts_de_t_D, differences=1)


autoplot(ts_de_t_D_d)

summary(ur.kpss(ts_de_t_D_d))

## como el valor p asociado al valor crítico de 0.0214 es nmayor al 10%, este sería mayor a la singificancia del 5% y por lo tanto se aprueba la ho nula que dice que la serie es estacionaria.


#### D=1, d=1


### 3. Identificar p,q y P, Q usando Acf y Pacf

par(mfrow=c(1,2))
Acf(ts_de_t_D_d, lag.max = 60)
Pacf(ts_de_t_D_d, lag.max = 60)


##### vamos a probar q=1 porque en el ACF hay un pico significativo en el primer rezago
#### Vamos a probar la p =0 asumiendo caida exponencial
#### vamos aprobar Q = 2
### Vamos a probar P=0


#### Ajustar modelo identificados ###

modelo1=Arima(ts_de, order=c(0,1,1), seasonal=c(0,1,2), lambda=lambda)
summary(modelo1)

modelo2=auto.arima(ts_de, lambda=lambda, trace=T, stepwise = F, approximation=F)
summary(modelo2)

checkresiduals(modelo2)

### nos quedaremos con el modelo 2 por tener mejores métricas de desempeño y ser un modelo que no está alejado del análisis de los datos de la serie 



end(ts_de)

pron=forecast(modelo2,level=0.6, h=12)
plot(pron)


### producción
pron$upper  ### intervalo superior del 60% implica que tengo una probabilidad de quedarme sin energía del 20% y una probabilidad de que me sobre energía del 80%



summary(modelo2)

mean((pron$mean-pron$upper)/157)

### La producción es 1.6 desviaciónes aproximadamente, mayor al pronóstico de demanda



url2="https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/refs/heads/master/data/amep2.csv"

datos_reales=read.csv(url2)


### pronostico de 20 datos para compararlo con reales

pron2=forecast(modelo2, level=0.6, h=20)
produccion=pron2$upper

sobrante=produccion-datos_reales$d_aus_elec


## en todos los meses sobre energía y no hubo apagones

mean(sobrante)

