library(fpp2)
library(forecast)

data("departures")


### la notación [,1] se utiliza porque hay varuas seruies en departures, en este caso se está utilizando la primera

print(departures[,1]) 

### características de la serie
start(departures[,1])
end(departures[,1])
frequency(departures[,1])

autoplot(departures[,1])
Acf(departures[,1], lag.max = 80)
### no tiene atípicos
### la varaianza va aumentando requiere transformacion con un lambda
### tiene tendencia, entonces d>0
### tiene estacionalidad, y arima no la modela


### aplicar transformación

data(goog200)
data(elec)


autoplot(goog200)
autoplot(elec)

lambda_g=BoxCox.lambda(goog200)

yt=goog200
wt=BoxCox(yt, lambda_g)

par(mfrow=c(1,2))
plot(wt)
plot(yt)

### como no hace efecto sobre la serie no se debería aplicar transformación


yt2=elec
lambda_e=BoxCox.lambda(yt2)
lambda_e=0.25

wt2=BoxCox(yt2, lambda_e)


plot(yt2)
plot(wt2)

modelo_arima=auto.arima(yt2, seasonal = FALSE, lambda=lambda_e)

summary(modelo_arima)

pronos=forecast(modelo_arima, h=20, level=c(0.90,0.95))

plot(pronos)

