library(fpp2) ### datos
library(forecast) ## modelos
library(ggplot2) ## gráficos
library(urca) ## prueba de estacionariedad


####Analizar ACF y PACF
par(mfrow=c(1,2))
serie=uschange
Acf(uschange[,"Consumption"],main="Consumo EEEUU")
Pacf(uschange[,"Consumption"],main="Consumo EEEUU")

Acf(uschange[,"Consumption"],plot=F)
Pacf(uschange[,"Consumption"],plot=F)

#### ejericio con serie uschange[,'Consumption]

### series a analizar austa, marathon



serie1=uschange[,"Consumption"]


###1. verificar atípicos y varianza constante

plot.ts(serie1) ### ver la gráfica de la serie
seasonplot(serie1) ## ver si tiene estacionalidad

#### 2.transformación para estabilizar varianza

## no se aplica

###3. identificar el orden d

ndiffs(serie1) ## identifica el número de diferenciaciones
p=ur.kpss(serie1) ### prueba para estacionariedad
summary(p)

###el orden d = 0


### 4 identificar el p y q con acf y pacf


ggtsdisplay(serie1)

####modelos a probar ###
####ARIMA(3,0,0)
####ARIMA(0,0,3)
####ARIMA(0,0,4)
####ARIMA(3,0,3)

##5

model1=Arima(serie1, order=c(3,0,0))
summary(model1)

model2=Arima(serie1, order=c(0,0,3))
summary(model2)

model4=Arima(serie1, order=c(3,1,0))
summary(model4)

modelo5=auto.arima(serie1, seasonal=F,approximation = F, stepwise = F)
summary(modelo5)


plot(model4)

####







