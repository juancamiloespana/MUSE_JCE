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

####### realizar 'marathon' y 'austa' #####
#### con crossvalidation y comparando con otra técnicas #####


####1. Validar si es necesaria transformación box y cox

plot(marathon)

####

par(mfrow=c(1,2))

plot(marathon)
l=BoxCox.lambda(marathon,lower=-7, upper=5)

st_m=BoxCox(marathon,0)
plot(st_m)

ndiffs(st_m)

st_md=diff(st_m,differences = 1)

plot(st_md)

ggtsdisplay(st_md)


mod=auto.arima(marathon,lambda='auto', stepwise = F, 
               approximation = F, max.order=10, trace=T)

summary(mod)

mod2=Arima(marathon,order=c(2,1,1),lambda='auto')
summary(mod2)

par(mfrow=c(1,2))
plot(forecast(mod))
plot(forecast(mod2))



arima_f=function(x,h){
  
  Arima(x,order=c(2,1,1),lambda='auto')%>%
    forecast::forecast(h=h)
}

e=tsCV(marathon,arima_f,h=10)

eh1=e[,2]
mape=mean(abs(eh1/marathon),na.rm=T)


#######

ggtsdisplay(st_md)
ggtsdisplay(st_m)

mod2=Arima(marathon,order=c(2,1,3), lambda=0)
mod2$lambda
summary(mod2)

plot(diff(austa))

ggtsdisplay(diff(austa))


auto.arima(austa, trace=T)





