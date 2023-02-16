library(smooth) # para medias móviles
library(fpp) ## para conjuntos de datos
library(forecast) ## para modelos de pronósticos y validación cruzada
library(Metrics) ## para calcular MAPE


###### analizar serie austourist con sma diferenets h

class(austourists) ## ver formato de serie de tiempo
print(austourists) ## ver años y datos
frequency(austourists) ## Identificar si la serie tiene una frecuencia, sí se debe modificar toca convertira con función ts
length(austourists) ## cantidad de datos
plot(austourists)  ## ver la serie graficamente

train=window(austourists,end=c(2007,4))
test=window(austourists,start=c(2008,1))
m_sma=sma(train,h=12)
m_sma
mape(train,m_sma$fitted)
mape(test,m_sma$forecast)
errores=test-m_sma$forecast


###### Analizar serie e identificar modelos adecuados ######

class(austourists) ## ver formato de serie de tiempo
print(austourists) ## ver años y datos
frequency(austourists) ## Identificar si la serie tiene una frecuencia, sí se debe modificar toca convertira con función ts
length(austourists) ## cantidad de datos
plot(austourists)## ver la serie graficamente


aust1=ts(austourists,frequency=1)
acf(aust1)
pacf(aust1)

#### Ajustar medias móviles #######


sma_model=sma(austourists,h=10, order=8,silent=F,interval=T)

sma_model
sma_model$forecast


ajustado=sma_model$fitted
real=austourists

mape(real,ajustado)

sma_f<-function(x,h){  ### se debe crear función con la función de pronóstico y los argumentos que se quieran utilizar
  
  greybox::forecast(sma(x,h,order=8))
}


e=tsCV(austourists,sma_f,h=10)  ### esta función permite hacer el calculo de varios pronósticos de un mismo h al mismo tiempo

e[,1]

sma_model$residuals

erroresh1<-e[,1]
erroresh2<-e[29:48,2]
erroresh3<-e[29:48,3]
erroresh10<-e[29:48,10]



tsCV

mape(sma_model$y,sma_model$fitted)

mapeh1=mean(abs(erroresh1/austourists),na.rm=T)
mapeh2=mean(abs(erroresh2/ausair[29:48]),na.rm=T)
mapeh3=mean(abs(erroresh3/ausair[29:48]),na.rm=T)
mapeh10=mean(abs(erroresh10/ausair[29:48]),na.rm=T)

mapeh1
mapeh2
mapeh3
mapeh10

plot(pigs)


mode=ses(austourists)

