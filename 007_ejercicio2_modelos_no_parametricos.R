
library(fma)
library(forecast)
library(Metrics)
library(smooth)


##### ejemplo descargar y analizar serie

write.csv(beer,"beer.csv")
data=read.csv("beer.csv")

### analizarla gráficamente para identificar estacionalidad
plot(data$x,type="line")
pacf(data$x)

beer=ts(data$x,frequency = 12, start=c(1980,1))
#######


#####Análisis de piggs

###Mirar formato de serie ###

class(pigs)
print(pigs)
frequency(pigs)
start(pigs)
end(pigs)

###Graficamente ###

plot(pigs)
acf(pigs)
pacf(pigs)

####Eliminar atipico

st=window(pigs,start=c(1980,4))
plot(st)

sma_m=sma(st,silent=F,h=5, interval=T)
sma_m

pred=sma_m$fitted
mape(st,pred)


smaf=function(x,h)
{  
  
  greybox::forecast(sma(x,h,order=3))
}

e=tsCV(st,h=5,smaf)

abs_e=abs(e[,5])
mean(abs_e/st,na.rm=T)

sqrt(mean(e[,5]^2,na.rm=T))


hist(e[,2])

checkresiduals(sma_m)

######
sma_m

se_m=ses(st,h=15)
plot(se_m)

summary(se_m)
mape(st,se_m$fitted)


se_f=function(x,h){
  
  forecast::forecast(ses(x,h))
}

e_se=tsCV(st,h=15,se_f)

mean(abs(e_se[,15])/st,na.rm=T)


hist(e_se[,1])






