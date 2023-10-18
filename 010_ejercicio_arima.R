library(fpp2)
library(forecast)
library(urca)
library(dplyr) 

start(marathon)
frequency(marathon)

mara=ts(marathon, start=1897, end=2006, frequency = 1)
print(mara)

autoplot(mara)

mean(mara[100:110]) ## promedio de últimos 10 periodos

#### 1 necesita transformación lambda(estacionariedad en)

l=BoxCox.lambda(mara)
l_r=round(l)
mara_t=BoxCox(mara, l_r)

#### graficar serie transformada y no transformada
par(mfrow=c(1,2))
plot(mara)
plot(mara_t)
### como la transformacion no estabiliza la varianza es mejor no hacerla

d=ndiffs(mara)
mara_d=diff(mara, differences = 1)
plot(mara_d)

mara_d%>%ur.kpss()%>%summary()

### valor p > 10%, entonces se acepta la ho que dice que la serie es estacionaria.



###identificar orden p y q
ggtsdisplay(mara_d)

### probaremos el p=2 está claro
## q puede ser 0 o 3

mod1=Arima(mara, order = c(2,1,0), seasonal = F)
mod2=Arima(mara, order = c(2,1,3), seasonal = F)

summary(mod1)
summary(mod2)

### el ganador sería el mod 2 que gana en la mayoria de indicadores, solo pierde en el BIC porque este indicador tiene una penalización más alta por la complejidad del modelo


mod3=auto.arima(mara, seasonal = F, trace=T, max.order = 6, approximation = T)
summary(mod3)


### el modelo ajustado manualmente tiene mejores indicadores

checkresiduals(mod2)
checkresiduals(mod3)

###se ajusta mejor el mod2

pron=forecast(mod2, h=11)
write.csv(pron, "D:\\pron213.csv")

pron2=forecast(mod3, h=10)
write.csv(pron2, "D:\\pron112.csv")

marathon_l10=marathon[111:120]



write.csv(marathon_l10, "D:\\marathon_reales.csv")



library(urca)
library(forecast)

lynx%>%ur.kpss()%>%summary()

mod=Arima(lynx, order=c(1,0,1))
summary(mod)


autoplot(forecast(mod, h=10, level=0.85))


1547.2415*(1- 0.5424 -0.6108  )
