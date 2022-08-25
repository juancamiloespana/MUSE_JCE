

library(forecast)
library(dplyr) ### para manipulación




###### VAriables #####
url='https://github.com/juancamiloespana/MUSE_JCE/raw/master/flujo_efectivo.csv' ### url donde esté la base actualizada
h=2
suc=1
sign=0.05


df_flujo=read.csv(url)

flujo_suc=subset(df_flujo,suc==suc)
flujo_suc=flujo_suc%>%arrange(fechas) ### garantizar orden

##### determinar frecuencia
##### probar dos frecuencias findfrequency

freq=findfrequency(flujo_suc$flujo_efe)

##### convertir a serie de tiempo ####

flujo_ts=ts(flujo_suc$flujo_efe,frequency = freq)

#### ajustar modelos ####

fit1= auto.arima(flujo_ts)


##### gauardar la información del modelo ajsutado para crossvalidation

p=fit1$arma[1]
q=fit1$arma[2]
P=fit1$arma[3]
Q=fit1$arma[4]
d=fit1$arma[6]
D=fit1$arma[7]
m=frequency(flujo_ts)

#### función para cross validation 
arima_f=function(x,h,p,d,q,P,D,Q,m){
  
  Arima(x,order=c(p,d,q), seasonal=list(order=c(P,D,Q),period=m))%>% 
    forecast::forecast(h=h)
}


### resultados con argumentos de modelo ganador

e2=tsCV(flujo_ts,h=h, arima_f,p=p,d=d,q=q,P=P,D=D,Q=Q,m=m)
mape=mean(abs(e2[,h]/flujo_ts),na.rm=T)*100

cr=checkresiduals(fit1)


if(cr$p.value >=sign)
{
  residuales= "Cumple supuestos"
}else
{
  residuales="No cumple supuestos"
}  
  
resultado=list("modelo"=fit1, "mape_porcentaje"=mape,"supuestos"=residuales)

resultado$supuestos
