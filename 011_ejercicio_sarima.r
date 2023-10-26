library(forecast)
library(urca)

url= 'https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/amep.csv'


##identificar frecuencia
amep_df=read.csv(url)
findfrequency(amep_df$d_aus_elec) ### frecuencia de 12

### convertir a serie de tiempo

ts_amep=ts(amep_df$d_aus_elec, start=c(1956,1), frequency = 12)

autoplot(ts_amep)

### transformación para estabilizar varianza

l=BoxCox.lambda(ts_amep)
l_r=round(l, 1)
ts_a_t=BoxCox(ts_amep, l_r)

autoplot(ts_a_t) ### comprobar si funciona la transformacion utilizarla con transformación porque la varianza quedó homogena=constante


####identificar D diferenciaciones estacionales
D=nsdiffs(ts_a_t, max.D = 4)

ts_a_t_D=diff(ts_a_t, lag=12, differences = D)
### en el lag va la frecuencia de la serie

autoplot(ts_a_t_D)

nsdiffs(ts_a_t_D, max.D = 4) ### verificar que no necesite más estacionales

###prueba de estacionariedad

ts_a_t_D%>%ur.kpss(lag='long')%>%summary()


#### identificar d o primeras diferenciaciones

d=ndiffs(ts_a_t_D, max.d = 5)
ts_a_t_D_d=diff(ts_a_t_D, differences = d)
autoplot(ts_a_t_D_d)

####prueba de estacionariedad

ts_a_t_D_d%>%ur.kpss(lag='long')%>%summary()

### el valor p es mayor a 10% es estacionaria


#######identificar p, q, P, Q

ggtsdisplay(ts_a_t_D_d, lag.max = 60)


###probar dos modelos 
###modelo 1: (0,1,1)(0,1,2)
###modelo 2: (0,1,1)(0,1,0)

model1=Arima(ts_amep, order=c(0,1,1), seasonal=c(0,1,2), lambda = l_r)
model2=Arima(ts_amep, order=c(0,1,1), seasonal=c(0,1,0), lambda = l_r)

summary(model1) 
summary(model2)

### gana modelo 1 por AIC y por RMSE

model3=auto.arima(ts_amep, lambda=l_r, seasonal = T, trace=T, approximation = F, stepwise = F)

### stepwise =T se salta algunos modelos para hacer la busqueda más rápida.

summary(model3) ## mejor en AIC y RMSE
### verificar supuestos
checkresiduals(model3)


end(ts_amep)

#####para pronosticos

pron=forecast(model3, level=c(0.99))
autoplot(pron)


