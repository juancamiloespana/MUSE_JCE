library(forecast)
library(urca)

ruta1='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/dif.csv'
ruta2='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/pib.csv'

dif=read.csv(ruta1)
pib=read.csv(ruta2)

ts_pib=ts(pib$pib2, start=1900, frequency = 1)
ts_dif=ts(dif$x, start=1900, frequency = 1)


autoplot(ts_pib)
autoplot(ts_dif)

acf(ts_pib)
acf(ts_dif)


######diferenciacion orden 1 manual ###

n_pib=length(ts_pib) #### calcular cuantos valores tiene la serie
ts_pibt1=ts_pib[1:n_pib-1] ### extraemos la serie sin el ultimo valor
ts_pibt=ts_pib[2:n_pib] ##extraemos la serie sin el primer valor

ts_pibdif= ts_pibt-ts_pibt1 ### calcular serie diferenciada
plot(ts_pibdif, type='l') ######grafico de lineas


#####diferenciacion con funcione
autoplot(ts_pib)
ts_pibdif2 =diff(ts_pib, differences = 1) ### el argumento differences define el orden de las diferenciaciones
autoplot(ts_pibdif2)
acf(ts_pibdif2)
ndiffs(ts_pib) ##indica el numero de diferenciaciones necesarias para volverse estacionarias

#####segunda serie aplicar diferencaciones

ts_dif_dif=diff(ts_dif, differences=1)
autoplot(ts_dif_dif)
acf(ts_dif_dif)
ndiffs(ts_dif)

ur.kpss(ts_dif)%>%summary() ### para serie original, no es estacionaria
ur.kpss(ts_dif_dif)%>%summary() ### con primera difereciacion no es estacionaria

ts_dif_dif=diff(ts_dif, differences=2) ###aplicar differenciación de orden 2
ur.kpss(ts_dif_dif)%>%summary() ####


