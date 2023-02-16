library(fpp2)
library(plotly)
library(forecast)








###Analizar gráfica

plot(h02)

#### verificar estacionariedad

ur.kpss(h02)%>%summary()

######## verificar cuántas diferenciaciones necesita

ndiffs(h02) ## primeras diferenciaciones
nsdiffs(h02) ### diferenciacción estacional

##### identificar el periodo de la estacionalidad ###

frequency(h02) #### normalmente las series no lo traen

plot_ly(y=h02,type = 'scatter', mode = 'lines')

par(mfrow=c(1,2))
Acf(h02,lag.max = 60)
Pacf(h02)


####aplicar primera diferenciación

#### calcular la diferenciación estacional de la serie manualmente
#### 0.5 al primero que lo haga ####

h02_s=diff(h02,lag=12)
mean(h02_s) ### con la media de la serie diferenciada se verifica que haya quedado bien

n=length(h02)
n_f=n-12
yt_m= h02[1:n_f]
yt=h02[13:n]

ytt=yt-yt_m
mean(ytt)

#### con un for ####

#dif=rep(NA, n-12)
rm(dif)
dif=c()


for(i in 13:n){
  dif[i-12]=h02[i]-h02[i-12]
 
}

mean(dif,na.rm=T)

#########

plot.ts(h02_s)
ur.kpss(h02_s)%>%summary()


h02_sd=diff(h02_s, differences = 1)
mean(h02_sd)

plot.ts(h02_sd)
ur.kpss(h02_sd)%>%summary()



##### analizar euretail


st=euretail

autoplot(st)

ndiffs(st)
nsdiffs(st)

### no parece necesaria transformación


frequency(st)
Acf(st, lag=60)
Pacf(st)
plot_ly(y=st, type="scatter",mode="lines")


st_s=diff(st,lag=4)

autoplot(st_s)
ur.kpss(st)%>%summary()

st_sd=diff(st_s, differences = 1)

autoplot(st_sd)
ur.kpss(st_sd)%>%summary()

ggtsdisplay(st_sd)

### p=(0,1,2),d=1, q=(0,1)
### P=1, D=1, Q =1

