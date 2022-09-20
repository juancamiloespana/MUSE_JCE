
library(astsa)
library(forecast)
library(urca)
library(plotly)

set.seed(78)
ts=sarima.sim(ar=(0.6),sar=c(0.3,0.6),S=12,n=500)
ggtsdisplay(ts)


findfrequency(ts)

nsdiffs(ts)


ts_D=diff(ts,lag=12)

ur.kpss(ts_D)%>%summary()

ggtsdisplay(ts_D)
ndiffs(ts_D)


write.csv(ts,"serie_examen.csv")

ts


plot_ly(y=ts, type="scatter", mode="line")


acf(ts)

ts

ts[500]=5

fit=auto.arima(ts)
fit%>%summary()
a=8

if(a>1 & a< 5){
  
  print("hola")
}else{
  print("chao")
  
}


