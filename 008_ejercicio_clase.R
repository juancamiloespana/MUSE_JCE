library(forecast)
library(Metrics)


url="https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/caso_estudio/flujo_efectivo.csv"

datos=read.csv(url)
datos$flujo_efe=datos$flujo_efe/1000000
datos_suc=subset(datos, suc==2) ### filtra solo datos de suc 2
datos_suc$flujo_efe=datos_suc$flujo_efe+10
mean(datos_suc$flujo_efe) ### tiene que dar 8.673029


################

ts_flujo=ts(datos_suc$flujo_efe, start=1, frequency = 7)


end(ts_flujo)



plot(ts_flujo, type="lines")
lines()
mod=hw(ts_flujo, seasonal="multiplicative", frequency=7)

library(Metrics)

summary(mod)
mape(ts_flujo, mod$fitted)

plot(mod)
lines(mod$fitted, col="red")
mean(ts_flujo)
plot(ts_flujo)
mod2=ses(ts_flujo)
summary(mod2)
library(smooth)
m=sma_old(ts_flujo, silent=F)
summary(m)

library(plotly)
plot_ly(y=ts_flujo[1:200], type="scatter", mode="lines")

findfrequency(ts_flujo)

############

sma=sma_old(ts_flujo, level=0.5, interval="parametric")
ses_m=ses(ts_flujo)
holt_m=holt(ts_flujo)
hw_m=hw(ts_flujo, seasonal="additive")
hw_m2=hw(ts_flujo, seasonal="multiplicative")

summary(sma)
summary(ses_m)
summary(holt_m)
summary(hw_m)
summary(hw_m2)


plot(sma$forecast)
sma$upper
mean(ts_flujo)
plot(ts_flujo)

end(ts_flujo)

ts_flujo[601]= 8.06

ts2=ts(c(ts_flujo,8.06), start=1, frequency=7)


