
library(forecast)
library(smooth)
url='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/valor_accion_ecopetrol.csv'

vac_df=read.csv(url)
ts_vac=ts(vac_df$valor, end=c(2023,8), frequency=12)


autoplot(ts_vac)
autoplot(decompose(ts_vac))


mod_ma=sma_old(ts_vac, silent="none", interval="parametric", level=0.80)
### silente="none" para que muestre gráfica
###interval="parametric" para que gráfique intervalo de predicción
##level: nivel de confianza del intevalo

summary(mod_ma) ### ver indicadores


### modelo con ventana de 2

mod_ma2=sma_old(ts_vac, silent="none", interval="parametric", level=0.80, order=2)

summary(mod_ma2)

checkresiduals(mod_ma)
checkresiduals(mod_ma2)

library(Metrics)


## cuanto se desvía pronóstico con respecto a valor real
mape(ts_vac,mod_ma$fitted) ### en porcentaje
rmse(ts_vac,mod_ma$fitted) ## en unidades de la variable

ts_vac
mod_ma$forecast

mean(mod_ma$residuals)
