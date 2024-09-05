
library(forecast)
library(fpp)
library(smooth)
#install.packages("smooth")

autoplot(elecequip)
autoplot(decompose(elecequip))


######

ruta="c:/cod/MUSE_JCE/data/valor_accion_ecopetrol.csv"
df=read.csv(ruta)


#### convertir a serie de tiempo
serie=ts(df$valor, end=c(2024,8), frequency = 12)
autoplot(decompose(serie))


mod_mv=sma_old(serie,h=10, silent=F, interval = 'p' )
mod_mv2=sma_old(serie,h=10, silent=F , order=5)


summary(mod_mv)
summary(mod_mv2)


library(Metrics)

##### modelo 1 ventanaa 25

mape(mod_mv$y, mod_mv$fitted)
hist(mod_mv$y)
rmse(mod_mv$y, mod_mv$fitted)
mae(mod_mv$y, mod_mv$fitted)

##### modelo 2 ventanaa 5

mape(mod_mv2$y, mod_mv2$fitted)
hist(mod_mv2$y)
rmse(mod_mv2$y, mod_mv2$fitted)
mae(mod_mv2$y, mod_mv2$fitted)

####
mean(mod_mv2$residuals)

hist(mod_mv2$residuals, breaks = 10)
hist(mod_mv$residuals, breaks = 10)

