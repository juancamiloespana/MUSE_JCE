library(forecast) ### tiene la mayoria de funciones y las de boxcox
library(fpp2) ### para los datos


### aparezca en la memoria
data("goog200")
data("elec")

#### graficando la primera

autoplot(goog200)
lambda=BoxCox.lambda(goog200) #### me devuelve el lambda
lambda_r=-1  ### aproximar lambda a entero o fracción entera más cercano
goog_tran=BoxCox(goog200, lambda_r)
autoplot(goog_tran)

#### graficar serie 2 elec


autoplot(elec)
lambda_e=BoxCox.lambda(elec)
lambda_er=0.25
elec_tran=BoxCox(elec, lambda_er)
autoplot(elec_tran)


#### ajustar modelo con función automática###

mod_arima_auto=auto.arima(goog200, lambda=NULL, seasonal=F)
summary(mod_arima_auto)
plot(goog200)
lines(mod_arima_auto$fitted, col="red")

mod_arima_auto2=auto.arima(goog200, lambda=NULL, seasonal=F,trace=T, approximation = F) ### es la misma que el anterior pero muestra modelos probados y no se salta modelos

#### probar modelo con transformación propuesta por boxy cox
mod_arima_auto_t=auto.arima(goog200, lambda="auto", seasonal=F,trace=T, approximation = F)

summary(mod_arima_auto2)
summary(mod_arima_auto_t)


#####
install.packages("urca") ### para prueba de hipótesis de estacionariedad
install.packages("dplyr") ### para usar notación pipe
library(urca)
library(dplyr)

goog200%>%ur.kpss()%>%summary()

#### aplicar diferenciacio

serie_dif=diff(goog200, differences = 1)
serie_dif%>%ur.kpss()%>%summary()



#### procedimientos
d=ndiffs(goog200) ### número de diferenciaciones necesarias

serie_dif=diff(goog200, differences = d)
serie_dif%>%ur.kpss()%>%summary()

autoplot(serie_dif)


####### analizar series pib.csv y diff.csv ######





