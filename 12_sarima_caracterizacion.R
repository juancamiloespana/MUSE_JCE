

# Para simular series sarima

#install.packages("astsa") 


library(fpp2)
library(plotly)
library(forecast)
library(astsa)
library(tseries)


###Asimulación procesos SARIMA ######


set.seed(123)
sar1=sarima.sim(sma=c(-0.3,-0.3,-0.3),S=4,n=2000)

ggtsdisplay(sar1,lag.max = 60)

set.seed(123)
sar1=sarima.sim(n=1000)

ggtsdisplay(sar1,lag.max = 60)


set.seed(123)
sar1=sarima.sim(sma=.8, sar=.8,S=12,n=5000)

ggtsdisplay(sar1,lag.max = 60)

################################################
########### Ejercicio completo ##################
################################################

##### Ajustar modelo para serie1.csv de github
### la serie es mensual y empieza en febrero de 1910
url='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/serie1.csv'
st=read.csv(url)

plot.ts(st$x)
plot_ly(y=st$x,type="scatter", mode="line")
st=ts(st$x,frequency=12,start=c(1910,2) )

autoplot(st)
ur.kpss(st)%>%summary()
adf.test(st)

nsdiffs(st)

st_D=diff(st, lag=12)
ndiffs(st_D)


#### Analizar ACF y PACF

ggtsdisplay(st_D,lag.max = 100)



fit=auto.arima(st, approximation = F, stepwise = F, trace=T)

checkresiduals(fit)

plot(forecast(fit))

seasonplot(st)

summary(fit)

mean(st)
sd(st)

##################################################
###########Ejericio estudiantes ###################

##### Ajustar modelo para serie2.csv de github
### la serie es mensual y empieza en diciembre de 2000


