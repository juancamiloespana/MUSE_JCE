

# Para simular series sarima

#install.packages("astsa") 


library(fpp2)
library(plotly)
library(forecast)
library(astsa)


###Asimulación procesos SARIMA ######


set.seed(123)
sar1=sarima.sim(sma=c(-0.3,-0.3,-0.3),S=4,n=2000)

ggtsdisplay(sar1,lag.max = 60)


set.seed(123)
sar1=sarima.sim(n=1000)

ggtsdisplay(sar1,lag.max = 60)


set.seed(9)
sar1=sarima.sim(d=1,D=1, sar=.3, ar=0.4,S=12,n=100)

ggtsdisplay(sar1,lag.max = 100)

write.csv(sar1,"serie2.csv",row.names = F)

################################################
########### Ejercicio completo ##################
################################################

#####

set.seed(123)
sar1=sarima.sim(D=1, sar=.3,S=12,n=100)
start(sart)ggtsdisplay(sar1,lag.max = 60)
ggtsdisplay(sar1)

write.csv(sar1, "serie1.csv",row.names  =F)
sar

serie=read.csv('https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/serie1.csv')

plot.ts(serie$x)
plot_ly(y=serie$x, type="scatter", mode="lyne")
ggtsdisplay(serie$x,lag.max = 100)

### convertir a serie ###

st=ts(serie$x,frequency=12,start=c(1910,2))
plot(st)

ur.kpss(st)%>%summary()

ndiffs(st)
nsdiffs(st)

st_D=diff(st, lag=12)

ggtsdisplay(st,lag.max = 100)

fit=auto.arima(st,trace=T, approximation = F)

checkresiduals(fit)


##### serie 2 ##############


set.seed(9)
sar1=sarima.sim(d=1,D=1, sar=.3, ar=0.4,S=12,n=100)

ggtsdisplay(sar1,lag.max = 100)

write.csv(sar1,"serie2.csv",row.names = F)
