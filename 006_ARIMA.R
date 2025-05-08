library(fpp2)
library(forecast)


data("goog200")

autoplot(goog200)

y=goog200

length(y) ## calcular cantidad de datos


y_1=y[2:200] ## serie empezando en 2
y_2=y[1:199] ## serie terminando uno antes

y_diff= y_1-y_2

plot(y_diff, type="l")

install.packages("urca")
install.packages("dplyr")
library(urca)
library(dplyr)## para notación pip

y%>%ur.kpss()%>%summary()
y_diff%>%ur.kpss()%>%summary()

d=ndiffs(goog200) ## dice la d 

ydiff=diff(goog200, differences = d) ## aplica las diferenciaciones


