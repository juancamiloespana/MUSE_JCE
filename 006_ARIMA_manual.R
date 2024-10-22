
library(fpp2)
library(forecast)
library(urca)


goog200
length(goog200)
yt=goog200[2:200]
yt_1=goog200[1:199]

plot(goog200,type="l")
y_dif=yt-yt_1

plot(y_dif, type="l")

y_doff2=diff(goog200, differences = 1)

plot(y_doff2, type="l")



prueba=ur.kpss(y_dif)
summary(prueba)

ndiffs(y_dif)


