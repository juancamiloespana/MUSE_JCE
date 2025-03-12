#install.packages("forecast")
library(forecast)

lynx=lynx
print(lynx) 

plot(lynx)
autoplot(lynx)
cor(lynx,lynx)
lynx[4]

### rezago 1
lynx1=lynx[2:114]
lynx2=lynx[1:113]

cor(lynx2,lynx1)

#rezago 2
lynx1=lynx[3:114]
lynx2=lynx[1:112]

cor(lynx2,lynx1)

par(mfrow=c(1,2))
Acf(lynx, lag.max = 40, plot=F,type="correlation") ### autocorelacipon
Pacf(lynx, lag.max=40) ## función de autocorrelación
