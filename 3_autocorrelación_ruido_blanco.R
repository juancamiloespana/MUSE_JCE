library(car)
library(lmtest)

plot(lynx)
length(lynx)


lynx1=lynx[-c(114,113)]
lynx2=lynx[-c(1,2)]

cor(lynx1,lynx2)
dwtest(lynx1~lynx2)

par(mfrow=c(1,2))
acf(lynx)
pacf(lynx)


acf(lynx, plot=F)
pacf(lynx,plot=F)
