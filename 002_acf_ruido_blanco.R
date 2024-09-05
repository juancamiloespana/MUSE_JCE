
library(lmtest) ### pruebas de independencia
library(forecast)


autoplot(lynx)
length(lynx) ## tamaño

lynxt1=lynx[2:110] ## original eliminando último
lynxt_1=lynx[5:114] ## rezagada 1 eliminando el primero

cor(lynxt1,lynxt_1)

### prueba estadistica

dwtest(lynxt_1~lynxt1)

par(mfrow=c(1,2))
Acf(lynx, lag.max = 60)
Pacf(lynx, lag.max = 60)



