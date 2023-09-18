###instalar paquetes
install.packages("fpp2")
install.packages("forecast")
installed.packages("Metrics")


##### cargar paquetes

library(fpp2)
library(forecast)
library(Metrics)

### cargar datos
data(oil, package = "fpp2")

print(oil)
autoplot(oil)

mod_ses=ses(oil,h=5, level=c(0.60,0.95))

plot(mod_ses)
lines( mod_ses$fitted, col="red")

summary(mod_ses)

mod_ses$fitted
datosunidos=data.frame(mod_ses$fitted, oil)

mod_ses2= ses(oil,h=5, level=c(0.70,0.95), alpha=0.2)
plot(mod_ses2)
lines( mod_ses2$fitted, col="red")

summary(mod_ses)

mean(mod_ses$residuals)

bp



