library(fpp2) ## datos
library(forecast) ### suavizamientos exponenciuales
library(smooth) #medias moviles


#### cargar serie de tiempo ###

serie=oil ### ya esta como serie de tiempo

datos=c(serie,1,3,4)

autoplot(serie)
###es una serie que parece tener tendencia, no es claro su patron, se recomendaria ajustar modelo con y sin tendencia y comparar.

m_ses=ses(serie, h=12, level=c(0.95,0.6))

summary(m_ses)

### analisis intervalo pronostico 2014
# (471,(542), 612) hay una probabilidad del 85% de que el valor real de produccion de petroleo quede en ese intervalo

### complemento del intervalo 15% 
### tengo una probabilidad de 7.5% de que el valor real de la produccion sea inferior a 471
### tengo una probabilidad de 7.5% de el valor real sea superior a 612


m_ses2=ses(serie, h=12, level=c(0.95,0.85), alpha=0.3)
summary(m_ses2)


autoplot(m_ses)
autoplot(m_ses2)

hist(m_ses$residuals)

#  error = real -predicho

mean(m_ses$residuals)



### complemento del intervalo 40% 
### tengo una probabilidad de 20% de que el valor real de la produccion sea inferior a 501
### tengo una probabilidad de 20% de el valor real sea superior a 583


##### ajustar modelo de holt para oil


m_holt=holt(serie, h=12, level=c(0.95, 0.85))
summary(m_holt)

autoplot(m_holt)


######## austourists

frequency(austourists)
autoplot(austourists)

mod1=sma_old(austourists)
mod2=ses(austourists)
mod3=holt(austourists)
mod4=hw(austourists, seasonal='additive')
mod5=hw(austourists, seasonal='multiplicative')


summary(mod1)
mod1$s2 ### sacar sigma cuadrado varianza del error
mod2$model$sigma2
mod3$model$sigma2
mod4$model$sigma2
mod5$model$sigma2

autoplot(mod5)
autoplot(mod2)

summary(mod4)
summary(mod5)



