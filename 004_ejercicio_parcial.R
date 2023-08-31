
library(fpp2)
elec2 <- window(elec, start=1974)

mean(elec2)

plot(elec2)

#### dividir la serie

elec_t=window(elec2, end=c(1991,4))
elec_e=window(elec2, start=c(1991,5))

t_train=as.numeric(time(elec_t))
t_test=as.numeric(time(elec_e))


###entrenamos
mod1=lm(elec_t~t_train)
summary(mod1)
####b0 = -8.3 x 10^5
####b1= 423,1

library(Metrics)
mape(elec_t,mod1$fitted.values)*100


#### ### para evaluación

test=data.frame(t_train=t_test)
pred=predict(mod1, newdata=test)

rmse(elec_e, pred)
mae(elec_e, pred)
