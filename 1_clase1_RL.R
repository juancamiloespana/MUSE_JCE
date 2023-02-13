


library(tseries) # cargar librería para series de tiempo




set.seed(100) ###semilla
t=c(1:200) ##Consecutivo del tiempo
pib=200+ 4*t + rnorm(n=200,mean=100,sd=50) ###Crear variable ficticia


pib_ts=ts(pib, start=1820,frequency =1 ) ##Convertir variable a serie de tiempo
plot(pib_ts) ### para grficarprint(pib_ts) ###Para ver información de la serie

n=length(pib_ts)

# propiedades de la serie de tiempo

start(pib_ts)
end(pib_ts)
frequency(pib_ts)

### Ajustar modelo de regresión lineal
t_ano=t+1820
m1=lm(pib_ts~t_ano)

plot(pib_ts)
abline(m1)
summary(m1)

#######Ejercicio ###

set.seed(100) ###semilla
t=c(1:200) ##Consecutivo del tiempo
pib=200+ 4*t + 2*t^2 + rnorm(n=200,mean=100,sd=50)
  
mean(pib) ### para verificar valor

#######



