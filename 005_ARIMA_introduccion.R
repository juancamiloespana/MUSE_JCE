library(fpp2)
library(forecast)




###datos a analizar departures

data(departures)
#rm(df_dep) para borrar objetos de la memora
dep_perm=departures[,'permanent']
start(dep_perm)
frequency(dep_perm)
end(dep_perm)

plot(dep_perm)
dep_perm2000=window(dep_perm, start=c(1995,1)) ###eliminar primera parte

plot(dep_perm2000)

Acf(dep_perm2000, lag.max=60)


####transformacion

lambda=BoxCox.lambda(dep_perm2000)

dep_perm_trans=BoxCox(dep_perm2000, lambda)


plot(dep_perm_trans)


###ajustar modelo automatico

arima1=auto.arima(y=dep_perm2000, seasonal = F, lambda=lambda)
arima1$fitted

summary(arima1)






