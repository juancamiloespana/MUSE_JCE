
#### cargar paquetes

library(smooth) ## paquete con funcion sma_old
library(forecast)
library(fpp)### paquete con series de tiempo tiene la serie a10


#### cargar archivos con series 

ruta_vae='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/valor_accion_ecopetrol.csv'
ruta_dem='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/demanda.csv'
ruta_suc='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/caso_estudio/flujo_efectivo.csv'

df_vae=read.csv(ruta_vae)
df_dem=read.csv(ruta_dem)
df_suc=read.csv(ruta_suc) ### serie de tiempo de todas las sucursales
df_suc17=df_suc[df_suc$suc ==17,] ###serie de tiempo de sucursal 17 (escogida al azar)

####convertir a series de tiempo

ts_vae=ts(df_vae$valor, start=c(1959,3), frequency=12)
ts_dem=ts(df_dem$d_pterminado, start=c(1820,45), frequency=365)
ts_suc17=ts(df_suc17$flujo_efe,start=c(2021,1), frequency = 365)


#### graficar las series de tiempo ###

autoplot(ts_vae)
autoplot(ts_dem)
autoplot(ts_suc17)
autoplot(a10) ### esta serie de tiempo esta en el paquete fpp, ya esta en formato ts

####################################
########  1. Serie VAE ############
###################################


####### comparar modelos 
####usar: sma, ses, holt, holt winters

### Ajustar modelos de medias moviles y ses(alpha optimizado y alpha fijo)
mod_sma=sma_old(ts_vae, silent=F) ###modelo de medias móviles
mod_ses=ses(ts_vae) ##modelo con alpha optimizado
mod_ses2=ses(ts_vae, alpha=0.8, h=2) ##modelo con alpha manual


### graficar ####

plot(mod_sma, which=7) ## la funcion autoplot no funciona con objeto de smooth
autoplot(mod_ses) ### los modelos del paquete forecast no imprimen grafica como smal_old
autoplot(mod_ses2)

#### analizar resultados
summary(mod_sma) ## ###AIC 176.94 ###
sqrt(mod_sma$lossValue) ### para calcular rmse de medias móviles 
summary(mod_ses) ###AIC 193.26, RMSE 4.69
summary(mod_ses2,alpha=0.8)### AIC 206 ### RMSE 6.13











