#install.packages('smooth')
#install.packages('fpp')

library(smooth) ## paquete con funcion sma_old
library(forecast)
library(fpp)### paquete con series de tiempo

##########################################################
################  Ejercicio  ##############################
##########################################################

#Para el siguiente ejercicio Utilizara las siguientes series:

  #valor accion ecopetrol (inicia 1959 marzo) es mensual 
  #demanda (inicia el día 45 de 1820)  es diaria frecuencia anual
  #Serie a10 del paquete fpp venta mensual

#1 Garantizar que las series  estan en formato ts y con frecuencia diferente de 1
#2. Evaluar graficamente las siguientes series y decir que componentes parecen tener
  ### 1.1 Serie vs el tiempo
  ### 1.2 ACF y PACF
  ### 1.3 descomponer la serie
#3. de acuerdo al analisis anterior a cual serie le funcionaria mejor un pronostico por medias moviles
#4. ajustar modelo de medias moviles para las tres series y analizar


##############################################
############# punto 1 #########################
#################################################

### cargar series de tiempo

ruta_vae='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/valor_accion_ecopetrol.csv'
ruta_dem='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/demanda.csv'

df_vae=read.csv(ruta_vae)
df_dem=read.csv(ruta_dem)
##### cargar tabla con series de tiempo de caso de estudio
ruta_suc='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/caso_estudio/flujo_efectivo.csv'
df_suc=read.csv(ruta_suc) ### serie de tiempo de todas las sucursales
df_suc17=df_suc[df_suc$suc ==17,] ###serie de tiempo de sucursal 17 (escogida al azar)
ts_suc17=ts(df_suc17$flujo_efe,start=c(2021,1), frequency = 365)


### convertir a formato de serie de tiempo
ts_vae=ts(df_vae$valor, start=c(1959,3), frequency= 12 )
ts_dem=ts(df_dem$d_pterminado, start=c(1820,45), frequency = 365 )

##### cuando la serie es de R y está en formato ts, se analizan sus caracteristicas
print(a10)
start(a10)
end(a10)
frequency(a10)

##############################################
############# punto 2 #########################
#################################################

autoplot(ts_suc17) ### es estacionaria, seria adecuada para MA
autoplot(ts_vae)### es estacionaria, seria adecuada para MA
autoplot(ts_dem) ## tiene tendencia no seria adecuada MA, seria mejor por RL
autoplot(a10) ###tiene tendencia, tiene estacionalidad, tiene varianza No constante


par(mfrow=c(1,2))

Acf(ts_suc17)
Pacf(ts_suc17)
####la serie suc17 parece ser estacionaria, la mayoria de correlaciones no superan el intervalo de confianza

par(mfrow=c(1,2))
Acf(ts_vae)
Pacf(ts_vae)
### parece ser estacionaria, el intervalo de confianza de la grafica es más grande cuando hay menos datos, como en esta serie

par(mfrow=c(1,2))
Acf(ts_dem)
Pacf(ts_dem)
### tiene tendencia 

par(mfrow=c(1,2))
Acf(a10)
Pacf(a10)
### la tendencia y la estacionalidad en period 12-13


####descomposicion clasica
ts_d=decompose(a10)
autoplot(ts_d)

ts_d2=decompose(ts_vae)
autoplot(ts_d2)

##############################################
############# punto 3 #########################
#################################################

mod_suc=sma_old(ts_suc17, silent=F, order=5, interval='p')
mod_vae=sma_old(ts_vae, silent=F, order=5, interval='p')
mod_vae2= sma_old(ts_vae, silent=F, interval='p')

summary(mod_vae) ## permite ver indicadores de desempeño del modelo
summary(mod_vae2)

mod_vae$fitted ### para extraer valor ajustados 
mod_vae$forecast ## para extraer pronosticos de la serie de tiempo
mod_vae$lower ### para extraer intervalo de confianza de pronostico inferior
mod_vae$upper### para extraer intervalo de confianza de pronostico superior

mod_vae=sma_old(ts_vae, silent=F, order=5, interval='p', holdout = T)
mod_dem=sma_old(ts_dem, silent=F,order=50, interval='p', holdout = T)
### en este modelo se ve que para series con tendencia el pronostico no es bueno con MA






