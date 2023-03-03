library(lmtest) ### para dwtest de correlación


#### analizar autocorrelación de lynx y demanda
#### ¿parece haber autocorrelación?
#### medir correlacion entre dos variables demanda y tiempo ##

plot(lynx) #### no debería tener patron claro parecido a distribucion uniforme

###correlacion entre dos variables
lynx2=lynx*3 ### crear una segunda variable

plot(lynx2)

####### probar correlacion entre dos series
cor(lynx,lynx2)

###### correlacion de una serie con ella misma rezagada un tiempo
#### que el valor del tiempo t de la serie, esa afectado por el tiempo t-1

rez=1
lon=length(lynx)-rez ### calcular cantidad de datos al eliminar rezagos
lynxt=lynx[1:lon] ### serie eliminando el numero de rezagos al final
lynxt1=lynx[-c(1:rez)] ### elimina las filas desde la 1 hasta el rez (el - elimina filas)

cor(lynxt,lynxt1)
plot(lynxt, lynxt1, col='red')

dwtest(lynxt~lynxt1)

#### autocorrelacion rezago 5

rez=5
lon=length(lynx)-rez ### calcular cantidad de datos al eliminar rezagos
lynxt=lynx[1:lon] ### serie eliminando el numero de rezagos al final
lynxt1=lynx[-c(1:rez)] ### elimina las filas desde la 1 hasta el rez (el - elimina filas)

cor(lynxt,lynxt1)
plot(lynxt, lynxt1, col='red')
dwtest(lynxt~lynxt1)


#####

ruta='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/demanda.csv'
d=read.csv(ruta)
d_ts=ts(d$d_pterminado,start=1800, frequency=1)
plot(d_ts)

rez=1
lon=length(d_ts)
d_tst=d_ts[1:lon-rez]
d_tst1=d_ts[-c(1:rez)]

cor(d_tst,d_tst1)

dwtest(d_tst1~d_tst) 
### no es capaz de detectar la autorcorrelacion pero se ve graficamente 
#y con el coeficiente que si existe.
plot(d_tst,d_tst1)



########
par(mfrow=c(1,2))
acf(lynx)
pacf(lynx)

acf(lynx, plot=F)
pacf(lynx, plot=F)
