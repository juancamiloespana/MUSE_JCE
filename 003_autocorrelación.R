
library(forecast)

url="https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/demanda.csv"

dem=read.csv(url)

ts_demanda=ts(dem$d_pterminado, start = c(1820,2), frequency=12)


serie_lynx=lynx ### no es necesario convertirla a serie de tiempo porque ya está en serie de tiempo


plot(ts_demanda) ### series con tendencia tienen correlación alta con razagos bajos y disminuye la correlación a medida que aumenta el rezago

rez=100  ### se define el rezago que se quiere analizar, se analizan varios rezagos

largo=length(ts_demanda) 

ts1=ts_demanda[1:(largo-rez)] ## se calcula vector original eliminando un numero de filas igual al rezago

ts2=ts_demanda[(1+rez):largo] ### se toma desde el periodo igual al rezago.

cor(ts1, ts2)

###maner automática ###

acf(ts_demanda) ## el 1 está en el numero de periodos de la frecuencia
Acf(ts_demanda,plot=F ) ### el rezago lo muestra consecutivo


### mide la correlación descontando los efectos intermedios, es decir mide mejor la corelación entre un rezago mayor 1
pacf(ts_demanda, plot=F)
Pacf(ts_demanda, plot=F)

