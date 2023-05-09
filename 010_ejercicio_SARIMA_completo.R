###### cargar paquetes ####
library(forecast)
library(urca)


###### Ejercicio 1: Ajuste un modelo para la serie  de producción de electricidad mensual en australia
###### amep.csv  que se encuentra en la carpeta data de github
##### la serie inicia en enero de 1956 

#### Realice un pronostico para agosto de 1997

#### cuál sería su pronóstico si tuviera que asegurar que sólo se arriesga un
#### 3% a tener un valor real superior a lo pronósticado

##### cargar datos

ruta='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/amep.csv'

elec=read.csv(ruta) 

### 1. identificar frecuencia
plot(elec$elec, type='lyne') ## se identifica patron repetitivo (estacionalidad)
Acf(elec$elec, lag.max = 80) ## se identifica que se da cada 12 periodos aproximadamente
Pacf(elec$elec,lag.max = 40 )## se identifica que se da cada 12 periodos aproximadamente
findfrequency(elec$elec)## nos dice el valor de la frecuencia

### 2. Convertir ts y explorar la serie original

ts_e=ts(elec$elec, frequency=12,start=c(1956,1)) ## convertir a ts con frecuencia definida e información dada (inicio)
autoplot(ts_e)
ggseasonplot(ts_e, polar=T)

### 3. TRansformación box y cox

l=BoxCox.lambda(ts_e) ### identifica lambda y se guarda

ts_e_t=BoxCox(ts_e, l) ### serie transformada

par(mfrow=c(1,2)) ## dividir pantalla de graficos
plot(ts_e_t) ## graficar serie transformada
plot(ts_e) ## grafica serie original

ggseasonplot(ts_e_t) ### ver las estacionalidad despues de estabilizar varianza


#### 4. identificar D, o diferenciacion estacional ###

ns=nsdiffs(ts_e_t) ## nuemro de diferenciaciones estacionales necesarias
ts_e_t_D =diff(ts_e_t, lag= 12, differences = ns) ### serie transformada y diferenciada
autoplot(ts_e_t_D)
ur.kpss(ts_e_t_D)%>%summary() ## verificar si se volvio estacionaria

#### 5. identificar d, o diferenciacion oridinaria ###

n=ndiffs(ts_e_t_D) ## identificar si sigue necesitando diferenciacion normal

ts_e_t_D_d=diff(ts_e_t_D, differences = n) ## no se le da el lag para que sepa que son primeras diferenciaciones

autoplot(ts_e_t_D_d)

ur.kpss(ts_e_t_D_d)%>%summary() ### ya es estacionaria 

#### 6 identificar con ACF y PACF los ordenes p,q y P y Q

par(mfrow=c(1,2))
Acf(ts_e_t_D_d, lag.max = 50)
Pacf(ts_e_t_D_d, lag.max = 50)

### en la grafica se identifican q=1, Q=2; p=0,1 ; P=0

#### Ejercicio 2 ajustar un modelo para la serie1.csv de data en github
#### la serie es mensual, termina en 2023 marzo
### hacer un pronostico para 2023 mayo y 2026 mayo y comparar



