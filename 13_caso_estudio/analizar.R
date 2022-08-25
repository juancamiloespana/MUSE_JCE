

install.packages("formattable")
#### cargar paquetes

library(forecast)
library(dplyr) ### para manipulación
library(formattable)


#### cargar archivo de funciones propias

source("13_caso_estudio/funciones.R")
load(".RData")


###Definir variables

suc1=10
h=2
sign=0.05
fecha='2022-08-23'
freq=30



#####Analizar una sola sucursal ###

suc_una=analizar_suc(suc1=suc1,h=h,sign=sign,fecha=fecha,freq=freq)

### dar formato a table

datos_suc=formattable(suc_una$info_suc)
datos_suc$saldo_actual=currency(datos_suc$saldo_actual/1000000,digits=0)
datos_suc$saldo_max_pro=currency(datos_suc$saldo_max_pro/1000000,digits=0)
datos_suc$saldo_min_pro=currency(datos_suc$saldo_min_pro/1000000,digits=0)
datos_suc$cap_suc=currency(datos_suc$cap_suc/1000000,digits=0)
datos_suc

suc_una$modelo%>%summary()


####Analizar todas las sucursales #####

t_ini=proc.time()
suc_todas=analizar_todas(fecha=fecha,max_suc=20,freq=1)
t_f =proc.time()-t
t_f

datos_suc=formattable(suc_todas$info_suc)
datos_suc$saldo_actual=currency(datos_suc$saldo_actual/1000000,digits=0)
datos_suc$saldo_max_pro=currency(datos_suc$saldo_max_pro/1000000,digits=0)
datos_suc$saldo_min_pro=currency(datos_suc$saldo_min_pro/1000000,digits=0)
datos_suc$cap_suc=currency(datos_suc$cap_suc/1000000,digits=0)
datos_suc
