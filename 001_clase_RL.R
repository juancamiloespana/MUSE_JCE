
#### instalar paquetes
#install.packages("forecast")
#install.packages("fpp")
##### cargar paquetes ##########

library(forecast)
library(fpp)

#### 1. Leer tabla desde github ###
url='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/pib.csv'

url2="D://pib.csv"

data=read.csv(url2)



#### 2. Convertir a formato ts ####

pib_ts=ts(data$pib2, start=c(1820,1), frequency = 1 )
pib_ts
### 3. graficar la serie y analizarla

autoplot(pib_ts)

### 3.1 ver varias gráficas diferentes





#### 4. crear modelo de RL y analizarlo

##### 4.1 crear variable tiempo y crear data frame ###


#### 4.2 probar que la serie y el tiempo tengan la misma longitud


#### 4.3 Ajustar modelo RL

###  4.4 Generar resumen del modelo y analizarlo

###  5. Escribir modelo con base en salida

##  6. Graficar modelo sobre la serie 

##  7. Hacer predicciones de 2024 del PIB

### 8. COmparar valor ajustado 1850 con valor real 

### 9. Validar supuesto



