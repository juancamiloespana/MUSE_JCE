

##########################################
### 1.4 Ejemplo MAS sin reemplazo #####
########################################

##Marco y Lucy: Encuesta realizada por una entidad gubernamental para medir el crecimiento económico del sector industrial.
##Estrategia de muestreo: Estimador de HT aplicado a un MAS sin reemplazo




#install.packages("TeachingSampling")
library(TeachingSampling) ### Librería con datos ejemplos Marco y Lucy


data(BigLucy) ##Base de datos, marco de muestreo.
attach(BigLucy) 

N <- dim(BigLucy)[1]  ##Tamaño población
set.seed(123) ## para que los calculos sean iguales


### 1. Piloto para estimación de muestra  ####

sam <- sample(N,100) ## extraer muestra piloto de 100 para varianza muestral y calculo de tamaño de muestra, la pilote se extrae por MAS.


Inc.pilot <- Income[sam] ###Variable de interés, ingresos.


mean(Inc.pilot) ##Media de muestra piloto
var(Inc.pilot) ## varianza muestra piloto

## Confianza 95% = 1-alpha 

z=qnorm(0.975)  ## Valor de la normal estandar para una probabilidad de 95%
s=sd(Inc.pilot)
c=25 ## ##Margen de error máximo aceptable (está en millones) lo define el investigador

no= (z^2)*(s^2)/(c^2) ## Muestra con población infinita
n = no/(1+(no/N)) ## Muestra con población finita


### 2. Extracción de muestra con tamaño definido  ####

n=round(n,0)
sam <- S.SI(N,n) ## función para extraer MAS sin reemplazo
muestra <- BigLucy[sam,]   ## base de datos con la muestra
attach(muestra)

### 3. Estimaciones de parámetros de interés ###

estima <- muestra[,c("Income", "Employees", "Taxes")] # base con las columnas para hacer estimación


E.SI(N,n,estima)## función para hacer estimaciones MAS sin reemplazo con HT





###########################################
## 2. Muestreo Aleatorio Sistemático ####
#########################################3


data(BigLucy) ##Base de datos, marco de muestreo.
attach(BigLucy) 

set.seed(123)
N <- dim(BigLucy)[1]
a <- 40 ## definir número de grupos en los que se divide la población

n=floor(N/a)#numero de observaciones en cada grupo

sam <- S.SY(N, a) ## función para extracción de muestra
muestra <- BigLucy[sam,] ##Base con muestra de datos
attach(muestra)
estima <- data.frame(Income, Employees, Taxes) # variables de interés
attach(estima)

E.SY(N, a, estima)  #Estimación muestreo sistemático HT sin reemplazo



#########################################
##### 3. Muestreo Aleatorio Estratificado
##########################################


#install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

## se selecciona una variable categórica que influya en las de interés pero no con el orden. 

data(BigLucy)
attach(BigLucy)

table(Level)

p1 <- qplot(Level, Income, data=BigLucy, geom=c("boxplot"))
p2 <- qplot(Level, Employees, data=BigLucy, geom=c("boxplot"))
p3 <- qplot(Level, Taxes, data=BigLucy, geom=c("boxplot"))
grid.arrange(p1, p2, p3, ncol = 3)


## Tamaño de población de cada estrato

N1 <- summary(Level)[[1]]
N2 <- summary(Level)[[2]]
N3 <- summary(Level)[[3]]

N <- c(N1,N2,N3)
N
## [1] 2905 25795 56596

## Se calcula un tamaño de muestra y se divide proporcional a los valores poblacionales


n1 <- round(2000 * N1/sum(N))
n2 <- round(2000 * N2/sum(N))
n3 <- round(2000 * N3/sum(N))
n <- c(n1,n2,n3)
n

sam <- S.STSI(Level, N, n)
muestra <- BigLucy[sam,]
attach(muestra)


estima <- data.frame(Income, Employees, Taxes)
E.STSI(Level, N, n, estima)


#######################################  
##### 4. Muestreo por conglomerado
#####################################


set.seed(123)
data(BigLucy)
attach(BigLucy)

## Se define una variable que facilite la toma de muestra (Geografía)

UI <- levels(BigLucy$Zone) ## se trata cada nivel de la variable definida como un individuo, se llaman UPM


NI <- length(UI)  ##El nuevo tamaño muestral


nI <- 10 ## tamaño  muestral de las UPM

samI <- S.SI(NI, nI) ## se utiliza MAS para la selección de las UPM, puede ser estratificado o sistemático.

muestra <-UI[samI] ## para saber el nombre de la zona

Lucy1 <- BigLucy[which(Zone == muestra[1]),]
Lucy2 <- BigLucy[which(Zone == muestra[2]),]
Lucy3 <- BigLucy[which(Zone == muestra[3]),]
Lucy4 <- BigLucy[which(Zone == muestra[4]),]
Lucy5 <- BigLucy[which(Zone == muestra[5]),]
Lucy6 <- BigLucy[which(Zone == muestra[6]),]
Lucy7 <- BigLucy[which(Zone == muestra[7]),]
Lucy8 <- BigLucy[which(Zone == muestra[8]),]
Lucy9 <- BigLucy[which(Zone == muestra[9]),]
Lucy10 <- BigLucy[which(Zone == muestra[10]),]

LucyI <- rbind(Lucy1, Lucy2, Lucy3, Lucy4, Lucy5, Lucy6, Lucy7, Lucy8, Lucy9, Lucy10)

## se forma base de elementos muestrales filtrando zonas seleccionadas (censo dentro de la zona)

attach(LucyI)

zona<- as.factor(as.integer(Zone)) ## para tener lista de zonas parte de la muestra
estima <- data.frame(Income, Employees, Taxes)
estimaI <- as.data.frame(T.SIC(estima,zona)) ## totaliza variables por cada variable de interés


E.SI(NI, nI, estimaI)


