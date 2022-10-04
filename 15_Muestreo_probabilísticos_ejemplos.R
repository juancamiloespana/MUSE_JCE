

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


### 1. Piloto para estimación de tamaño muestra  ####

sam <- sample(N,100) ## extraer muestra piloto de 100 para varianza muestral y calculo de tamaño de muestra, la pilote se extrae por MAS.


Inc.pilot <- Income[sam] ###Variable de interés, ingresos.


mean(Inc.pilot) ##Media de muestra piloto
var(Inc.pilot) ## varianza muestra piloto



n_sd=2  ## Número de desviaciones para una probabilidad de 95%
s=sd(Inc.pilot)
B=25 ## ##Margen de error máximo aceptable (está en millones) lo define el investigador
D=(B/n_sd)^2

n=(N*s^2)/(((N-1)*D)+s^2)  ## Muestra con población finita dist normal



### 2. Extracción de muestra con tamaño definido  ####

n=round(n,0)
sam <- S.SI(N,n) ## función para extraer MAS sin reemplazo
muestra <- BigLucy[sam,]   ## base de datos con la muestra
attach(muestra)

### 3. Estimaciones de parámetros de interés ###

estima <- muestra[,c("Income", "Employees", "Taxes")] # base con las columnas para hacer estimación


A=E.SI(N,n,estima)## función para hacer estimaciones MAS sin reemplazo del total con con HT



#### tarea hacer una función para calcular tamaño muestral, definiendo si la distribución es normal o no y el número de desviaciones utilizadas




###########################################
## 2. Muestreo Aleatorio Sistemático ####
#########################################3


data(BigLucy) ##Base de datos, marco de muestreo.
attach(BigLucy) 

set.seed(123)
N <- dim(BigLucy)[1]
n=386 ## según MAS
a <- floor(N/n) ## definir número de grupos en los que se divide la población


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

## se selecciona una variable categórica que influya en las de interés. 
### en este caso la variable selecionada es: Level que mide el tamaño de la empresa

data(BigLucy)
attach(BigLucy)
N <- dim(BigLucy)[1]

table(Level)

p1 <- qplot(Level, Income, data=BigLucy, geom=c("boxplot"))
p2 <- qplot(Level, Employees, data=BigLucy, geom=c("boxplot"))
p3 <- qplot(Level, Taxes, data=BigLucy, geom=c("boxplot"))
grid.arrange(p1, p2, p3, ncol = 3)


## Tamaño de población de cada estrato

N1 <- summary(Level)[[1]]
N2 <- summary(Level)[[2]]
N3 <- summary(Level)[[3]]

Nst <- c(N1,N2,N3)
Nst
## [1] 2905 25795 56596

#### calcular alfas o proporciones de observaciones en cada estrato

aB=N1/sum(N)
aM=N2/sum(N)
aS=N3/sum(N)

a=c(aB,aM,aS)
##### generar piloto y calcuar la varianza muestral de cada estrato



Big=BigLucy[BigLucy$Level=="Big",] ## filtrar por empresas BIG
samBig=sample(N1,20) ### seleccionar una muestra para el piloto
BigSam=Big[samBig,] ### seleccionarmuestra aleatoria de Big
varB=var(BigSam$Income) ### calcular desv muestral para estrato Big

Med=BigLucy[BigLucy$Level=="Medium",] ## filtrar por empresas M
samMed=sample(N2,30) ### seleccionar una muestra para el piloto
MedSam=Med[samMed,] ### seleccionarmuestra aleatoria de Big
varM=var(MedSam$Income) ### calcular desv muestral para estrato Big

Small=BigLucy[BigLucy$Level=="Small",] ## filtrar por empresas M
samS=sample(N3,50) ### seleccionar una muestra para el piloto
SmallSam=Small[samS,] ### seleccionarmuestra aleatoria de Big
varS=var(SmallSam$Income) ### calcular desv muestral para estrato Big

varst=c(varB,varM,varS)

#### calcular tamaño muestral

B=25
D= 25^2/4 ## para la media

num=  sum(Nst^2*varst*(1/a))
den = (N^2)*D + sum(Nst*varst)

n=num/den
  
## calcular  tamaño de muestra y se divide proporcional a los valores poblacionales
  
n1 <- round(n*aB)
n2 <- round(n*aM)
n3 <- round(n* aS)
nst <- c(n1,n2,n3)

sam <- S.STSI(Level, Nst, nst)
muestra <- BigLucy[sam,]
attach(muestra)


estima <- data.frame(Income, Employees, Taxes)
E.STSI(Level, Nst, nst, estima)


#######################################  
##### 4. Muestreo por conglomerado
#####################################


set.seed(123)
data(BigLucy)
attach(BigLucy)

## Se define una variable que facilite la toma de muestra (Geografía)

M <- dim(BigLucy)[1]
UI <- levels(BigLucy$Zone) ## se trata cada nivel de la variable definida como un individuo, se llaman UPM


NI <- length(UI)  ##El nuevo tamaño muestral



######
nI <- 10 ## tamaño  muestral de las UPM para piloto

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



#### analizar tamaño de muestra #####

##### calcular tamaño muestral por 0.5############

