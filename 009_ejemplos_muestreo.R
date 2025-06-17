
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



### 1. Tamaño muestra  ## ###

### para media ####
n_sd=2  ## Número de desviaciones para una probabilidad de 95%

## 99%  2.58
## 95%  1.96
## 90%  1.64


s=300  ## se conoce teórica o por un piloto
B=25 ## ##Margen de error máximo aceptable (está en millones) lo define el investigador
D=(B/n_sd)^2

n=(N*s^2)/(((N-1)*D)+s^2)  ## Muestra con población 



###  1. Extracción de muestra con tamaño definido  ####

n=round(n,0)

set.seed(123)
sam <- S.SI(N,n) ## función para extraer MAS sin reemplazo
muestra <- BigLucy[sam,]   ## base de datos con la muestra
attach(muestra)

### 3. Estimaciones de parámetros de interés ###

estima <- muestra[,c("Income", "Employees", "Taxes")] # base con las columnas para hacer estimación
A=E.SI(N,n,estima)## función para hacer estimaciones MAS sin reemplazo del total con con HT

sum(BigLucy$Income) ### parámetro poblacional, normalmente no se conoce

media_income=A[1,2]/N  ###estimador muestral

mean(BigLucy$Income) ### parametro poblacional media, real, normalmente no se conoce

###el argumento DEFF es  Design effect es la relación entre la varianza obtenido por el muestreo actual y el que se obtendría con


### análisis despues de datos
## suponga que recolectó una muestra de 300 y tome la desviación muestral, calcule el margen de error real

s=sd(muestra$Income)
n=300

B = n_sd * s * sqrt((N/n - 1) / (N - 1))  ### de la ecuación de n despejando B





###########################################
## 2. Muestreo Aleatorio Sistemático ####
#########################################3


data(BigLucy) ##Base de datos, marco de muestreo.
attach(BigLucy) 

set.seed(123)
N <- dim(BigLucy)[1]
n=572 ## según MAS el  mismo del anterior
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


## se selecciona una variable categórica que influya en las de interés. 
### en este caso la variable selecionada es: Level que mide el tamaño de la empresa

data(BigLucy)
attach(BigLucy)
N <- dim(BigLucy)[1]

table(Level)

boxplot(Income~Level, data=BigLucy)


## Tamaño de población de cada estrato

N1 <- summary(Level)[[1]]
N2 <- summary(Level)[[2]]
N3 <- summary(Level)[[3]]

Nst <- c(N1,N2,N3) ## pongo los tamaños de poblaciones por estrato en un solo vector

#### calcular alfas o proporciones de observaciones en cada estrato

aB=N1/N
aM=N2/N
aS=N3/N

a=c(aB,aM,aS)


#####  la varianza muestral de cada estrato teórica o por piloto


sdB=378 # si no se tiene teórica suponer una y verificar después con datos obtenidos
sdM=131
sdS=115

varB=sdB^2### calcular varianza para estrato 
varM=sdM^2 ### calcular varianza para estrato 
varS=sdS^2 ### calcular varianza para estrato 

varst=c(varB,varM,varS)

#### calcular tamaño muestral

B=25
D= (25^2)/4 ## para la media, intervalo del 95%

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
table(muestra$Level) ## para verificar las muestra que calcule


estima <- data.frame(Income, Employees, Taxes) ## suponer que son datos recolectados en la muestra
E.STSI(Level, Nst, nst, estima)
##3 Level debe ser de la muestra

#######################################  
##### 4. Muestreo por conglomerado
#####################################

set.seed(123)
data(BigLucy)
attach(BigLucy)

## Se define una variable que facilite la toma de muestra (Geografía)

M <- dim(BigLucy)[1] ### total de empresas 

U <- levels(BigLucy$Zone) ## UPM, es la lista de conglomerados

N <- length(U)  ##Número de conglomerados

Mi=table(BigLucy$Zone) ## tamaño de población de cada conglomerado

n <- 10 ## Número de conglomerados en muestra para piloto

### n=37 para repetir procedimiento después de calculo de muestra


#### extraer muestra de conglomerados ###

sam <- S.SI(N, n) ## Muestreo de conglomerados con MAS, puede ser estratificado o sistemático.

muestra <-U[sam] ## para saber el nombre de la zona


#### filtrar tabla por conglomerados de la muestra

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

### juntar
Lucy <- rbind(Lucy1, Lucy2, Lucy3, Lucy4, Lucy5, Lucy6, Lucy7, Lucy8, Lucy9, Lucy10)

## se forma base de elementos muestrales filtrando zonas seleccionadas (censo dentro de la zona)
table(Lucy$Zone)

attach(Lucy)


table(Zone) ### se deben borrar los vacios
table(droplevels(Zone)) 

zona=droplevels(Zone) ## 
estima <- data.frame(Income, Employees, Taxes)

estimaI <- as.data.frame(T.SIC(estima,zona)) ## totaliza variables de interés por cada conglomerado
E.SI(N, n, estimaI)

###la notación de diapositivas es diferente a R:
### Ni = mi(diapositivas)=Mi

mi=estimaI$Ni
yi=estimaI$Income
Mbarra=M/N
ybarra=sum(yi)/sum(mi)

##### calcular tamaño muestral ############

dif=yi-(ybarra*mi)
sr_2=sum(dif^2)/(n-1)
B=25
D=(B^2)*(Mbarra^2)/4
n=(N*sr_2)/(N*D +sr_2)


######### despues de obtener el tamaño muestral se repite el procedimiento para el número de conglomerados identificados.



#######################################  
##### 5. Muestreo por etapas MAS-MAS
#####################################



data("Lucy")
attach(Lucy)

table(Lucy$Zone)

U=levels(Lucy$Zone)
N=length(U)
n=4 ### calcular como se hace en conglomerado

set.seed(987)
samc=S.SI(N,n)

muestrac=U[samc]

Lucy1 <- Lucy[which(Lucy$Zone==muestrac[1]),]
Lucy2 <- Lucy[which(Lucy$Zone==muestrac[2]),]
Lucy3 <- Lucy[which(Lucy$Zone==muestrac[3]),]
Lucy4 <- Lucy[which(Lucy$Zone==muestrac[4]),]

M1 <- dim(Lucy1)[1]
M2 <- dim(Lucy2)[1]
M3 <- dim(Lucy3)[1]
M4 <- dim(Lucy4)[1]



prop_mas= 0.2### se asigno arbitrariamente, se debería calcular por mas en cada UPM

m1=floor(M1*prop_mas)
m2 =floor(M2*prop_mas)
m3= floor(M3*prop_mas)
m4= floor(M4*prop_mas)

Mi=c(M1,M2,M3,M4)
mi=c(m1,m2,m3,m4)

sum(mi) ### total de unidades secundarias en muestra

sam1 <- S.SI(M1,m1)
sam2 <- S.SI(M2,m2)
sam3 <- s.SI(M3,m3)
sam4 <- S.SI(M4,m4)

muestra1 <- Lucy1[sam1,]
muestra2 <- Lucy2[sam2,]
muestra3 <- Lucy3[sam3,]
muestra4 <- Lucy4[sam4,]

muestra=rbind(muestra1,muestra2,muestra3,muestra4)

attach(muestra)
estima=data.frame(Income,Employees, Taxes)

Zone <- droplevels(muestra$Zone)

E.2SI(N,n,Mi,mi,estima,Zone)