

library(TeachingSampling)


data(Lucy) ##Base de datos, marco de muestreo.
attach(Lucy) 

N <- dim(Lucy)[1]  ##Tamaño población
set.seed(123) ## para que los calculos sean iguales


### 1. Piloto para estimación de tamaño muestra  ####

sam <- sample(N,100) ## extraer muestra piloto de 100 para varianza muestral y calculo de tamaño de muestra, la pilote se extrae por MAS.


emp.pilot <- Employees[sam] ###Variable de interés.


mean(emp.pilot) ##Media de muestra piloto
var(emp.pilot) ## varianza muestra piloto
sd(emp.pilot)


n_sd=3  ## Número de desviaciones 
s=sd(emp.pilot)
B=10 ## ##Margen de error máximo aceptable (está en millones) lo define el investigador
D=(B/(2*n_sd))^2
n=(N*s^2)/(((N-1)*D)+s^2)  ## Muestra con población finita dist normal



### 2. Extracción de muestra con tamaño definido  ####

n=round(n,0)
sam <- S.SI(N,n) ## función para extraer MAS sin reemplazo
muestra <- Lucy[sam,]   ## base de datos con la muestra
attach(muestra)

### 3. Estimaciones de parámetros de interés ###

estima <- muestra[,c("Employees")] # base con las columnas para hacer estimación


A=E.SI(N,n,estima)## función para hacer estimaciones MAS sin reemplazo del total con con HT



###########################################
## 2. Muestreo Aleatorio Sistemático ####
#########################################3


data(Lucy) ##Base de datos, marco de muestreo.
attach(Lucy) 

set.seed(123)
N <- dim(Lucy)[1]
n=n ## según MAS
a <- floor(N/n) ## definir número de grupos en los que se divide la población


sam <- S.SY(N, a) ## función para extracción de muestra
muestra <- Lucy[sam,] ##Base con muestra de datos
attach(muestra)
estima <- data.frame(Employees) # variables de interés
attach(estima)

E.SY(N, a, estima)  #Estimación muestreo sistemático HT sin reemplazo



#########################################
##### 3. Muestreo Aleatorio Estratificado
##########################################



## se selecciona una variable categórica que influya en las de interés. 
### en este caso la variable selecionada es: SPAM

data(Lucy)
attach(Lucy)
N <- dim(Lucy)[1]

table
table(SPAM)

boxplot(Employees~SPAM)

## Tamaño de población de cada estrato

N1 <- summary(SPAM)[[1]]
N2 <- summary(SPAM)[[2]]


Nst <- c(N1,N2)
Nst


#### calcular alfas o proporciones de observaciones en cada estrato

ano=N1/N
ayes=N2/N


a=c(ano,ayes)

n=n

## calcular  tamaño de muestra y se divide proporcional a los valores poblacionales

n1 <- round(n*ano)
n2 <- round(n*ayes)
nst <- c(n1,n2)

sam <- S.STSI(SPAM, Nst, nst)
muestra <- Lucy[sam,]
attach(muestra)


estima <- data.frame(Employees)
E.STSI(SPAM, Nst, nst, estima)

