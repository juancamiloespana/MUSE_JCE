install.packages("TeachingSampling")

library(TeachingSampling) ### funciones para extraer la muestra segun tipo de muestreo y tiene funcion para calcular total población y desviación


data("BigLucy") ### este es el marco de muestreo 

N=dim(BigLucy)[1]
sd= 300 ### esta en millones y está definido teoricamente
nsd=2 ### una confianza del 95%
B= 30   ####si es pequeño muestra es muy grande


D= (B/nsd)^2  
n= N*(sd^2)/((N-1)*D +sd^2) ## tamaño de muestra, a cuantas empresas se deberia encuestar
n=round(n)

sam= S.SI(N,n) ### este me selecciona las filas para la muestra
sam_datos= BigLucy[sam, 'Income'] ### ingresos de las empresas encuestadas


#### calcular estimadores

estimaciones=E.SI(N,n, sam_datos) ### solo muestran total poblacional

### para sacar media

media=estimaciones[1,2]/N
sdmedia= estimaciones[2,2]/N


#############para sistematico ######

###extracción

a=round(N/n) ## calcular numero de grupos

sam=S.SY(N, a) ### extraer muestra sitematica
sam_datos2=BigLucy[sam,'Income']


estimaciones2= E.SY(N,a, sam_datos2) ### estimacion total poblacional y su varianza

####media

media2=estimaciones2[1,2]/N
sdmedia2=estimaciones2[2,2]/N



####MAE 


Nst=table(BigLucy$Level) ## tamaño por estrato
sdst=c(378,131,115) ##desviacion por estrato
alphast=(Nst/N) ###proporciones por estrato

D= (B^2)/4

num= sum((Nst^2)*(sdst^2)/alphast)
den= (N^2)*D + sum(Nst*(sdst^2))

n= num/den ### muestra total
nst= round(alphast*n) ### muestra por estratificado


#####
attach(BigLucy) ####variable de base de datos queda independiente


sam3=S.STSI(Level, Nst, nst)
sam_datos3=BigLucy[sam3,c("Income", "Level")]
sam_datos3=data.frame(sam_datos3)
attach(sam_datos3)


estimaciones3=E.STSI(Level, Nst, nst, sam_datos3$Income)
estimaciones3




