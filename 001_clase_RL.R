install.packages('forecast')
library(forecast)

####Leer tabla desde github

archivo='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/pib.csv'

pib= read.csv(archivo)


####Convertir a formato ts

pib_ts=ts(pib$pib2, start=1820, frequency= 1) ## convertir un vector de datos en una serie de tiempo

pib_ts=ts(pib$pib2, start=c(1820,1), frequency= 2) ## la frecuencia indica en cuantos periodos se divide el ano 
###el argumento start o end pueden ser un vector en caso de indicar el a?o y el periodo en el que inica la serie

print(pib_ts) ## ver las caracter?sticas de la serie

start(pib_ts)  #### para saber cuando empieza la serie
end(pib_ts)  ####saber cuando termina
frequency(pib_ts)

plot(pib_ts)


##### crear variable tiempo ###

t=c(1820:2019) 

t1=c(1:200) # se puede representar el tiempo como consecutivo

#### probar que la serie y el tiempo tengan la misma longitud
length(t)
length(pib_ts)

rl=lm(pib_ts~t)
abline(rl)

summary(rl)

df_pred=data.frame(t=c(1850,2024))

predict(rl, newdata =df_pred )

df_entre=data.frame(pib_ts,t)

####### real vs predicción en el año 1850
real=419.4443
pred=424.1161

####Validar supuestos ####

checkresiduals(rl)

#######

