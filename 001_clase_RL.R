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
t=c(1:200)
pib2=4+t +5*t^2

df=data.frame(pib2)
write.csv(df,"pib2.csv")



####### ejercicio 2 PIB2

url='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/data/pib2.csv'

pib2=read.csv(url)

pib_ts2=ts(pib2$pib2, start=1500, frequency=1)

plot(pib_ts2) ## la gráfica muestra una relacion cuadratica entre la serie y el tiempo
### se necesita un componente de tiempo al cuadrado

ini=start(pib_ts2)[1] ## ano de inicio de la serie
fin=end(pib_ts2)[1] ### ano fin de la serie

t=c(ini:fin) ### tiempo utilizando ano y fin 
t2= t**2 ### componente cuadratico de la serie


rl=lm(pib_ts2~t+t2) ### ajsutar el modelo usando t y t2 (t elevado al cuadrado)
abline(rl) ### el modelo no grafica sobre la serie porque esta en tres dimensiones

summary(rl)


t_pred=c(1560,1830)  ## crear vector de tiempos a predecir
t2_pred=t_pred**2  ## vector de tiempos a predecir elevado al cuadrado

data_pred=data.frame(t=t_pred,t2=t2_pred)  ## t y t2 llevarlos a data frame, para ser usados en funcion predic

predict(rl, newdata=data_pred)  ### predecir anos registrados

plot(rl)

checkresiduals(rl) 
### los graficos se ven muy extraños porque la serie tiene muy poco variabilidad y eso dana la escala del modelo, sin embargo el supuesto de independencia se cumple, el de varianza constante salvo el primer valor, parece cumplirse y normalidad pareceria que no 

