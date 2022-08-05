
library(fpp2) ### datos
library(forecast) ## modelos y funciones  para series de Tiempo
library(urca) ## prueba de estacionariedad



###########################################################
###### serie marathon ###########################################
###################################################################

################################################
#####1. Volver serie estacionaria #############
#################################################


##### 1.1 estabilizar varianza ####


l1=BoxCox.lambda(marathon,lower=-10, upper=10)
l2=BoxCox.lambda(marathon)
l3=0


marathon_t1=BoxCox(marathon, l1)
marathon_t2=BoxCox(marathon, l2)
marathon_t3=BoxCox(marathon, l3)



par(mfrow=c(2,2))
plot(marathon,main="original")
plot(marathon_t1,main="lambda -3")
plot(marathon_t2,main="lambda -1")
plot(marathon_t3,main="lambda 0")


##### 1.2 Diferenciaciones #####

d0=ndiffs(marathon)
d1=ndiffs(marathon_t1)
d2=ndiffs(marathon_t2)
d3=ndiffs(marathon_t3)


marathon_d=diff(marathon,differences = d0)
marathon_t1_d=diff(marathon_t1,differences = d1)
marathon_t2_d=diff(marathon_t2,differences = d2)
marathon_t3_d=diff(marathon_t3,differences = d3)

par(mfrow=c(2,2))
plot(marathon_d,main="original")
plot(marathon_t1_d,main="lambda -3")
plot(marathon_t2_d,main="lambda -1")
plot(marathon_t3_d,main="lambda 0")


ps1=ur.kpss(marathon_d)
ps2=ur.kpss(marathon_t1_d)
ps3=ur.kpss(marathon_t2_d)
ps4=ur.kpss(marathon_t3_d)


summary(ps1)
summary(ps2)
summary(ps3)
summary(ps4)

#### ¿Cuál es la mejor? ####



###########################################################
########  2. identificar orden con ACF y PACF##############
###########################################################
par(mfrow=c(1,2))
ggtsdisplay(marathon_d)
ggtsdisplay(marathon_t1_d)
ggtsdisplay(marathon_t2_d)
ggtsdisplay(marathon_t3_d)

### candidatos: (2,1,3),(2,1,2), (2,1,1), (2,1,0) 

######################################################
########## 3. Comparar modelos #######################
####################################################


mod1=auto.arima(marathon, lambda=NULL , approximation=F, 
                stepwise=F, trace=T, max.order = 10)

mod2=auto.arima(marathon, lambda=-3, approximation=F,
                stepwise=F, trace=T, max.order = 10)

mod3=auto.arima(marathon, lambda='auto', approximation=F,
                stepwise=F, trace=T, max.order = 10)

mod4=auto.arima(marathon, lambda=0, approximation=F,
                stepwise=F, trace=T, max.order = 10)


summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

mod3$coef



######################### Mirar pronóstico #################
par(mfrow=c(1,2))
plot(forecast(mod1))
plot(forecast(mod2))
plot(forecast(mod3))
plot(forecast(mod4))




########### comparar con modelo de holt ####

mod5=holt(marathon)
summary(mod5)
plot(forecast(mod5))


#######################################################
#######################validar supuestos ######################

ggtsdisplay(mod1$residuals)



###### validación cruzada #######


arima_f=function(x,h){
  
  Arima(x,order=c(2,1,0))%>%
    forecast::forecast(h=h)
}

e=tsCV(marathon,arima_f,h=20)

eh1=e[,1]
mape=mean(abs(eh1/marathon),na.rm=T)*100

eh2=e[,2]
mape=mean(abs(eh2/marathon),na.rm=T)*100


eh6=e[,6]
mape=mean(abs(eh6/marathon),na.rm=T)*100

eh10=e[,10]
mape=mean(abs(eh10/marathon),na.rm=T)*100


par(mfrow=c(2,2))

hist(eh1,breaks=20)
hist(eh2,breaks=20)
hist(eh6,breaks=20)
hist(eh10,breaks=20)

mean(marathon)


######## ajustar modelo para eeadj #####

eadj=elecequip %>% stl(s.window='periodic') %>% seasadj() 

plot(eadj)






