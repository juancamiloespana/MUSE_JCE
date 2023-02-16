
library(fpp2) ### para cojunto de datos
library(forecast)



### mcopper
###a10
###arrivals[,3]
### auscafe
  
plot(auscafe)



########

par(mfrow=c(1,2))
plot(eggs)
lambda=BoxCox.lambda(eggs,lower=-10,upper=5)
lambda1=0

eggs_t1=BoxCox(eggs,lambda)
eggs_t2=BoxCox(eggs,lambda1)

par(mfrow=c(1,3))
plot(eggs)
plot(eggs_t1)
plot(eggs_t2)

ndiffs(eggs)

ur.kpss(eggs)%>%summary()

eggs_d=diff(eggs,differences=1)
eggs_t1_d=diff(eggs_t1,differences=1)
eggs_t2_d=diff(eggs_t2,differences=1)



plot(eggs_d)
plot(eggs_t1_d)
plot(eggs_t2_d)

eggs_d%>%ur.kpss()%>%summary()
eggs_t1_d%>%ur.kpss()%>%summary()
eggs_t2_d%>%ur.kpss()%>%summary()

####### 1. Gráfica una serie con 36 números aleatorios de una normal estándar y grafíquelos, otra serie con 360 y otra serie con 1000


par(mfrow=c(1,3))

s1=rnorm(36)
s2=rnorm(360)
s3=rnorm(1000)

Acf(s1)
Acf(s2)   
Acf(s3)  


ggtsdisplay(visitors)


plot(visitors)
l=BoxCox.lambda(mcopper)
  
s2=BoxCox(mcopper,l)          

plot(s2)     

plot(wmurders)
ndiffs(wmurders)

            
            
            
            