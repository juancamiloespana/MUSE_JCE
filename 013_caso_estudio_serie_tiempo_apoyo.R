
install.packages("gt")

library(gt)
library(plotly)
library(astsa)
  
suc=c(1:700)
cap=round(runif(700,800,3000),0)*1000000
  
capac_suc=data.frame('suc'=suc,'cap'=cap)

plot_ly(x=cap, type="histogram")


##############################################################################
####################### crear base de datos de flujo de efectivo ###########
############################################################################

df_flujo=data.frame()



n_suc=50
f_ini=as.Date('2021/1/1')
f_fin=as.Date('2022/08/23')
fechas=seq(from=f_ini, to=f_fin, by='day')
n_dias=as.numeric(f_fin-f_ini+1)

i=1
for(i in 1:n_suc)
{



d=sample(x=0:1,size=1,prob=c(0.5,0.5))
D=sample(x=0:1,size=1,prob=c(0.5,0.5))
coef=round(runif(4,-0.8,0.8),2)
ar=sample(x=c(0,coef[1]),size=1,prob=c(0.5,0.5))
ma=sample(x=c(0,coef[2]),size=1,prob=c(0.5,0.5))
sar=sample(x=c(0,coef[3]),size=1,prob=c(0.5,0.5))
sma=sample(x=c(0,coef[4]),size=1,prob=c(0.5,0.5))

serie=sarima.sim(D=D,d=d, S=30, ar=ar, ma=ma, sar=sar,sma=sma, n=600)

porc_media=round(runif(1,.0001,0.005),5)
porc_media2=round(runif(1,-0.05,0.05),5)
media=porc_media*cap[4]
media2=porc_media2*cap[4]

ser=media*serie +media2

suc=rep(i,n_dias)

df_flujo_suc=data.frame('suc'=suc,'fechas'=fechas, 'flujo_efe'=as.numeric(ser))

df_flujo=rbind(df_flujo,df_flujo_suc)


}

write.csv(capac_suc,"capa_suc.csv",row.names=F)
write.csv(df_flujo,"flujo_efectivo.csv",row.names=F)


#######Generación de saldo #####
url_cap='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/capa_suc.csv'

cap=read.csv(url_cap)
cap=cap$cap[1:50]
fecha=rep(f_fin,50)
suc=c(1:50)
saldo=runif(50,0.01,0.99)*cap

saldos=data.frame(fecha,suc, saldo)

write.csv(saldos, "13_caso_estudio/saldo_suc.csv",row.names=F)
