library(forecast) ##paquete con modelos ses, holt y hw
library(smooth) ###modelo sma

ruta_suc='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/caso_estudio/flujo_efectivo.csv'


flujos=read.csv(ruta_suc)
flujo_17=flujos[flujos$suc==17,]
y=ts(flujo_17$flujo_efe,start=c(2021,1), frequency= 7)

escoger_modelo=function(y){

  best_aic=999999999
  modelo_candidato=sma_old(y)
  aic_candidato=  modelo_candidato$ICs[1]

  if (aic_candidato <best_aic) {
    best_aic=aic_candidato
    modelo=modelo_candidato
  } 
  
  modelo_candidato=ses(y)
  aic_candidato=modelo_candidato$model$aic

  if (aic_ses<best_aic){
    best_aic=aic_candidato
    modelo=modelo_candidato

  }  
  
  modelo_candidato=holt(y)
  aic_candidato=modelo_candidato$model$aic
  
  if (aic_ses<best_aic){
    best_aic=aic_candidato
    modelo=modelo_candid
  }
  
  modelo_candidato=hw(y)
  aic_candidato=modelo_candidato$model$aic
  
  if (aic_ses<best_aic){
    best_aic=aic_candidato
    modelo=modelo_candidato

  }
 
return(modelo)
   
}


flujos=read.csv(ruta_suc)
flujo_17=flujos[flujos$suc==35,]
y=ts(flujo_17$flujo_efe,start=c(2021,1), frequency= 7)


modelo=escoger_modelo(y)
summary(modelo)
  

###### funcion hacer pedido

ruta='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/caso_estudio/saldo.csv'
saldos=read.csv(ruta)
saldo_suc=saldos[saldos$suc==17,]$saldo

ruta='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/caso_estudio/capa_suc.csv'
cap=read.csv(ruta)
cap_suc=cap[cap$suc==17,]$cap


flujos=read.csv(ruta_suc)
flujos$fechas=as.Date(flujos$fechas, format="%Y-%m-%d")
flujo_suc=flujos[flujos$suc==17 & flujos$fechas<="2022-06-01",]
y=ts(flujo_suc$flujo_efe,start=c(1,1), frequency= 7)


pedido=function(cap_suc,saldo_suc, y){
modelo=escoger_modelo(y)
pron=modelo$forecast[1] ## pronostico 2 dias de flujo 2022-06-03
pron_saldo=saldo_suc+pron
punto_restabl=cap_suc/2
if( pron_saldo>=cap_suc){
recol=pron_saldo-punto_restabl
aprov=0
}else if (pron_saldo<=0){
  
  aprov=punto_restabl
  recol=0
  
  
}else{
  aprov=0
  recol=0
  
}
pedido=list('a'=aprov,'r'= recol)
return(pedido)
}

ped=pedido(cap_suc= 2000591414, saldo_suc=saldo_suc, y)

  






