


library(forecast) ### para modelos 
library(dplyr) ### para manipulación

cargar_bases=function()
{
  url_flujo='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/13_caso_estudio/flujo_efectivo.csv'
 
  url_cap='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/13_caso_estudio/capa_suc.csv'
  url_saldo='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/13_caso_estudio/saldo_suc.csv'
  
  df_flujo=read.csv(url_flujo)
  saldo=read.csv(url_saldo)
  caps=read.csv(url_cap)
  
  dfs=list('flujos'= df_flujo,'saldos'=saldo, 'cap'= caps)
  return(dfs)
}

ajustar_modelo=function(df_flujo, suc1,freq=NULL)
{
 

  

  flujo_suc=subset(df_flujo,suc==suc1)
  flujo_suc=flujo_suc%>%arrange(fechas) ### garantizar orden
  
  ##### determinar frecuencia
  ##### probar dos frecuencias findfrequency
  if(is.null(freq))
  {
  freq=findfrequency(flujo_suc$flujo_efe)
  }
  
  
  ##### convertir a serie de tiempo ####
  
  flujo_ts=ts(flujo_suc$flujo_efe,frequency = freq)
  
  #### ajustar modelos ####
  
  modelo= auto.arima(flujo_ts)
  
  
  return(modelo)
  
  
  
}

medir_modelo=function(modelo, sign, h)
{
  ##### gauardar la información del modelo ajsutado para crossvalidation
  
  p=modelo$arma[1]
  q=modelo$arma[2]
  P=modelo$arma[3]
  Q=modelo$arma[4]
  d=modelo$arma[6]
  D=modelo$arma[7]
  m=modelo$arma[5]
  
  #### función para cross validation 
  arima_f=function(x,h,p,d,q,P,D,Q,m){
    
    Arima(x,order=c(p,d,q), seasonal=list(order=c(P,D,Q),period=m))%>% 
      forecast::forecast(h=h)
  }
  
  
  ### resultados con argumentos de modelo ganador
  
  e2=tsCV(modelo$x,h=h, arima_f,p=p,d=d,q=q,P=P,D=D,Q=Q,m=m)
  mape=mean(abs(e2[,h]/modelo$x),na.rm=T)*100
  
  cr=checkresiduals(modelo,plot=F)
  
  
  if(cr$p.value >=sign)
  {
    residuales= "Cumple supuestos"
  }else
  {
    residuales="No cumple supuestos"
  }  
  
  resultado=list( "mape_porcentaje"=mape,"supuestos"=residuales)

}

definir_solicitudes= function(saldo, caps, h=2,suc1,conf=.95,fecha,modelo)
{
  
 saldo_suc=saldo[saldo$suc==suc1 & saldo$fecha==fecha,'saldo']
 cap_suc=caps[caps$suc==suc1,"cap"]
 pronos= forecast(modelo,h=h,level=conf)
 sum_pronos_up=sum(pronos$upper)
 sum_pronos_low=sum(pronos$lower)
 sum_pronos=sum(pronos$mean)
 
 saldo_pronos_prom=sum_pronos+saldo_suc
 saldo_pronos_upp=sum_pronos_up+saldo_suc
 saldo_pronos_low=sum_pronos_low+saldo_suc
 
 solicitud=case_when(
   saldo_pronos_upp>=cap_suc ~"Recolección",
   saldo_pronos_low<=0~"Aprovisionamiento",
   TRUE~"Ninguno")
 
 solicitud=list('solicitud'=solicitud,
                "saldo_actual"=saldo_suc,
                "saldo_max_pro"= saldo_pronos_upp, 
                "saldo_min_pro"=saldo_pronos_low,"cap_suc" =cap_suc,
                'por_uso_cap_prom'=round(saldo_pronos_prom*100/cap_suc,1))
 
 return(solicitud)
}  


analizar_suc=function(suc1,h=2, fecha, sign=0.05, conf=.95,freq=NULL)
{
  
  dfs=cargar_bases()
  
  modelo=ajustar_modelo(df_flujo=dfs$flujos,suc1=suc1,freq=freq)
  
  medidas=medir_modelo(modelo=modelo, sign=sign,h =h)
  
  solicitud=definir_solicitudes(saldo=dfs$saldos,caps=dfs$cap, h=h,suc1=suc1,fecha=fecha,modelo=modelo)
  info_suc=data.frame(solicitud,medidas)
  resultado=list("modelo"=modelo, 'info_suc'=info_suc)
  
  return(resultado)

}


analizar_todas=function(h=2,sign=0.05,fecha, conf=.95,max_suc=NULL, freq=NULL)
{
  dfs=cargar_bases()
  
  sucs=unique(dfs$flujos$suc)
  

  if(!is.null(max_suc)){
  sucs=sucs[1:max_suc]
  } 
 
  
   modelos=list()
   medidas=data.frame()
  
  
  for(i in sucs )
  {
    res_suc=analizar_suc(suc1=i, fecha=fecha, sign=sign, conf=conf,freq=freq)
    
    modelos[[suc1]]=res_suc$modelo

    info_suc=data.frame("cod_suc"=i,res_suc$info_suc)
    medidas=rbind(medidas,info_suc)
    
  }
   
   res=list('modelos'=modelos,'info_suc'=medidas)
   return(res)
  
}







