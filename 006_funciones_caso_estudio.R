
### cargar librerias

library(smooth) ## paquete con funcion sma_old
library(forecast) ### modelos holt, hw y ses


cod_suc = 1
ruta_sucs='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/caso_estudio/flujo_efectivo.csv'
df_sucs=read.csv(ruta_sucs) ### serie de tiempo de todas las sucursales
df_suc=df_sucs[df_sucs$suc ==cod_suc,] ###serie de tiempo de sucursal 17 (escogida al azar)

ruta_cap='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/caso_estudio/capa_suc.csv'
cap_sucs=read.csv(ruta_cap) ### serie de tiempo de todas las sucursales
cap_suc=cap_sucs[cap_sucs$suc==cod_suc,] ### filtrar para suc seleccionada
cap=cap_suc$cap

ruta_saldo='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/caso_estudio/saldo.csv'
saldos=read.csv(ruta_saldo) ### serie de tiempo de todas las sucursales
saldo_suc=saldos[saldos$suc==cod_suc,] ### filtrar para suc seleccionada
saldo=saldo_suc$saldo
###filtrar serie y guardar

df_suc2=df_suc[df_suc$fechas<='2022-06-01',] # filtrar el flujo hasta la fecha del saldo
ts_suc2=ts(df_suc2$flujo_efe,start=c(1,5), frequency = 7) ### corresponde a estacionalidad de 7 días, inicia en semana 1 dia 2, cada7 dias cambia de semana


escoger_modelo=function(ts_suc){ #### nombrar funcion y definir entrada
#### inicializar valores

best_aic= 9999999999  ### para medir mejor aic encontrado
best_modelo=NA ### para guardar modelo con mejor aic

####probar candidato 1

mod_cand=sma_old(ts_suc) ## modelo candidato medias moviles
aic_cand=mod_cand$ICs[[1]] ### extraer criteriio de informacion

#### esta funcion decide si el aic del modelo candidato mejora el mejor aic y actualiza el mejor aic y el mejor modelo
if(aic_cand>best_aic){
  best_aic=aic_cand
  best_modelo=mod_cand
}


##### probar candidato 2
mod_cand=ses(ts_suc) ## modelo candidato medias moviles
aic_cand=mod_cand$model$aic ### extraer criteriio de informacion

#### esta funcion decide si el aic del modelo candidato mejora el mejor aic y actualiza el mejor aic y el mejor modelo
if(aic_cand<best_aic){
  best_aic=aic_cand
  best_modelo=mod_cand
}



##### probar candidato 3
mod_cand=holt(ts_suc) ## modelo candidato medias moviles
aic_cand=mod_cand$model$aic ### extraer criteriio de informacion

#### esta funcion decide si el aic del modelo candidato mejora el mejor aic y actualiza el mejor aic y el mejor modelo
if(aic_cand<best_aic){
  best_aic=aic_cand
  best_modelo=mod_cand
}



###probar candidato 4

mod_cand=hw(ts_suc) ## modelo candidato medias moviles
aic_cand=mod_cand$model$aic ### extraer criteriio de informacion

#### esta funcion decide si el aic del modelo candidato mejora el mejor aic y actualiza el mejor aic y el mejor modelo
if(aic_cand<best_aic){
  best_aic=aic_cand
  best_modelo=mod_cand
}

return(best_modelo) ### devuelve el mejor modelo (el modelo es un objeto con toda la informacion del modelo)

}

mod=escoger_modelo(ts_suc=ts_suc2) ### para probar la funcion, se le da una serie y prueba sma, ses, holt y hw y devuelve el de mejor aic

summary(mod)


pedido=function(cap_suc,saldo_suc, y){ ### ingresa saldo suc, capacidad y serie de tiempo
  
  modelo=escoger_modelo(y)##ajustar modelo con funcion realizada
  
  if(class(modelo)=='smooth'){ ##validar si es medias moviles para extraer diferente el pronostico
  pron=modelo$forecast[1] ## pronostico 1 dias de flujo 2022-06-02
  }else{
    pron=modelo$mean[1]# pronostico 1 dias de flujo 2022-06-02
  }
  
  pron_saldo=saldo_suc+pron ## calcular saldo pronosticado 2022-06-02
  punto_restabl=cap_suc/2 ##3 punto de restablecimiento de inventario
  
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


ped=pedido(cap=cap, saldo ,ts_suc2)
ped
