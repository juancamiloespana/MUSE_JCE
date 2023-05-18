
### cargar librerias
library(smooth) ## paquete con funcion sma_old
library(forecast) ### modelos holt, hw y ses
library(Metrics)

sel_suc=function(df_sucs,cap_sucs, saldos, cod_suc)
{

  
  
  df_suc=df_sucs[df_sucs$suc ==cod_suc,] ###serie de tiempo de sucursal 17 (escogida al azar)
  flujo_suc=df_suc$flujo_efe/1000000
  fechas=df_suc$fechas
  
  cap_suc=cap_sucs[cap_sucs$suc==cod_suc,] ### filtrar para suc seleccionada
  cap=cap_suc$cap/1000000 ## para que sea mas legible, se trabajara con cifrasn en millones

  saldo_suc=saldos[saldos$suc==cod_suc,] ### filtrar para suc seleccionada
  saldo=saldo_suc$saldo/1000000 # ## para que sea mas legible, se trabajara con cifrasn en millones

  salida=list('saldo_2022_06_01'=saldo, 'cap_suc'=cap, 
              'flujo_suc'=flujo_suc,
              'fechas'=fechas)  
 return(salida) 
}

freq_ts=function(serie)
{
  
  freq_cand=findfrequency(serie)
  
  list_freq=c(7, 15, 30)
  diff=abs(list_freq -freq_cand)
  index_min_diff = which.min(diff)
  freq=list_freq[index_min_diff ]
  ts_flujo=ts(serie, frequency=freq)
  
  
  return(ts_flujo)
  
}

escoger_modelo=function(serie)
{
  best_ind=999999999999999 ###indicador a medir
  best_mod=NA ### modelo ajustado ganador
  n_best_mod=NA ### nombre del modelo ganador
  
  list_modelos=list('sma'=sma_old, 'ses'= ses,
                    'holt'= holt, 'hw'=hw,
                    'arima'=auto.arima)
  
  
  for (n_mod in names(list_modelos)){
    
    
    fun_mod=list_modelos[[n_mod]]
    if(n_mod=='hw'& frequency(serie)>15)
    {
     serie_hw=ts(serie, frequency=15)
     mod_cand=fun_mod(serie_hw)
     
    }else
    {
      mod_cand=fun_mod(serie, lambda='auto')
      
      
    }
    
    ajustados=fitted(mod_cand)
    ind_cand=rmse(as.vector(serie), as.vector(ajustados))
   
    
   
    if(ind_cand<best_ind)
    {
      best_mod=mod_cand
      n_best_mod=n_mod
      best_ind=ind_cand
    }
    
  }
  
  salida=list('mod'=best_mod, 
              'n_mod'=n_best_mod,
              'ind'=best_ind)
  
  return(salida)
}

reajustar_mod=function(serie, datos_mod)
{

  mod=datos_mod$mod
  n_mod=datos_mod$n_mod
  list_mods=list('sma'=sma_old, 'ses'= ses,
                 'holt'= holt, 'hw'=hw, 
                 'arima'=Arima)
  
  fun_mod=list_mods[[n_mod]]
  mod_reajustado=fun_mod(serie, model=mod)
  return(mod_reajustado)
  
}

pedido=function(datos_mod, datos_suc, saldo_dia_ant,flujo_hast_dia_ant, h=2)
{ ### ingresa saldo suc, capacidad y serie de tiempo
  

  cap_suc=datos_suc$cap_suc
  saldo_suc=saldo_dia_ant
  flujo=flujo_hast_dia_ant

  
  modelo=reajustar_mod( flujo, datos_mod)

  

  
  if(class(modelo)[1]=='smooth')##validar si es medias moviles para extraer diferente el pronostico
  { 
    pron=sum(modelo$forecast[1:h]) ## pronostico h dias 
  }else{
    pron_a=forecast(modelo)# pronosticos modelo e intervalos
    pron=sum(pron_a$mean[1:h]) ## solo pronostivo son interv h periodos
   
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

simu_datos=function(datos_mod, datos_suc, ini_sim=517, h=2)
{
  cap=datos_suc$cap ## la capacidad no cambia
  s_flujo=datos_suc$flujo_suc
  
  saldo_dia=numeric() ###  iniciarlizar un vector para acumular los saldos de simulación 
  aprov_dia=numeric() ###  iniciarlizar un vector para acumular los aprovisionamientos
  recol_dia=numeric() ###  iniciarlizar un vector para acumular las recolecciones
  
  saldo_dia[ini_sim] =datos_suc$saldo_2022_06_01 ### saldo 1 de junio ida ant
  
  
  for (j in (ini_sim+1):(ini_sim+h-1))
  {
    saldo_dia[j] =max(saldo_dia[j-1] + s_flujo[j],0) ### saldo dia actual el pedido no llega el mismo dia entonces no depende de aprovisionamiento
    aprov_dia[j]=0
    recol_dia[j]=0
    
  }
# el contador de flujo es diferente porque tiene la serie en el pasado para el modelo


  n_flujo=length(s_flujo)


### inicia en 2 porque el saldo del dia 1 está definido es el del 
  for (dia_act in (ini_sim+1):(n_flujo) )
  {

    dia_ant=dia_act-1
    dia_h=dia_ant+h
    
    s_flujo_dia_ant =s_flujo[1:dia_ant] ##serie flujo hasta dia anterior al que estoy iniciando 1 de junio
    ts_flujo_dia_ant=freq_ts(s_flujo_dia_ant)  ### convertir serie  anterior en ts
  
    saldo_dia_ant_aj=saldo_dia[dia_ant] + aprov_dia[dia_act]-recol_dia[dia_act] ### yo sé las ordenes que tengo para el dia act entonces debo descontarlas
 
    ped=pedido(datos_mod, datos_suc,  saldo_dia_ant_aj,ts_flujo_dia_ant,h=h)
    
    aprov_dia[dia_h]=ped$a
    recol_dia[dia_h]=ped$r
  
     
      ### actualizar saldo dias siguientes #####
    flujo_dia_h=s_flujo[dia_h]
    saldo_dia[dia_h] = max(saldo_dia[dia_act] + flujo_dia_h + aprov_dia[dia_h] - recol_dia[dia_h],0)


  }


  salida=list('saldo'=saldo_dia,'aprov'= aprov_dia,'recol'= recol_dia)
  return(salida)

}





