### cargar librerias ####
library(dplyr)
## Este código carga las funciones del script que se llame
source("D:\\cod\\R_muse\\caso_estudio\\funciones.R") #se usan dos backslash

main=function(num_suc, ini_sim=517, h=2)
  {
  

    ######## cargar los datos #####  
      
    ruta_sucs='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/caso_estudio/flujo_efectivo.csv'
    df_sucs=read.csv(ruta_sucs) ### serie de tiempo flujo de sucursales 
    
    ruta_cap='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/caso_estudio/capa_suc.csv'
    cap_sucs=read.csv(ruta_cap) ### lista con capacidad de cada sucursal
    
    ruta_saldo='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/caso_estudio/saldo.csv'
    saldos=read.csv(ruta_saldo) ### lista con saldo de sucursal 1 de jun de 2022
    
    list_suc= unique(df_sucs$suc) ## lista de las sucursales con serie de flujo
    num_suc=min(num_suc, length(list_suc)) ## para cuantas suc se va a realizar el proceso
    
  
    df_acum=data.frame() ## para guardar todos los resultados de simulaciones

    for( suc in list_suc[1:num_suc])  ## hacer el proceso para cada sucursal
    {

   
      datos_suc=sel_suc(df_sucs,cap_sucs,saldos, cod_suc=suc) ## todos los datos de la suc indicada, en una lista: cap, flujo, saldo_1_jun
      
      ts_flujo=freq_ts(datos_suc$flujo_suc) ### encuentra la frecuencia y convierte a ts
      
      datos_mod=escoger_modelo(ts_flujo) ### escoge el mejor modelo para la sucursal
      
  
      res_sim= simu_datos(datos_suc =datos_suc, ### realiza la simulación apartir de la periodo definido
                                datos_mod = datos_mod,
                                ini_sim=ini_sim,
                                h=h)
      
   
      
      ## dentro se llama pedido, para cada dia
      #####dentro de pedido se llama reajustar modelo
      res_sim$cod=suc
      res_sim$sup_cap=as.integer(res_sim$saldo>datos_suc$cap_suc)  
      res_sim$sin_efe=as.integer(res_sim$saldo<=0)  
      res_sim$n_ped=as.integer(res_sim$aprov+res_sim$recol >0)
      
      df_res_sim= na.omit(data.frame(res_sim))

      df_acum=rbind(df_acum,df_res_sim)
      
      
    }
  
    return(df_acum)

  }



datos_p=main(num_suc=3, h=2)
datos=datos_p




c_ped=100
c_sup=200  
c_sin_efe=400

por_suc=datos%>%group_by(cod)%>%
  summarise(n_ped=sum(n_ped)*c_ped, 
            n_sup_cap=sum(sup_cap)*c_sup, 
            n_sin_efe=sum(sin_efe)*c_sin_efe)

total=datos%>%
  summarise(n_ped=sum(n_ped)*c_ped, 
            n_sup_cap=sum(sup_cap)*c_sup, 
            n_sin_efe=sum(sin_efe)*c_sin_efe)

sum(total)


suc=2
cap=cap_sucs[suc,][[2]]/1000000

datos1=datos[datos$cod==suc,]
plot(datos1$saldo, ylim=c(0,cap))
abline(h=cap, col='red', lwd=2)

flujo=df_sucs[df_sucs$suc==suc,]
plot(flujo$flujo_efe/1000000, type='l')
