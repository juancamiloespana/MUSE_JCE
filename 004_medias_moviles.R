#install.packages('smooth')
#install.packages('fpp')

library(smooth) ## paquete con funcion sma_old
library(forecast)
library(fpp)### paquete con series de tiempo

##########################################################
################  Ejericio  ##############################
##########################################################

#Para el siguiente ejercicio Utilizara las siguientes series:

  #valor accion ecopetrol (inicia 1959 marzo) es mensual 
  #demanda (inicia el 45 dia de 1820)  es diaria frecuencia anual
  #Serie a10 del paquete fpp venta mensual

#1 Garantizar que las series  estan en formato ts y con frecuencia diferente de 1
#2. Evaluar graficamente las siguientes series y decir que componentes parecen tener
  ### 1.1 Serie vs el tiempo
  ### 1.2 ACF y PACF
  ### 1.3 descomponer la serie
#3. de acuerdo al analisis anterior a cual serie le funcionaria mejor un pronostico por medias moviles
#4. ajustar modelo de medias moviles para las tres series y analizar


