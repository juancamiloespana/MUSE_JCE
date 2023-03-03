install.packages('sma')
library(smooth)
library(forecast)
install.packages('smooth')

library(smooth)
library(forecast)


#######

#Evaluar graficamente las series 

  ##Lynx, 
  ###demanda (inicia en febrero de 1820)  es mensual
  #valor_acciones_Ecopetrol (inicia 1959) es anual 

## Analizar con acf y decir cuál es la más apropiada para sma.

#Ajustar modelo de sma para serie valor accion ecopetrol y probar cambiando los siguientes argumentos:
## Order
## Holdout
## silent

### Comparar los modelos para las tres series 
###Basado en la serie de valores de ecopetrol:

    ###decir cual es el pronostico para 1995
    ### decir cuales es el IC del 97% en el pronostico de 1995


