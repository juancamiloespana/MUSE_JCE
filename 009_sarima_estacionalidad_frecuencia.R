l1=ts(lynx, start=c(1,6), frequency = 7)

print(l1)

l2=ts(lynx, start=c(1,1), frequency = 3)
print(l2)


plot(elec[1:48], type='l')
Acf(elec,lag.max = 50)

findfrequency((elec))


l3=ts(elec, start=(1956, 2), frequency=12)

autoplot(elec)

nsdiffs(elec) ### requiere diferenciacion estacional
## este es el D


ndiffs(elec) ## requiere una diferenciacion (pero es mejor después de diferenciación estacional)


elec_ds=diff(elec,lag=12, differences = 1) ### lag se pone la frecuencia o periodo estacional

autoplot(elec_ds)

ndiffs(elec_ds) ### verificar si despues de diferenciacion estaciona requiere las normales
### da d minuscula

elec_dd=diff(elec_ds, differences = 1)
autoplot(elec_dd)





