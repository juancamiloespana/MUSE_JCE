library(TeachingSampling)

data(Lucy) ## cargar los datos


### hacer piloto para calcular la varianza muestral de la variable de interés que es employees

##vamos a hacer el piloto con 15 empresas
N=dim(Lucy)[1]
n_piloto=15



set.seed(123)
sam=S.SI(N,n_piloto)### solo se extraen las filas del marco de muestreo que se seleccionaron para el piloto

muestra=Lucy[sam,] ## con la información disponible se envían las encuestas

s=sd(muestra$Employees) ## desviación muestral calculada con piloto


### tamaño muestral calculado de acuerdo a las características solicitadas
B=10
n_sd=3 ##99.9%
D=(B/n_sd)^2
n=(N*s^2)/(((N-1)*D)+s^2) 
n=round(n,0)

#### extraer muuestra de MAS con tamño calculado
set.seed(123)
sam2=S.SI(N,n)

muestra2=Lucy[sam2,]

estima=muestra2[,'Employees']

E=E.SI(N, n, estima)
TE=E[1,2] ### total empleados de todas las empresas estimado a través de la muestra
mTE=TE/N  ### promedio de empleados por empresa estimado


##### estimación de total de empleados con Muestreo Sistemático
a=round(N/n, 0)

set.seed(123)
sam_sis=S.SY(N,a)

muestra_sist=Lucy[sam,]
estima_sis=muestra_sist[,'Employees']

E_sist=E.SY(N, a, estima_sis)
TE_sist=E_sist[1,2]
m_TE_sist=TE_sist/E_sist[1,1] ## media de empleados estimado con sistematico, el N varia con en la estimación de sistemático



############ Muestreo estratificado


boxplot(Lucy$Employees~Lucy$SPAM)

### teoricamente no es una variable adecuada para estratificar porque los estratos se comportarían muy similar, y no es lo ideal.
Nno=summary(Lucy$SPAM)[1]
Nyes=summary(Lucy$SPAM)[2]

Nst=c(Nno, Nyes)

ano=Nno/N
ayes=Nyes/N

ast=c(ano,ayes)

varst=c(s^2,s^2)

n_no=round(ano*n)
n_yes=round(ayes*n)

nst=c(n_no, n_yes)

set.seed(123)
sam_st=S.STSI(Lucy$SPAM, Nst,nst)

muestra_st=Lucy[sam_st,]

estima_st=muestra_st[,"Employees"]

E_ST=E.STSI(muestra_st$SPAM, Nst, nst, estima_st)

E_ST['Estimation','Population']
TE=E_ST[1,3,2]
mTE=TE/N
