




### ruido blanco
set.seed(222)
et=rnorm(100,0,50)
plot(et, type="line")

#### ruido blanco +C
y1=et-12
plot.ts(y1)

par(mfrow=c(1,2))
plot.ts(y1)
plot.ts(et)

#### caminata aleatoria sin deriva ####

y2=cumsum(et)
plot(y2, type="line")

#### caminata aleatoria con deriva ####

y3=cumsum(y1)
plot(y3, type="line")

### parámetro menor a 1

et=rnorm(100)

n=length(et)
y4=c()
y4[1]=et[1]


for(i in 2:n){

  y4[i]<- -15+(-1.2*y4[i-1]) + rnorm(1,0,50)
 
}

plot(y4, type="line")


 
