a<- c(1,2,3,4,5,6,7,8 )

b <- c(2,6,10,5,10,3,2,2)


#tabla de frecuencias
x<-a
absoluta<-b
absoluta_acumulada <- cumsum(b)
frecuencia_relativa<-b/sum(b)
relativa_acumulada <- cumsum(b/sum(b))
cbind(x,absoluta,absoluta_acumulada,frecuencia_relativa, relativa_acumulada)


#diagrama de barras para frecuencuas absoluta, relativa y acumulada

barplot(absoluta,cex.names=1,names.arg=x,xlab="Metros",ylab="Niños",main="Los niños que caminan =)")

barplot(absoluta_acumulada,cex.names=1,names.arg=x,xlab="Metros",ylab="Niños",main="Los niños que caminan =)")

barplot(frecuencia_relativa,cex.names=1,names.arg=x,xlab="Metros",ylab="Porcentaje niños",main="Los niños que caminan =)")

barplot(relativa_acumulada,cex.names=1,names.arg=x,xlab="Metros",ylab="Porcentaje niños",main="Los niños que caminan =)")



# medidas de tendencia central
summary(b)

#medidas de desviacion
sd(b)
var(b)

#creo un vector para calcular las medidas de tendencia con todos los valores
datos<-vector()

for (i in 1:length(a)){
  datos<-c(datos,rep(a[i],b[i]))
}
# medidas de tendencia con metros
summary(datos)
#desviacion con metros
sd(datos)
var(datos)

  
#grafico de dispersion

plot(b,xlab="Metros",ylab="Niños",main="Los niños que caminan =)")


#conclusiones


