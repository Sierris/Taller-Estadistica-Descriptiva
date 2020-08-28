#a<- c(77.3,61.2,82.4,75.9,61,70.2,65,80)
a<-c(77.3,61.2,82.4,75.9,61,70.2,65,80)
b <- c(3,10,15,13,8,5,2,0)

data<-cbind(a,b)

data = data[order(data[,1]),];

a<-data[,1]
b<-data[,2]


#tabla de frecuencias
x<-a
absoluta<-b
absoluta_acumulada <- cumsum(b)
frecuencia_relativa<-b/sum(b)
relativa_acumulada <- cumsum(b/sum(b))
cbind(x,absoluta,absoluta_acumulada,frecuencia_relativa, relativa_acumulada)


#diagrama de barras para frecuencuas absoluta, relativa y acumulada

barplot(absoluta,cex.names=1,names.arg=a,xlab="Hidroxiprolina (mg) ",ylab="Pacientes",main="Niveles de hidroxiprolina")
barplot(absoluta_acumulada,cex.names=1,names.arg=a,xlab="Hidroxiprolina (mg) ",ylab="Pacientes",main="Niveles de hidroxiprolina")
barplot(frecuencia_relativa,cex.names=1,names.arg=a,xlab="Hidroxiprolina (mg) ",ylab="Porcentaje de Pacientes",main="Niveles de hidroxiprolina")
barplot(relativa_acumulada,cex.names=1,names.arg=a,xlab="Hidroxiprolina (mg) ",ylab="Porcentaje Pacientes",main="Niveles de hidroxiprolina")



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
  
datos
# medidas de tendencia con metros
summary(datos)
#desviacion con metros
sd(datos)
var(datos)

  
#grafico de dispersion

plot(b,xlab="Hidroxiprolina (mg) ",ylab="Pacientes",main="Niveles de hidroxiprolina")


#conclusiones


