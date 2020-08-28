# Solucion punto 4

# Hidroxiproplina
hidro<-c(77.3,61.2,82.4,75.9,61,70.2,65,80)

# Numero de pacientes
num_pacientes <- c(3,10,15,13,8,5,2,0)

data<-cbind(hidro, num_pacientes)

data = data[order(data[,1]),];

hidro<-data[,1]
num_pacientes<-data[,2]


# A) Tabla de frecuencias

x<-hidro
absoluta<-num_pacientes
absoluta_acumulada <- cumsum(num_pacientes)
frecuencia_relativa<-num_pacientes/sum(num_pacientes)
relativa_acumulada <- cumsum(num_pacientes/sum(num_pacientes))
cbind(x,absoluta,absoluta_acumulada,frecuencia_relativa, relativa_acumulada)


# Diagrama de barras para las frecuencias absoluta, relativa y acumulada

barplot(absoluta,cex.names=1,names.arg=hidro,xlab="Hidroxiprolina (mg) ",ylab="Pacientes",main="Niveles de hidroxiprolina")
barplot(absoluta_acumulada,cex.names=1,names.arg=hidro,xlab="Hidroxiprolina (mg) ",ylab="Pacientes",main="Niveles de hidroxiprolina")
barplot(frecuencia_relativa,cex.names=1,names.arg=hidro,xlab="Hidroxiprolina (mg) ",ylab="Porcentaje de Pacientes",main="Niveles de hidroxiprolina")
barplot(relativa_acumulada,cex.names=1,names.arg=hidro,xlab="Hidroxiprolina (mg) ",ylab="Porcentaje Pacientes",main="Niveles de hidroxiprolina")


# B) Medidas de tendencia central

summary(num_pacientes)

# C) Medidas de desviacion

var(num_pacientes)
sd(num_pacientes)

# Creo un vector para calcular las medidas de tendencia con todos los valores
datos<-vector()

for (i in 1:length(hidro)){
  datos<-c(datos,rep(hidro[i],num_pacientes[i]))
}
# Medidas de tendencia con metros

summary(datos)

# Desviacion con metros

var(datos)
sd(datos)


# D) Grafico de barras

hist(data, xlab = "Hidroxiprolina (mg)", ylab = "Frecuencia", main = "Histograma de los datos")

# E) Grafico de dispersion

plot(num_pacientes, xlab="Hidroxiprolina (mg) ",ylab="Pacientes",main="Niveles de hidroxiprolina")

# F) Conclusiones

# La mayor cantidad de Hidroxiprolina absorbida por los pacientes fue del 82.40%,
# frente a un promedio de 72.06%. En la gr�fica podemos percibir un peque�o sesgo
# a la izquierda esto tambi�n es evidenciable ya que la mediana es mayor a la media.


