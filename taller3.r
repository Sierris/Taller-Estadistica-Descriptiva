a = c(1,2,3,4,5,6,7,8 )
b = c(2,6,10,5,10,3,2,2)

# Punto 3
#a
x = a
absoluta = b
absoluta_acumulada = cumsum(b)
frecuencia_relativa = b/sum(b)
relativa_acumulada = cumsum(b/sum(b))
cbind(x,absoluta,absoluta_acumulada,frecuencia_relativa, relativa_acumulada)


#b
#Frecuencia absoluta
barplot(absoluta, cex.names = 1, names.arg = x, xlab = "Metros", ylab = "Niños", main = "Niños que caminan")
#Frecuencia acumulada
barplot(absoluta_acumulada, cex.names = 1, names.arg = x, xlab = "Metros", ylab = "Niños", main = "Niños que caminan")
#Frecuencia relativa
barplot(frecuencia_relativa, cex.names = 1, names.arg = x, xlab = "Metros", ylab = "Porcentaje de niños", main = "Niños que caminan")
#Frecuencia relativa acumultada
barplot(relativa_acumulada, cex.names = 1, names.arg = x, xlab = "Metros", ylab = "Porcentaje de niños", main="Niños que caminan")


#c
summary(b)

#d
sd(b) #Desviacion estandar
var(b) #varianza

#e
plot(b,xlab = "Metros",ylab = "Niños",main = "Niños que caminan")

#f
# Se puede observar que en promedio los niños caminan 4 metros, también se puede obsertbar que el 75% de los niños caminan
# al menos 7 metros y los datos tienen una dispersión de 11.71 metros
