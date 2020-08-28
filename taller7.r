datos = c(3.0, 4.0, 3.0, 3.6, 4.4, 2.6, 2.6, 3.2, 3.0, 3.8, 3.6, 3.0, 3.2, 4.0, 3.2, 3.0, 3.6, 3.2, 2.8, 2.2)
datos_or = datos

max = max(datos)
min = min(datos)
len_intervalo = (max-min)/5


actual = min+len_intervalo
intervalos = vector()

for(i in 1:5) {
  intervalos = c(intervalos,actual)
  actual = actual+len_intervalo
}

datos = sort(datos)
frecuencia = c(rep(0,5))
indice_comp = 1
d = 1

while(d<=length((datos))){
  if(datos[d]<=intervalos[indice_comp]){
    frecuencia[indice_comp] = frecuencia[indice_comp]+1
    d = d+1
  }else{
    indice_comp = indice_comp+1
  }
}

nombres = vector()
for(i in intervalos){
  nombres = c(nombres,paste(c(i-len_intervalo,i),collapse = " - "))
}

a = intervalos
b = frecuencia

# Tabla de frecuencias
x = a
absoluta = b
absoluta_acumulada = cumsum(b)
frecuencia_relativa = b/sum(b)
relativa_acumulada = cumsum(b/sum(b))
cbind(x,absoluta,absoluta_acumulada,frecuencia_relativa, relativa_acumulada)
datos = datos_or

# Medidas de tendencia central
summary(datos)
ancho_clase = c(min(datos),intervalos)

# Histograma de frecuencias
hist(datos,breaks = ancho_clase,xlim=c(min(datos),max(datos)),xaxt="n",main="Notas de los estudiantes",xlab="Notas",ylab="Estudiantes")
axis(1, ancho_clase,las=1, font=1,cex.axis=1)