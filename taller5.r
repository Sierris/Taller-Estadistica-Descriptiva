datos =  c(7.33,7.31,7.26,7.33,7.37,7.27,7.30,7.33,7.33,7.32,7.35,7.39,7.33,7.38,7.33,7.31,7.37,7.35,7.34,7.32,7.29,7.35,7.38,7.32,7.32,7.33,7.32,7.40,7.32,7.34,7.33,7.33)
datos_or = datos

#Punto 5
max = max(datos)
min = min(datos)
len_intervalo = (max-min)/5


actual = min+len_intervalo
intervalos = vector()

for(i in 1:5){
  intervalos = c(intervalos,actual)
  actual = actual+len_intervalo
}


datos = sort(datos)
frecuencia = c(rep(0,5))

indice_comp = 1
d = 1
while(d<=length((datos))){
  if(datos[d] <= intervalos[indice_comp])
    {
    frecuencia[indice_comp] = frecuencia[indice_comp]+1
    d = d+1
  }
  else
  {
    indice_comp = indice_comp+1
  }
}

nombres = vector()

for(i in intervalos){
  nombres = c(nombres,paste(c(i-len_intervalo,i),collapse = " - "))
}

a = intervalos
b = frecuencia

#a
x = a
absoluta = b
absoluta_acumulada  =  cumsum(b)
frecuencia_relativa = b/sum(b)
relativa_acumulada = cumsum(b/sum(b))
cbind(x,absoluta,absoluta_acumulada,frecuencia_relativa, relativa_acumulada)

datos =  datos_or

#b
summary(datos)

#c
sd(datos)
var(datos)

#d
ancho_clase=c(min(datos),intervalos)
hist(datos,breaks = ancho_clase,xlim=c(min(datos),max(datos)),xaxt="n",main="pH",xlab="pH",ylab="Pacientes")
axis(1, ancho_clase,las=1, font=1,cex.axis=1)

#e
# Se puede concluir que al menos el 75% del PH se encuentra por debajo de 7.350, también se puede concluir 
# que los datos poseen una dispersión de 0.0009 aproximadamente


