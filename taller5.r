datos<- c(7.33,7.31,7.26,7.33,7.37,7.27,7.30,7.33,7.33,7.32,7.35,7.39,7.33,7.38,7.33,7.31,7.37,7.35,7.34,7.32,7.29,7.35,7.38,7.32,7.32,7.33,7.32,7.40,7.32,7.34,7.33,7.33)

datos_or<-datos

#defino max y minimo para el tamano del intervalo
max<-max(datos)
min<-min(datos)
# tamano intervalo
len_intervalo <- (max-min)/5

#inicio el actual como el minimo
actual<-min+len_intervalo
#inicio un vector vacio
intervalos<-vector()

for(i in 1:5){
  #creo un vector de intervalos para luego evaluar los rangos
  intervalos<-c(intervalos,actual)
  actual<-actual+len_intervalo
}

# ordeno los datos para contar mejor
datos<-sort(datos)
#vector donde se cuenta frecuencia
frecuencia<-c(rep(0,5))
#indice para saber con que intervalo comparo
indice_comp<-1
#indice dato
d<-1

while(d<=length((datos))){
  if(datos[d]<=intervalos[indice_comp]){
    frecuencia[indice_comp]<-frecuencia[indice_comp]+1
    d<-d+1
  }else{
    indice_comp<-indice_comp+1
  }
}

nombres<-vector()

for(i in intervalos){
  nombres<-c(nombres,paste(c(i-len_intervalo,i),collapse = " - "))
}

a<-intervalos
b<-frecuencia

#tabla de frecuencias
x<-a
absoluta<-b
absoluta_acumulada <- cumsum(b)
frecuencia_relativa<-b/sum(b)
relativa_acumulada <- cumsum(b/sum(b))
cbind(x,absoluta,absoluta_acumulada,frecuencia_relativa, relativa_acumulada)

datos<- datos_or

# medidas de tendencia central
summary(datos)

#medidas de desviacion
sd(datos)
var(datos)

#grafico de dispersion
plot(datos,main="Nivel de pH",xlab="Pacientes",ylab="pH")


ancho_clase=c(min(datos),intervalos)

#histograma de frecuencias
hist(datos,breaks = ancho_clase,xlim=c(min(datos),max(datos)),xaxt="n",main="Nivel de pH",xlab="pH",ylab="Pacientes")
axis(1, ancho_clase,las=1, font=1,cex.axis=1)
#ancho_clase

#conclusiones


