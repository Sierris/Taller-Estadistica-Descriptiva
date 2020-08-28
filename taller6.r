datos<- c(3720,3795,3340,5600,3800,3580,5500,2000,1570,2360,1500,1840,3725,3790,3345,3805,5595,3575,1995,5505,2055,1575,1835,1505)
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
plot(datos,main="Niveles séricos de creatiquinasa",xlab="Niveles",ylab="Pacientes")

ancho_clase=c(min(datos),intervalos)



#histograma de frecuencias
hist(datos,breaks = ancho_clase,xlim=c(min(datos),max(datos)),xaxt="n",main="Niveles séricos de creatiquinasa",xlab="Niveles",ylab="Pacientes")
axis(1, ancho_clase,las=1, font=1,cex.axis=1)
#conclusiones





