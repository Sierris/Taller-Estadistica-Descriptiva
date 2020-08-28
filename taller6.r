# Solucion Punto 6

datos<- c(3720,3795,3340,5600,3800,3580,5500,2000,1570,2360,1500,1840,3725,3790,3345,3805,5595,3575,1995,5505,2055,1575,1835,1505)
datos_or<-datos

# Defino max y minimo para el tamano del intervalo

max<-max(datos)
min<-min(datos)

# Tamano intervalo, utilizamos 5 en este caso

len_intervalo <- (max-min)/5

# Inicio el actual como el minimo

actual<-min+len_intervalo

# Inicio un vector vacio

intervalos<-vector()

# Creo un vector de intervalos para luego evaluar los rangos
for(i in 1:5){
  intervalos<-c(intervalos,actual)
  actual<-actual+len_intervalo
}

# Ordeno los datos para contar mejor

datos<-sort(datos)

# Vector donde se cuenta frecuencia

frecuencia<-c(rep(0,5))

# Indice para saber con que intervalo comparo

indice_comp<-1

# Indice dato

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

# A) Tabla de frecuencias

x<-a
absoluta<-b
absoluta_acumulada <- cumsum(b)
frecuencia_relativa<-b/sum(b)
relativa_acumulada <- cumsum(b/sum(b))
cbind(x,absoluta,absoluta_acumulada,frecuencia_relativa, relativa_acumulada)

datos<- datos_or

# B) Medidas de tendencia central

summary(datos)

# C) Medidas de desviacion

sd(datos)
var(datos)

# Grafico de dispersion

plot(datos,main="Niveles sericos de creatiquinasa",xlab="Niveles",ylab="Pacientes")

ancho_clase=c(min(datos),intervalos)

# D) Histograma de frecuencias

hist(datos,breaks = ancho_clase,xlim=c(min(datos),max(datos)),xaxt="n",main="Niveles sericos de creatiquinasa",xlab="Niveles",ylab="Pacientes")
axis(1, ancho_clase,las=1, font=1,cex.axis=1)

# F) Conclusiones

# El mayor n�mero de pacientes est� ubicado en el rango de 3140 a 3960, el 25% de la
# poblaci�n tiene niveles s�ricos mayores a 3796. Los datos de esta muestra tienen una
# alta volatilidad y esto se evidencia con su varianza.
