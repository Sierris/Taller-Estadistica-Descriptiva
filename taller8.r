# Solucion punto 8

datos<-c(0.43, 1.50, 2.05, 2.58, 2.86, 2.93, 3.90, 4.38, 4.46, 5.11, 5.23, 5.36, 5.44, 5.54, 6.08, 6.12, 6.36, 6.39, 6.48, 6.51, 6.88, 7.31, 7.34, 8.06, 8.10, 8.20, 8.21, 8.23, 8.34, 8.56, 8.64, 8.73, 8.73, 8.73, 8.73, 8.74, 8.88, 8.90, 8.93, 9.14, 9.25, 9.56, 9.68, 9.85, 9.87, 9.94, 9.99, 10.06, 10.11, 10.24, 10.26, 10.51, 11.23, 11.60, 11.63, 11.64, 11.85, 11.92, 12.34, 12.78, 12.94, 13.05, 13.18, 13.31, 13.48, 13.88, 14.05, 14.15, 14.23, 14.24, 14.30, 14.55, 14.59, 15.42, 15.45, 15.71, 16.07, 16.54, 16.84, 17.04, 19.39)
datos_or<-datos

# Defino max y minimo para el tamano del intervalo

max<-max(datos)
min<-min(datos)

# Tamano intervalo

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

# Tabla de frecuencias

x<-a
absoluta<-b
absoluta_acumulada <- cumsum(b)
frecuencia_relativa<-b/sum(b)
relativa_acumulada <- cumsum(b/sum(b))
cbind(x,absoluta,absoluta_acumulada,frecuencia_relativa, relativa_acumulada)

datos<-datos_or

# Medidas de tendencia central

summary(datos)

# Medidas de desviacion

sd(datos)
var(datos)

ancho_clase=c(min(datos),intervalos)

# Histograma de frecuencias

hist(datos,breaks = c(min(datos),intervalos),xlim=c(min(datos),max(datos)),xaxt="n",main="Retrasos aereos",xlab="Minutos",ylab="Numero de retrasos")

data.cut = cut(datos, ancho_clase, right=FALSE)
data.freq = table(data.cut)
cbind(data.freq);
par(new=TRUE)
plot(data.freq, type="b",ann='FALSE')

#length(datos)

# Conclusiones

# La gráfica aparenta que se trata de una distribución normal pero la diferencia entre la
# media y la mediana hace pensar que existe un sesgo a la derecha de la distribución ya
# que la media es mayor a la mediana. La mayor cantidad de retrasos están contenidos
# en el intervalo 8.01-11.8.
