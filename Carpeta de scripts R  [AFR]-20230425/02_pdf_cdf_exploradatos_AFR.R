
#############################################################
# Introduccion basica a software R -- open source -- gratis!!
# Prof. Andres Fuentes, PhD.
# Biometria ACF-352
# Fall 2023
# 21.03.23
# Universidad de La Frontera
#############################################################

# La importancia de la tecla de "gato" "pound" "hashtag"... para hacer comentarios
# al margen - notas que nos ayudan a recordar para que sirven los comandos
# R ignora (no lee) "#" y se lo salta

# Antes de entrar a los analisis propiamente tales,
# veamos algunas cosas basicas sobre R

# R es un programa gratuito, cross-platform (multiples OS - Mac, Win, Linux)
# Es extremadamente versatil - analisis - graficos - manejo base de datos, etc
# R evoluciona y se perfecciona gracias al trabajo en red de la comunidad open source
# R es un lenguaje universal para el analisis y visualizacion de datos
# R es un lenguaje de programacion para estadisticos.
# Un lenguaje de programacion es un modo de comunicarnos con el computador
# para que hagan calculos, operaciones, graficos, etc.

# limpiar memoria local de R
rm(list = ls())

############################################################################
# tarea chica: calcular var y sd
############################################################################

pop.tarea<-as.data.frame(c(76,84,69,92,58,89,73,97,85,77))
colnames(pop.tarea)<- "va"
pop.tarea # revisamos los datos
mean.tarea<-mean(pop.tarea$va) # calculamos la media
n<-nrow(pop.tarea) # tamanio muestreal
n

for(i in 1:nrow(pop.tarea)){
  pop.tarea$dif[i] <- pop.tarea$va[i] - mean.tarea
  pop.tarea$dif.2[i] <- (pop.tarea$dif[i] ^2)
  pop.tarea$dif.2.n[i] <- (pop.tarea$dif.2[i] / (n-1))
}

# como quedan los datos?
pop.tarea

var.tarea<-sum(pop.tarea$dif.2.n)
var.tarea

sd.tarea<-sqrt(var.tarea)
sd.tarea

# comprobamos usando funciones "var" y "sd" 
var(pop.tarea$va)
sd(pop.tarea$va)

###################
# lab 02: PDF & CDF
###################

# Creamos un set de datos con parametros especificos
# de tamano (n), media (mean), y desviacion estandard (sd) usando distribucion normal:

data<-rnorm(n=120, mean=4.9, sd=1.26)  # creamos datos con distribucion normal
                                      # puedes modificar los parametros y ver como afecta eso el

mean(data)
median(data)
data  # los revisamos                 # histograma y bosxplot de 'data'

head(data)  # vemos las 6 primeras observaciones

tail(data)  # vemos las 6 ultimas observaciones

summary(data)  # exploramos la variabilidad de los datos

hist(data, col="orange")  # graficamos el hitograma de frecuencia de los datos

# ahora graficamos un boxplot - grafico de cajas
# xlab=nombre eje X; ylab= nombre eje Y

boxplot(data, col="pink", xlab="Data", ylab="Variable respuesta")


# en R la pdf de la función Normal se obtiene mediante
# la función dnorm(), mientras que la cdf con la función pnorm()


# secuencia numerica de -10 a 20
y<-seq(-10, 20, by=0.1)
y

# calcular la PDF Normal en base a la media y sd de los datos
pdf.y<- dnorm(y, mean=4, sd=6)
pdf.z<- dnorm(y, mean=1, sd=3)
pdf.x<- dnorm(y, mean=-5, sd=1)


# Plot la PDF (Normal Distribution Function)
plot(y, pdf.y, type="l", ylab="Probabilidad", xlab="Valores de Y,Z,X",
     xlim=c(-10, 20), col="red", lwd=2, ylim=c(0, 0.5))
lines(y, pdf.z, col="darkgreen", lwd=2)
lines(y, pdf.x, col="blue", lwd=2)

legend("topright", legend=c("Y m=4, sd=6", "Z m=1, sd=3", "X m=-5,
                            sd=1"), lty=1, lwd=2,
                            col=c("red", "darkgreen", "blue"))


# calculemos probabilidades
pnorm(1, mean=0, sd=1) # prob que Y sea < 1 dado la media y sd

1-pnorm(1, mean=0, sd=1) # prob que Y sea > 1 dado la media y sd

pnorm(1, mean=0, sd=1) - pnorm(0, mean=0, sd=1) # prob que 0< Y < 1 dado la media y sd


# calcular la Cumulative Distribution Function (CDF)
# en base a media y sd de los datos

# create sample data
sample_data<- rnorm(500, mean=6, sd=0.7)

# calculate CDF 
data.cdf<- ecdf(sample_data)

# draw the cdf plot
plot(data.cdf, col="blue",cex=0.5, main="Grafico CDF", xlab="Data values",
     ylab="Prob. Acumulada", las=1)

###################################################
###################################################

# Con R podemos crear o simular datos, pero tambien podemos leer datos
# desde planillas excel, csv, dat...

# R accede o "lee" en directorios de trabajo - Donde esta leyendo R?

getwd()

# Donde quiero que trabaje R ahora?

# Hay que especificar la ruta a la carpeta que quiero (directorio de trabajo)

setwd("/Users/andres_mac/Dropbox/A-UFRO/Clases Ecologia UFRO/Fall 2023/ACF352-Biometria/Datasets") # Mac AF
setwd("/home/andres/Dropbox/A-UFRO/Clases Ecologia UFRO/Fall 2023/ACF352-Biometria/Datasets") # Linux machines
setwd("C:/Users/anfue/Dropbox/A-UFRO/Clases Ecologia UFRO/Fall 2023/ACF352-Biometria/Datasets") # windows

# Lo confirmamos:
getwd()

# Que archivos hay en la carpeta (ruta) que especifique a R?
list.files()
options(max.print=99999)


# leemos los datos desde carpeta del curso
mi.ufc<-read.csv("ufcData.csv", sep=",", header=TRUE)

# cargamos los datos directamente desde librería alr4
library(alr4)
head(ufc)
mi.ufc<-(ufc)
#####################################################

str(mi.ufc)  # revisamos la estructura de los datos
head(mi.ufc)
tail(mi.ufc)
dim(mi.ufc)  # dimensiones del data frame


# otra forma: seleccionamos el archivo que queremos leer:
mi.ufc<-read.csv(file.choose())
str(mi.ufc)

