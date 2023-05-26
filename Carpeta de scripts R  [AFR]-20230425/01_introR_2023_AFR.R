#############################################################
# Introduccion basica a software R -- open source -- gratis!!
# Prof. Andres Fuentes, PhD.
# Biometria ACF-352
# Fall 2023
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


##########################################
# Funciones basicas de calculos numericos
##########################################

# puede ser usado como calculadora simple
8+3 # suma simple

sqrt(23) # raiz cuadrada; 
(23^0.5)

exp(1) # num Euler
exp(6) # e^6

pi # constante pi
3 * pi # multplicacion
pi / 3 # division


# vectores
rep(1, 10) # 10 veces numero 1

a<-rep(1,10) # creamos el objeto "a" con el vector
a

b<-1:10 # otro objeto "b" con otro vector
b

d<-c(a, b) # otro objeto con los dos vectores juntos
d

a + b # sumamos el vector "a" + "b"

a + 2 * b # multiplicacion y suma de vectores

a / b


# el signo "=" significa que estamos creando un objeto, vector, etc -- tb podemos usar "<-"
result = 3 ^ 4 * (2/0.5) * sqrt(pi)
result
result <- 3 ^ 4 * (2/0.5) * sqrt(pi)
result

# crea vectores numericos correlativos
# esto es un conjunto de numeros ordenados correlativamente de 1 a 30
1:30

# secuencia de numeros
length(seq(0,200, by=50))

# Crea objetos a partir de funciones matematicas, bases de datos, etc
ej1 = 1:18
ej1

ej2 = seq(0,40, by=5)
ej2
class(ej2)

ej3 = 3^4*(2/0.5)
ej3
class(ej3)

leyenda = ("Este es un ejemplo literal")
leyenda
class(leyenda)


# revise los objetos guardados en la memoria de R

ls() # esto es (todos los objetos que hemos creado hasta el momento)

# limpiar memoria de R antes de iniciar trabajo o al finalizar  -- cuidado, se borra todo de la memoria!!
rm(list = ls())

# borrar solo algunos objetos seleccionados
rm(ej1, ej3)


############################################################################
############################################################################

# instalar librerias [packages] en R


# aca voy a instalar la libreria alr4
install.packages('alr4')

# aca voy a instalar el paquete datana
install.packages('datana', dependencies = TRUE)

# cargamos paquete datana a esta sesion
require(datana)

# cargamos libreria alr3 a esta sesion
library(alr4)


# instalemos otras 3 librerias muy utiles
install.packages("plotrix", dependencies = TRUE)
library(plotrix)

install.packages("plyr", dependencies = TRUE)
library(plyr)

install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# cuales son las librerias ya instaladas?
installed.packages()


# cada vez que usemos R para analsis en un informe/reporte/manuscrito lo debemos citar.
# pero, como saber la cita correcta de la version utilizada?

citation() # cita del software R

citation("datana") # cita de la libreria 'datana'

citation("alr4") # cita de la libreria 'alr4'

############################################################################
############################################################################


# Creamos un set de datos con parametros especificos
# de tamano (n), media (mean), y desviacion estandard (sd) usando distribucion normal:

data<-rnorm(n=200, mean=9.9, sd=3.26)  # creamos datos con distribucion normal
# puedes modificar los parametros y ver como afecta eso el

class(data)
mean(data)
median(data)

data  # los revisamos                 # histograma y boxplot de 'data'

head(data)  # vemos las 6 primeras observaciones

tail(data)  # vemos las 6 ultimas observaciones

summary(data)  # exploramos la variabilidad de los datos

hist(data, col="orange")  # graficamos el hitograma de frecuencia de los datos

# ahora graficamos un boxplot - grafico de cajas
# xlab=nombre eje X; ylab= nombre eje Y
boxplot(data, col="pink", xlab="Data", ylab="Variable respuesta",
        las=1, ylim=c(0,20))


hist(sample(data, 10), las=1)
summary(sample(data, 10))

hist(sample(data, 50))
summary(sample(data, 50))

hist(sample(data, 150))
summary(sample(data, 150))


###################################################
# una secuencia
###################################################
seq(0, 3) #serie de numeros, 0-1
length(seq(5, 15, by=0.1)) #serie de numeros, 10-11, cada 0.1


#########################################################################
# la funcion sample --> selecciona al azar "n" elementos de una poblacion
#########################################################################

sample(seq(1:1000), 10) # selecciona 3 de 1000

sample(1:6, 5, replace=TRUE)# tiremos los dados 5 veces

sample(c("Cara","Sello"), 5, replace=TRUE) # moneda lanzada 5 veces

# cuales son los objetos que he creado hasta el momento en r?
ls()


# eso es todo por hoy
# no olvides guardar tu scrip de R y repasar lo aprendido!


