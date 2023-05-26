#############################################################
# Introduccion basica a software R -- open source -- gratis!!
# Prof. Andres Fuentes, PhD.
# Biometria ACF-352
# Fall 2023
# Universidad de La Frontera
#############################################################


#######################################################
# 28/03/23
# lab 03: Manejo de datos & estadistica descriptiva I
#######################################################


# Con R podemos crear o simular datos, pero tambien podemos leer datos
# desde planillas excel, csv, dat...

# R accede o "lee" en directorios de trabajo - Donde esta leyendo R?

getwd()

# Donde quiero que trabaje R ahora?

# Hay que especificar la ruta a la carpeta que quiero (directorio de trabajo - wd)

setwd("/Users/andres_mac/Dropbox/A-UFRO/Clases Ecologia UFRO/Fall 2023/ACF352-Biometria/Datasets") # Mac AF
setwd("/home/andres/Dropbox/A-UFRO/Clases Ecologia UFRO/Fall 2023/ACF352-Biometria/Datasets") # Linux machines
setwd("C:/Users/anfue/Dropbox/A-UFRO/Clases Ecologia UFRO/Fall 2023/ACF352-Biometria/Datasets") # windows

# Lo confirmamos:
getwd()

# Que archivos hay en la carpeta (ruta) que especifique a R?
list.files()
options(max.print=99999)

# ok, leamos algunos datos
# libreria alr4

library(alr4)

mi.ufc<-read.csv("ufcData.csv", sep=",", header=TRUE)	

str(mi.ufc)  # revisamos la estructura de los datos

head(mi.ufc)

tail(mi.ufc)

dim(mi.ufc)  # dimensiones del data frame


# otra forma: seleccionamos el archivo que queremos leer:
mi.ufc<-read.csv(file.choose())
str(mi.ufc)

mi.ufc<-read.csv("https://www.dropbox.com/s/ikx6gw1ksmvpmm3/ufcData.csv?dl=1")

# define nuevos nombres de columnas
colnames(mi.ufc)

names(mi.ufc) <- c("parcela","arbol","spp","dap","altura")

summary(mi.ufc)  # summary de todo el set de datos
mi.ufc$spp<-as.factor(mi.ufc$spp)

# creemos nuevas variables
mi.ufc$d <- mi.ufc$dap/10
head(mi.ufc)

mi.ufc$h <- mi.ufc$altura/10
head(mi.ufc)

summary(mi.ufc$h) # summary de una sola variable
summary(mi.ufc[,c("h", "d")])

# seleccionemos porciones de los datos - solo algunas filas o columnas - o tambien combinando ambas
mi.ufc
mi.ufc[ 100:103, ] # observaciones para todas las variables entre 100 y 103
mi.ufc[ 365:367, 6] # observaciones solo de la col 6
mi.ufc[ 365:367, "d"] # observaciones solo de la col "d"

mi.ufc[ 200:210, c("spp","d","h")]
mi.ufc[ 200:210, c(3,6,7)]

# trasnsformemos variables usando log natural
mi.ufc$ln.d <- log(mi.ufc$d)
head(mi.ufc)

mi.ufc$ln.h <- log(mi.ufc$h)
head(mi.ufc)
str(mi.ufc)
dim(mi.ufc) # dimension de los datos o data frame

# donde esta leyendo/trabajando R?
getwd()

# guardemos nuestros datos como un archivo .csv --> pregunta, donde se guardar?? el archivo??
write.csv(mi.ufc, file="mi_data_ufc.csv", row.names=FALSE)

# donde esta leyendo/trabajando R?
getwd()

# seleccionemos todas las filas de un data frame, pero solo con algunas columnas especificas       
mi.otra.ufc <- mi.ufc[ , c("arbol","spp","d","h")]
head(mi.otra.ufc)

class(mi.otra.ufc$d)  # que tipo de variables son d y h
class(mi.otra.ufc$h)
class(mi.otra.ufc$arbol)

#variable factor       
class(mi.ufc$spp)  # que tipo de variable es spp
levels(mi.ufc$spp) # cuales son las especies?

# cuantos niveles de un factor       
length(levels(mi.ufc$spp))  # cuantas especies distintas hay?

# dimensiones y num de observaciones en el data frame 
dim(mi.otra.ufc)
nrow(mi.otra.ufc)
length(mi.otra.ufc$d)

#seleccionar la dataframe segun caracteristicas de variables (columnas) --> uso de la funcion "subset"      
subset(mi.ufc,  d > 95)
nrow(subset(mi.ufc,  d > 95))

nrow(subset(mi.ufc,  spp == "DF"))

subset(mi.ufc,  spp == "DF" & d >90)
subset(mi.ufc,  spp == "DF" & d >80)
subset(mi.ufc,  spp == "DF" & d >70)

nrow(subset(mi.ufc,  spp == "DF"))  # cuantas observaciones (filas) de la especie DF hay?
subset(mi.ufc,  spp == "DF" & d >90)  # cuantas DF hay con dap > 90 cm?

# funcion 'table' nos ayuda a explorar mejor los datos
table(mi.ufc$spp)

summary(mi.ufc$d)
summary(mi.ufc$h)
summary(mi.ufc)

# calculemos cuartiles y otras proporciones

quantile(mi.ufc$h, 0.25)  # primer cuartil
quantile(mi.ufc$h, c(0.25, 0.6, 0.9))  # percentiles 25% 60% y 90%
quantile(mi.ufc$h, 0.5)  # mediana 

tapply(mi.ufc$h, mi.ufc$spp, summary)  # summary de la altura por cada especie

tapply(mi.ufc$d, mi.ufc$spp, summary)  # summary del dap por cada especie

tapply(mi.ufc$d, mi.ufc$spp, length)  # num de individuos por cada especie


# graficos de distribucion para dap y altura   
boxplot(mi.ufc$d)
hist(mi.ufc$d)

hist(mi.ufc$h)
boxplot(mi.ufc$h)

# tambien podemos guardar nuestros graficos para su uso posterior  --> donde se guardar?? el archivo??

# 1. llama la funcion
jpeg("miboxplot.jpg", width = 600, height = 600)

# 2. Create the plot
boxplot(mi.ufc$h, xlab = "Especie xx", ylab = "Altura (m)",
        col = "darkgreen", main="Grafico de Altura")

# 3. Close the file
dev.off()


pdf("mihistograma.pdf")
hist(mi.ufc$d, xlab = "Clases DAP", ylab = "Frecuencia", col = "darkorange", main="Histograma")
dev.off()

# eso es todo estimados alumnos
# disfRuten!
# saludos


