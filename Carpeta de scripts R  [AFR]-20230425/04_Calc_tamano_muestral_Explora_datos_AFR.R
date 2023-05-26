#############################################################
# Introduccion a software R -- open source -- gratis!!
# Prof. Andres Fuentes, PhD.
# Biometria ACF-352
# Ing. en Recursos Naturales
# Fall 2023
# Universidad de La Frontera
#############################################################


#######################################################
# 04/04/23
# lab 04: Tamano muestral & Manejo de datos
#######################################################

rm(list = ls())

library(pwr)

############
# Ejemplo 1:
############

# Is the average body temperature of college students any different from 36.6°C?
# H0=36.6°C,  H1≠36.6°C
# We will guess that the effect size will be medium.
# For t-tests:   0.2 = small, 0.5 = medium, and 0.8 for large effect sizes.
# Selected two-tailed, because we were asking if temp differed,
# not whether it  was simply lower or higher.

# How the code is organized?

pwr.t.test(d = ?, sig.level = ?, power = ?, type = c("two.sample", "one.sample",
          "paired"), alternative=c("two-sided", "greater", "less"))

# d = effect size
# sig.level = significant level
# power = power of test
# type = type of test

# Calculate sample size using "pwr.t.test:
n.1<-pwr.t.test(d=0.50, sig.level=0.05, power=0.80, type="one.sample",
           alternative="two.sided")
n.1
str(n.1)

ceiling(n.1$n)  # what's "ceiling" for?

############
# Ejemplo 2:
############

# Queremos comparar el tiempo medio de reacción de los participantes en dos
# grupos de estudio sometidos a un tratamiento.

# Conocemos de la bibliografía que el tiempo de reacción tiene una desviación
# estándar (SD) de 1.25 segundos y que una diferencia en 1 segundo en el tiempo
# de reacción se considera una diferencia importante.

# El tamaño del efecto sería:
d<- (1/1.25) # [la diferencia de medias dividido por la SD].
d  # tamano grande

# Para un poder del 90% y un nivel de confianza del 95%, ¿cuántos participantes
# necesitamos en nuestro estudio? 
  
n.2<-pwr.t.test(d=0.8, sig.level=0.05, power=0.8, type="two.sample",
           alternative="two.sided")
n.2
ceiling(n.2$n)

############
# Ejemplo 3:
############

# ¿Es la temperatura corporal media más alta en hembras que en los machos de la
# especie Castor canadiensis?
  
#   H0 = 0°C		H1 > 0°C
# Supondremos que los tamaños del efecto serán medianos para las pruebas t:
# Recordemos que 0.2=pequeño, 0.5=mediano y 0.8 gran tamaño del efecto.

# Seleccionamos mayor que (greater), porque sólo nos interesaba comprobar si
# la temperatura corporal media de las hembras era mayor, no menor (el grupo 1
# es hembras; el grupo 2 machos)

n.3<-pwr.t.test(d=0.5, sig.level=0.05, power=0.80, type="two.sample",
                alternative="greater")
n.3
ceiling(n.3$n)

############
# Ejemplo 4:
############

# ¿Es mayor la frecuencia cardíaca después de correr en comparación con antes de correr?
#  H0: fc (después) - fc (antes) ≤ 0
#  H1: fc (después) - fc (antes) > 0

# Supondremos que los tamaños del efecto serán grandes para las pruebas t:
# Recordemos que 0.2=pequeño, 0.5=mediano y 0.8 gran tamaño del efecto.

# Seleccionamos mayor que (greater), porque sólo nos interesaba comprobar si la
# fc era mayor después de correr (el grupo 1 es antes, el grupo 2 después de correr).

n.4<-pwr.t.test(d=0.8, sig.level=0.05, power=0.80, type="paired",
                alternative="greater")
n.4
ceiling(n.4$n)

##########################
# Ejemplo 5: test de ANOVA
##########################

# Queremos comparar 5 grupos, con 25 individuos por grupo, para un
# nivel de significancia alfa del 5%.

# Calculamos el poder de la prueba para distintos valores de tamaño
# de efecto (f):
  
pwr.anova.test(k=5, n=25, f=0.10, sig.level=0.05)  #power = 0.11809
pwr.anova.test(k=5, n=25, f=0.25, sig.level=0.05)  #power = 0.5738
pwr.anova.test(k=5, n=25, f=0.40, sig.level=0.05)  #power = 0.95691

# Que tamaño muestral por grupo se requiere para obtener un poder
# estadístico de la prueba = 0.80, asumiendo que el tamaño del
# efecto es alto y alfa = 0.05?

# For ANOVA:   0.1 = small, 0.25 = medium, and 0.4 for large effect sizes.

n.5<-pwr.anova.test(k=5, power=0.80, f=0.40, sig.level=0.05)
n.5
ceiling(n.5$n)

########################################
# Ahora a computar sus propios calculos:
########################################

# Estamos interesados en determinar si la frecuencia cardiaca es
# mayor en las personas después de una visita al médico en
# comparación con antes de una visita. Asuma un alfa = 0.05 y
# poder = 0.80

# Hipotesis:  H0:        H1:

# Veamos los datos:

hr_antes <-c(126, 88, 53.1, 98.5, 88.3, 82.5, 105, 41.9)
hr_despues<-c(138.6, 110.1, 58.44, 110.2, 89.61, 98.6, 115.3, 64.3)

# Calcular tamaño del efecto:
# mean hr_despues - mean hr_antes / SD pooled

mean(hr_antes)
sd(hr_antes)

mean(hr_despues)
sd(hr_despues)

sd_pooled<-sqrt((26.8^2 + 27.2^2)/2)
d_hr<-(98.1 - 85.4) / sd_pooled

# t-test a una cola

pwr.t.test(d=0.47, sig.level=0.05, power=0.80, type="paired",
           alternative="greater")

# Cuantos individuos hay que considerar?


# Muy bien -- volvamos ahora algunos datos y explorar su est. descriptiva

library(alr4)

# check OS for defining working directory
Sys.info()

if(Sys.info()[1]=="Linux")
{setwd("/home/andres/Dropbox/A-UFRO/Clases Ecologia UFRO/Fall 2023/ACF352-Biometria/Datasets")} else
{setwd("C:/Users/anfue/Dropbox/A-UFRO/Clases Ecologia UFRO/Fall 2023/ACF352-Biometria/Datasets")}

# check correct path to WD
getwd()


mi.ufc<-read.csv("ufcData.csv", sep=",", header=TRUE)

mi.ufc<-read.csv("https://www.dropbox.com/s/ikx6gw1ksmvpmm3/ufcData.csv?dl=1")

names(mi.ufc) <- c("parcela","arbol","spp","dap","altura")

summary(mi.ufc)  # summary de todo el set de datos
mi.ufc$spp<-as.factor(mi.ufc$spp)
mi.ufc$d <- mi.ufc$dap/10
head(mi.ufc)
mi.ufc$h <- mi.ufc$altura/10
head(mi.ufc)
options(max.print=99999)
mi.ufc

# cambiemos el nombre de spp para saber cuales son
# para esto usaremos un "for in" loop:

# primero veamos los nombres originales en los datos;
tapply(mi.ufc$d, mi.ufc$spp, length)

# creamos una columna sin datos (NA) que llamaremos nom.especies
mi.ufc$nom.especie<-(NA)
head(mi.ufc)

# ejecutamos el loop
for(i in 1:nrow(mi.ufc)){
  if(mi.ufc$spp[i]=="DF") {
    mi.ufc$nom.especie[i]="Douglas_Fir"}
  if(mi.ufc$spp[i]=="GF") {
    mi.ufc$nom.especie[i]="Grand_Fir"}
  if(mi.ufc$spp[i]=="SF") {
    mi.ufc$nom.especie[i]="Subalpine_Fir"}
  if(mi.ufc$spp[i]=="WL") {
    mi.ufc$nom.especie[i]="West_Larch"}
  if(mi.ufc$spp[i]=="WC") {
    mi.ufc$nom.especie[i]="Western_Red_Cedar"}
  if(mi.ufc$spp[i]=="WP") {
    mi.ufc$nom.especie[i]="White_Pine"}
}

# revisemos los nuevos nombres
mi.ufc

tapply(mi.ufc$d, mi.ufc$nom.especie, length)


# Media de la altura de Douglas Fir?

# Mediana del DAP de Grand Fir?

# SD de la altura de Western Red Cedar?
