
#############################################################
# Introduccion basica a software R -- open source -- gratis!!
# Prof. Andres Fuentes, PhD.
# Biometria ACF-352
# Fall 2023
# Universidad de La Frontera
#############################################################


########################################################
# 11/04/23
# lab 05: Estadistica descriptiva, graficos, analisis I
########################################################


# R accede o "lee" en directorios de trabajo
# Donde esta leyendo R?

getwd()

# Donde quiero que trabaje R ahora?
# Hay que especificar la ruta a la carpeta que quiero (directorio de trabajo)

# check OS
Sys.info()

if(Sys.info()[1]=="Linux")
{setwd("/home/andres/Dropbox/A-UFRO/Clases Ecologia UFRO/Fall 2023/ACF352-Biometria/Datasets")} else
{setwd("C:/Users/anfue/Dropbox/A-UFRO/Clases Ecologia UFRO/Fall 2023/ACF352-Biometria/Datasets")}

# check correct path to WD
getwd()


# Que archivos hay en la carpeta (ruta) que especifique a R?
list.files()

# leer datos
data.arau<-read.csv("supervivencia_araucaria_24.csv", sep=",",
                    header=TRUE)

data.arau<-read.csv(file.choose(), header=TRUE)

str(data.arau)
data.arau$months<-as.factor(data.arau$months) # make months a categorical factor
data.arau$severity<-as.factor(data.arau$severity)
data.arau$bio_leg<-as.factor(data.arau$bio_leg)
data.arau$exclusion<-as.factor(data.arau$exclusion)

# check data structure
str(data.arau)
head(data.arau)
tail(data.arau)
dim(data.arau)

# remove empty cels (no data = mortality)
data.arau<-na.omit(data.arau); nrow(data.arau)

summary(data.arau$height)

# subsetting data by fire severity HS
arau.hs<-subset(data.arau, severity=="hs")
str(arau.hs)
summary(arau.hs$height)
quantile(arau.hs$height, c(0.2, 0.6, 0.9))

# subsetting data by fire severity MS
arau.ms<-subset(data.arau, severity=="ms")
str(arau.ms)
summary(arau.ms$height)

# quick plots for freq and distribution
# HS
hist(arau.hs$height)

# MS
hist(arau.ms$height)

# number of new buds
# HS
hist(arau.hs$num_buds)

# MS
hist(arau.ms$num_buds)

# quick boxplot
boxplot(height ~ months, data = arau.hs, col="orange", border="brown",
        pch=16)
# complete de plot with proper axis-names, units, las, etc.

boxplot(height ~ months, data = arau.ms, col="lightblue",
        border="navy", pch=16, las=1, ylab="Seedling height (cm)",
        xlab="Months after planting",
        main="Height in moderate fire severity")

# guardemos plot en formato JPEG
jpeg("Altura_araucaria_MS.jpg", width = 1200, height = 800)
boxplot(height ~ months, data = arau.ms, col="lightblue",
        border="navy", pch=16, las=1, ylab="Seedling height (cm)",
        xlab="Months after planting",  cex=1.2, cex.axis=1.2,
        cex.lab=1.4, main="Height in moderate fire severity")
dev.off()

# Pregunta -- como puedo saber si hay diferencias significativas en la 
# media de la altura entre los distintos meses?

# asumiendo que los datos de altura siguen una distribucion normal
# (histograma) podemos contruir un modelo lineal (lm) para
# responder la pregunta
# usemos un  alfa = 0.05

# modelo 1 en HS
mod.1<-aov(height ~ months, data=arau.hs)  # aov = analysis of variance
mod.1
summary(mod.1)

# sabemos que hay un efecto significativo de los meses
# post-plantacion en altura
# donde esta esa dif? ANOVA sola no nos dice donde!
# necesitamos un test a-posteriori -- cual?  Tukey

TukeyHSD(mod.1) # comparamos entre cada par de meses que se incluyen en el modelo
                # ademas, corrije por comparaciones multiples -- excelente!

# ahora comprueba tu que pasa en media severidad de fuego y la altura de plantas

mod.2<-aov(height ~ months, data=arau.ms)
mod.2
summary(mod.2)
TukeyHSD(mod.2)

# eso es todo estimados alumnos - prox clase continuamos
# disfRuten!
# saludos

