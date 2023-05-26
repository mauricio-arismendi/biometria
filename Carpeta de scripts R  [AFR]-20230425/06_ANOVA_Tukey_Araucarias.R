
#######################################
# Prof. Andres Fuentes, PhD.
# Biometria ACF-352
# Fall 2023 
# Universidad de La Frontera
# Martes 18/04/23
# Script: ANOVA, comp multiples Tukey HSD
#######################################

# check OS
Sys.info()

if(Sys.info()[1]=="Linux")
{setwd("/home/andres/Dropbox/A-UFRO/Clases Ecologia UFRO/Fall 2023/ACF352-Biometria/Datasets")} else
{setwd("C:/Users/anfue/Dropbox/A-UFRO/Clases Ecologia UFRO/Fall 2023/ACF352-Biometria/Datasets")}

# check correct path to WD
getwd()
list.files()
rm(list = ls())

library(plotrix)
library(ggplot2)
library(plyr)

###################################################
# read the data from .csv file
# Araucaria mean number of buds sprouts
# two types/sources of resprouting: canopy & roots
# RN China Muerta, 2016, 17 & 18
###################################################

buds.data<-read.csv(file.choose(), header=TRUE)

buds.data<-read.csv("araucaria_buds_2018.csv", sep=",", header=TRUE)	
str(buds.data)
buds.data$year<-as.factor(buds.data$year)
buds.data$type_bud<-as.factor(buds.data$type_bud)
str(buds.data)
head(buds.data)
tail(buds.data)
dim(buds.data)

buds<-subset(buds.data, bud_count != "NA") # removing cells with no data (NA)
str(buds)

# calculemos media y se para cada tipo de rebrote en cada anio de estudio
mean.buds<-ddply(buds, c("year", "type_bud"), function(df)
  return(c(mean.count=mean(df$bud_count), buds.se=std.error(df$bud_count))))
mean.buds

#Construir barras de error
buds.error<-aes(ymax=mean.count + buds.se, ymin=mean.count - buds.se)
buds.error

#grafico fino usando ggplot2
ggplot(data=mean.buds, aes(x=year, y=mean.count, group=type_bud)) +
  geom_point(aes(shape=type_bud, fill=type_bud), size=2) +
  scale_fill_manual(values=c("black","black", "black")) +
  scale_shape_manual(values=c(16,17)) +
  geom_line(linetype=2, size=0.25) +
  geom_errorbar(buds.error, width=0.1) +
  xlab("Anio") + ylab("Media num. de brotes") +
  scale_y_continuous(limits=c(0,40), breaks=seq(0,40, by=10)) +
  theme_bw() +
  theme(legend.text=element_text(size=12)) +
  theme(axis.title.x=element_text(colour="black", size=12)) +
  theme(axis.title.y=element_text(colour="black", size=12)) +
  theme(axis.text.x=element_text(colour="black", size=10)) +
  theme(axis.text.y=element_text(colour="black", size=10)) +
  theme(plot.background=element_blank(),
  panel.grid.major=element_blank(),
  panel.grid.minor=element_blank(),
  panel.border=element_blank(),
  panel.background=element_blank() +
  theme(axis.line=element_line(color="black")))

# Modelo de regresión con variable predictora categorica
m1<-lm(bud_count ~ type_bud, data=buds)

# Cual es la hipótesis?

summary(m1) # revisar la estructura del modelo

anova(m1) # tabla de ANOVA

#############################################
# Ejemplo 2: Produccion de semillas Araucaria
#############################################

seed<-read.csv("seed_data_arauc.csv", sep=",", header=TRUE)	
seed$year<-as.factor(seed$year)
seed$sampling<-as.factor(seed$sampling)
str(seed)
head(seed)
tail(seed)
dim(seed)

# Direction (N,S,E,O) y su efecto en las semillas
mean.dir<-ddply(seed, c("transect"), function(df)
  return(c(m.seed.dir=mean(df$seed_num), seed.dir.se=std.error(df$seed_num))))
mean.dir
dir.error<-aes(ymax=m.seed.dir + seed.dir.se, ymin=m.seed.dir - seed.dir.se)


ggplot(data=mean.dir, aes(x=transect, y=m.seed.dir)) +
  geom_bar(aes(fill=id_indiv), stat="identity", position="dodge", color="black",
           fill="black", width=0.5) +
  geom_errorbar(dir.error, width=0.2) +
  xlab("Transect direction") + ylab(bquote("Number of seeds/m"^"2")) +
  scale_y_continuous(limits=c(0,20), breaks=seq(0,20, by=5)) +
  geom_text(label=c("a"), vjust = -2, size= 4) +
  theme_bw() +
  theme(legend.text=element_text(size=12)) +
  theme(axis.title.x=element_text(colour="black", size=12)) +
  theme(axis.title.y=element_text(colour="black", size=12)) +
  theme(axis.text.x=element_text(colour="black", size=10)) +
  theme(axis.text.y=element_text(colour="black", size=10)) +
  theme(plot.background=element_blank(),
  panel.grid.major=element_blank(),
  panel.grid.minor=element_blank(),
  panel.border=element_blank(),
  panel.background=element_blank() +
  theme(axis.line=element_line(color="black")))

m2.dir<-aov(seed_num ~ transect, data = seed)
summary(m2.dir)
TukeyHSD(m2.dir)

# Efecto de la distancia desde el fuste/tronco

for(i in 1:nrow(seed)){
  if(seed$distance[i]==0) {
    seed$dist_class[i]=1}
  if(seed$distance[i]==2) {
    seed$dist_class[i]=1}
  if(seed$distance[i]==4) {
    seed$dist_class[i]=2}
  if(seed$distance[i]==6) {
    seed$dist_class[i]=2}
  if(seed$distance[i]==8) {
    seed$dist_class[i]=3}
  if(seed$distance[i]==10) {
    seed$dist_class[i]=3}
  if(seed$distance[i]==12) {
    seed$dist_class[i]=4}
  if(seed$distance[i]==14) {
    seed$dist_class[i]=4}
}

seed$dist_class<-as.factor(seed$dist_class)
head(seed)

# Distance class
mean.dist<-ddply(seed, c("dist_class"), function(df)
  return(c(m.seed.dist=mean(df$seed_num), seed.dist.se=std.error(df$seed_num))))
mean.dist
mean.dist$dist_class<-as.factor(mean.dist$dist_class)
mean.dist$m.seed.ha<-mean.dist$m.seed.dist * 10000

dist.error<-aes(ymax=m.seed.dist + seed.dist.se, ymin=m.seed.dist - seed.dist.se)

ggplot(data=mean.dist, aes(x=dist_class, y=m.seed.dist, group=1)) +
  geom_bar(aes(fill=m.seed.dist), stat="identity", position="dodge", color="black", fill="black", width=0.5) +
  geom_errorbar(dist.error, width=0.2) +
  xlab("Distance class") + ylab(bquote("Number of seeds/m"^"2")) +
  scale_y_continuous(limits=c(0,20), breaks=seq(0,20, by=5)) +
  geom_text(label=c("a", "b", "b", "b"), vjust = -2, size= 4) +
  theme_bw() +
  theme(legend.text=element_text(size=12)) +
  theme(axis.title.x=element_text(colour="black", size=12)) +
  theme(axis.title.y=element_text(colour="black", size=12)) +
  theme(axis.text.x=element_text(colour="black", size=10)) +
  theme(axis.text.y=element_text(colour="black", size=10)) +
  theme(plot.background=element_blank(),
  panel.grid.major=element_blank(),
  panel.grid.minor=element_blank(),
  panel.border=element_blank(),
  panel.background=element_blank() +
  theme(axis.line=element_line(color="black")))

m3.dist<-aov(seed_num ~ dist_class, data = seed)
summary(m3.dist)
anova(m3.dist)

# Comparemos diferencias entre grupos (clase de distancia
# desde el fuste)
TukeyHSD(m3.dist)


# eso es todo estimados alumnos - prox clase continuamos
# disfRuten!
# saludos

