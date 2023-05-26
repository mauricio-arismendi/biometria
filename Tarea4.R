#Tarea4



getwd()


list.files()
rm(list = ls())
library(ggplot2)


setwd("/home/mauricio/Documents/Biometria")
list.files()
data.course<-read.csv("altura_curso.csv")
summary(data.course)

data.course

generoNA<-subset(data.course, genero != "")
generoNA

data.course<-as.factor(data.course$genero)

summary(data.course)

generoNA$genero<-as.factor(generoNA$genero) 
generoNA

str(generoNA)
dim(generoNA)
