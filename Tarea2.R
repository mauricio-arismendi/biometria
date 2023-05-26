pob01 <- rnorm(240, mean = 9.9, sd = 3.26)

str(pob01)
summary(pob01)
pob01
muestra01 <- sample(pob01, size = 10, replace = FALSE)
muestra01

hist(muestra01, col="orange", xlab = "Frecuencia", ylab = "Datos", main = "Frecuencia de datos", las = 1)
muestra02 <- sample(pob01, size = 50, replace = FALSE)
muestra02
hist(muestra02, col="red", xlab = "Datos", ylab = "Frecuencia", main = "Frecuencia de datos", las = 1)


muestra03 <- sample(pob01, size = 180, replace = FALSE )
muestra03

hist(muestra03, col="purple", xlab = "Datos", ylab = "Frecuencia", main = "Frecuencia de datos", las = 1)
