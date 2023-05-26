
getwd()


list.files()
rm(list = ls())
library(ggplot2)
fert.data<-read.csv(file.choose(), header=TRUE)
str(fert.data)
fert.data$tratamiento<-as.factor(fert.data$tratamiento
                            )
# Crear variables
tratamiento <- fert.data$tratamiento
volumen <- fert.data$volumen

# Calcular las medias, desviaciones estándar y tamaños muestrales
medias <- tapply(volumen, tratamiento, mean)
medias

st_d <- tapply(volumen, tratamiento, sd)
st_d

tam_muestral <- tapply(volumen, tratamiento, length)
tam_muestral


# Calcular el error estándar
error_estandar <- st_d / sqrt(tam_muestral)
error_estandar
typeof(error_estandar)

# Crear un objeto de dataframe para el error estándar, ya que de otra forma no funciona en R, no sé por qué
#entonces, usamos cada dato creado como tratamiento, medias y error estandar para crear un nuevo set de datos y lo guardamos
# 
st.error <- data.frame(tratamiento = tratamiento, 
                       medias = medias, 
                       error_estandar = error_estandar,
                       volumen = fert.data$volumen)

st.error
# Combinar los datos en un único dataframe, porque de otra forma no funciona.
datos_completos <- merge(fert.data, st.error, by = "tratamiento")
datos_completos


# Gráfico de barras y error estándar
ggplot(data = datos_completos, aes(x = tratamiento, y = medias)) +
  geom_col(position = "dodge", color = "black", fill = "green", width = 0.8) +
  geom_errorbar(aes(ymin = medias + error_estandar, ymax = medias - error_estandar), 
                width = 0.1, color = "black") +
  xlab("Tratamiento") + ylab(bquote("Media Volumen")) +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) +
  #geom_text(label = c("a"), vjust = -1.2,  size = 4) +
  theme_bw() +
  theme(legend.text = element_text(size = 20),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))+
  geom_text(data = difs_significativas, aes(y = medias, label = dif_signif),
            position = position_nudge(y = 1.5))



prueba <- aov(volumen ~ tratamiento, datos_completos)
summary(prueba)


tukey_resultado <- TukeyHSD(prueba)
tukey_resultado

difs_significativas <- data.frame(tukey_resultado$diffs)
difs_significativas$tratamientos <- rownames(difs_significativas)

difs_significativas$dif_signif <- ifelse(difs_significativas$p.adj < 0.05, "*", "")

difs_significativas


#calcular los intervalos de confianza
tukey_result <- TukeyHSD(prueba)
tukey_result

difs_significativas <- data.frame(
  dif_signif = c("A1 - A0", "A2 - A0", "A3 - A0", "A4 - A0", "A2 - A1", "A3 - A1", "A4 - A1", "A3 - A2", "A4 - A2", "A4 - A3"),
  ymin = with(tukey_result, tratamiento$diff + lwr),
  ymax = with(tukey_result, tratamiento$diff + upr)
)

# Graficar los datos y los intervalos de confianza
ggplot(data = datos_completos, aes(x = tratamiento, y = medias)) +
  geom_col(position = "dodge", color = "black", fill = "green", width = 0.8) +
  geom_errorbar(aes(ymin = medias + error_estandar, ymax = medias - error_estandar), 
                width = 0.1, color = "black") +
  xlab("Tratamiento") + ylab(bquote("Media Volumen")) +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) +
  theme_bw() +
  theme(legend.text = element_text(size = 20),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black")) +
  # Agregar las letras de Tukey
  geom_signif(data = difs_significativas, aes(xmin = as.numeric(factor(dif_signif)) - 0.2,
                                              xmax = as.numeric(factor(dif_signif)) + 0.2,
                                              y = ymax + 1),
              textsize = "4", tip_length = 0.01)