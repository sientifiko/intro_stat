# CONFIGURACIONES

library(tidyverse); library(UsingR)
options(scipen = 999)

# ============= UNIDAD 1 COMPARACIÓN DE MEDIAS T-TEST ==============

# importar la data de la muestra
dat <- read.csv("femaleMiceWeights.csv")

# crear vector de control y tratamiento
control <- dat$Bodyweight[dat$Diet=="chow"] %>% mean()
tratamiento <- dat$Bodyweight[dat$Diet=="hf"] %>% mean()

# calcular diferencias 
obsdiff <- mean(tratamiento) - mean(control)

# obtener diferencias medias
mean(obsdiff) # <- variable aleatoria

# importar la data de "la población"
poblacion <- read.csv("femaleControlsPopulation.csv")

# extraemos aleatoriamente 12 sujetos, e imprimimos medias para ver diferencias
for (i in 1:3) {
  temp.samp <- sample(poblacion$Bodyweight, 12)
  print(mean(temp.samp))
  rm(temp.samp)
}

# == LA HIPOTESIS NULA (Ho)

control <- sample(poblacion$Bodyweight, 12)
tratamiento <- sample(poblacion$Bodyweight, 12)

tratamiento - control # imprimimos media bajo Ho

# Simulación Monte Carlo 10.000 muestreos aleatorios. 
# Esto nos dará la distribución bajo Ho
null.dist <- replicate(10000,{
  control <- sample(poblacion$Bodyweight, 12)
  tratamiento <- sample(poblacion$Bodyweight, 12)
  mean(tratamiento) - mean(control) 
})


# Y llegamos al gran tema ¿probabilidad de obtener un resultado más extremo
# asumiendo que Ho = TRUE ??
mean(null.dist >= obsdiff) # <- p.valor


# == LAS DISTRIBUCIONES

data("father.son")

father.son <- father.son %>% na.omit()

# qué es una distribución??
father.son$sheight %>% sample(10) %>% round(1)

# = VISUALIZANDO DISTRIBUCIONES

# Función de distribución acumulada, F(a) ~ P(X <= a)
ggplot(father.son, aes(sheight)) +
  stat_ecdf() +
  labs(x = "Altura del hijo", y = "Frecuencia acumulada")

# Histograma (bins determinado por máquina)
ggplot(father.son, aes(sheight)) +
  geom_histogram() +
  labs(x = "Altura del hijo", y = "Frecuencia")

# == DISTRIBUCIÓN DE PROBABILIDADES

# Simulación de Monte Carlo y ploteo de distribución nula
null.dist <- replicate(100, {
  control <- sample(poblacion$Bodyweight,12)
  tratamiento <- sample(poblacion$Bodyweight,12)
  mean(tratamiento) - mean(control)
})

# convertir vector a df
null.dist <- as.data.frame(null.dist)

# plotear histograma de distribución nula
ggplot(null.dist, aes(null.dist)) +
  geom_histogram() +
  labs(x="Peso", y = "Frecuencia")


# == DISTRIBUCIÓN NORMAL
# rnorm(), solo requiere N, media y ds
# pnorm(), chance de obtener valor más extremo dado meadia y ds

pnorm(obsdiff, mean(null.dist$null.dist), 
               sd(null.dist$null.dist), 
               lower.tail = F)


# == POBLACIONES, MUESTRAS Y ESTIMACIONES

# importar datos de ratas, ante experimento de dieta normal vs high fat
dat <- read.csv("mice_pheno.csv")

# capturar hembras en dieta de control
control.pop <- dat %>% filter(Sex == "F", Diet == "chow") %>%
  dplyr::select(Bodyweight) %>% unlist() # se sobrepone select() con librería MASS

# capturar hembras bajo hf
tratamiento.pop <- dat %>% filter(Sex == "F", Diet == "hf") %>%
  dplyr::select(Bodyweight) %>% unlist()

# teorema central del límite (CLT)
# latext de clt, p. 48

pnorm(2, lower.tail = F)

























