# CONFIGURACIONES

library(tidyverse)
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

# replicamos 10.000 muestreos aleatorios. Esto nos dará la distribución bajo Ho
null.dist <- replicate(10000,{
  control <- sample(poblacion$Bodyweight, 12)
  tratamiento <- sample(poblacion$Bodyweight, 12)
  mean(tratamiento) - mean(control) 
})


# Y llegamos al gran tema ¿probabilidad de obtener un resultado más extremo
# asumiendo que Ho = TRUE ??
mean(null.dist >= obsdiff) # <- p.valor


































