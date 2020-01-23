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

library(rafalib) # usaremos esta librería pa trabajar con sd y varianza
                 # poblacionales

# importar datos de ratas, ante experimento de dieta normal vs high fat
dat <- read.csv("mice_pheno.csv")

# capturar hembras en dieta de control
control.pop <- dat %>% filter(Sex == "F", Diet == "chow") %>%
  dplyr::select(Bodyweight) %>% unlist() # se sobrepone select() con librería MASS

# capturar hembras bajo hf
tratamiento.pop <- dat %>% filter(Sex == "F", Diet == "hf") %>%
  dplyr::select(Bodyweight) %>% unlist()

mean(tratamiento.pop) - mean(control.pop)

# teorema central del límite (CLT)
pnorm(2, mean = 0, sd = 1, lower.tail = F)

# solo conociendo media y sd poblacional, en una dist normal, podemos
# conocer que tan extremo es un valor (estimamos a partir de muestras)
# Y bajo CLT operamos por ley de grandes Numeros

# creamos data solo de ratas hembras
dat.f <- dat %>% filter(Sex == "F")

# ploteamos diferencias en pesos
ggplot(dat.f,aes(Bodyweight)) +
  geom_histogram(binwidth = 3.0) +
  geom_density(aes(y= 2.5 * ..count.., colour = "red"), 
               size = 1) +
  theme(legend.position = "none") +
  facet_grid(.~ Diet) +
  labs(x = "Pesos de ratas hembras", y = "frecuencia")

# plot de curva de cuantiles (qq plot)
ggplot(dat.f,aes(sample = Bodyweight)) +
  geom_qq() +
  geom_qq_line() +
  theme(legend.position = "none") +
  facet_grid(.~ Diet) +
  labs(x = "Cuantiles teóricos", y = "Cuantiles observados")


# Simulamos distribución para múltiples muestreos bajo supuesto de 
# población conocida, y por lo tanto su desviación
Ns <- c(3, 12, 25, 50)
res <- sapply(Ns, function(n){
  replicate(10000, {
    mean(sample(tratamiento.pop, n))-mean(sample(control.pop, n))
  })
})

# reconvertir a data frame
res <- res %>% as.data.frame()
n.label <- c("N_3", "N_12", "N_25", "N_50")
lista <- list()
for (i in seq(along=n.label)) {
  temp.res <- res[,i] %>% as.data.frame()
  temp.res$N <- n.label[i]
  lista[[i]] <- temp.res
}

# convertir lista a df y reorganizar factores
df.res <- do.call("rbind", lista)
df.res$N <- df.res$N %>% as.factor()
df.res$N <- factor(df.res$N, levels(df.res$N)[c(3,1,2,4)])

# borar objetos que no ulitzaremos
rm(i, n.label, res, lista, temp.res)

# qq plot para múltiples N
ggplot(df.res, aes(sample =.)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(.~N, nrow = 2) +
  labs(x = "Cuantiles teóricos", y = "Cuantiles observados")

# Pero bajo supuesto de población desconocida,con desviación 
# estándar estimada <- t de student

t.student.diff.n <- function(n){
  y <- sample(tratamiento.pop,n)
  x <- sample(control.pop,n)
  t.test(y, x)$statistic
}

# simulación de Monte Carlo de t de student, para distintos N
res <- sapply(Ns, function(n){
  replicate(10000, t.student.diff.n(n))
})

# reconvertir a data frame
res <- res %>% as.data.frame()
n.label <- c("N_3", "N_12", "N_25", "N_50")
lista <- list()
for (i in seq(along=n.label)) {
  temp.res <- res[,i] %>% as.data.frame()
  temp.res$N <- n.label[i]
  lista[[i]] <- temp.res
}

# convertir lista a df y reorganizar factores
df.res <- do.call("rbind", lista)
df.res$N <- df.res$N %>% as.factor()
df.res$N <- factor(df.res$N, levels(df.res$N)[c(3,1,2,4)])

# eliminar objetos a no ultilizar
rm(t.student.diff.n)
rm(i, n.label, res, lista, temp.res)

# qq plot estimada por t de student, para distintos N
ggplot(df.res, aes(sample =.)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(.~N, nrow = 2) +
  labs(x = "Cuantiles teóricos", y = "Cuantiles observados t-student")


# == PRUEBA T DE STUDENT
# (dataset específico de ratas hembras)
dat <- read.csv("femaleMiceWeights.csv")

# capturar hembras en dieta de control
control.pop <- dat %>% filter(Diet == "chow") %>%
  dplyr::select(Bodyweight) %>% unlist() 

# capturar hembras bajo hf
tratamiento.pop <- dat %>% filter(Diet == "hf") %>%
  dplyr::select(Bodyweight) %>% unlist()

# analizando función t de student, t.test()
resultado <-t.test(dat$Bodyweight[dat$Diet=="hf"], 
                   dat$Bodyweight[dat$Diet=="chow"])

# imprimir resultado
resultado

# como se ve ésta diferencia bajo distribución normal??
pnorm(resultado$statistic, lower.tail = F) * 2 # por 2 porque distribución es
                                               # simétrica

# diferencias?? Si, supuestos de CLT y t-student son distintos. 
# CLT -> dice que hay efecto cuando no lo hay (error de tipo I)
# t student -> dice que NO hay efecto cuando SI lo hay (error de tipo II)

# == INTERVALOS DE CONFIANZA EN T DE STUDENT

# imprimir CI
resultado$conf.int

# calculando CI a mano
# recorda que operamos bajo intérvalo de 
# \bar{X}-Q*\frac{s_x}{\sqrt{N}}\leq\mu_x\leq\bar{X}+Q*\frac{s_x}{\sqrt{N}}  

# importamos población de ratas
dat <- read.csv("mice_pheno.csv")

# extramos población de ratas femeninas en dieta normal
pop.control <- dat %>% 
  filter(Sex == "F", Diet == "chow") %>% 
  dplyr::select(Bodyweight)

# muestra de ratas
muestra <- sample(pop.control$Bodyweight, 30)

# calculamos parámetros
media <- mean(muestra) # \bar{X}
Q <- qnorm(1 - 0.05/2) # Q
se <- sd(muestra)/sqrt(30) # \frac{s_x}{\sqrt{N}}
intervalo <- c(media - Q*se,media, media + Q*se) # \mu_x\pm\bar{X}+Q*\frac{s_x}{\sqrt{N}} 
intervalo[1] <= mean(pop.control$Bodyweight) & 
  intervalo[3] >= mean(pop.control$Bodyweight) # intérvalo contiene a la media poblacional??

# función para obtener intérvalo
get.intervalo <- function(i, vector){
  media.pop <- mean(vector) 
  muestra <- sample(vector, 30) # puedes probar cambiando el tamaño muestral
  media.mue <- mean(muestra) 
  Q <- qnorm(1 - 0.05/2)
  se <- sd(muestra)/sqrt(30) 
  contiene <- (media.mue - Q*se) < media.pop & (media.mue + Q*se) > media.pop
  intervalo <- c(i, media.mue - Q*se, media.mue, media.pop, media.mue + Q*se, contiene)
  return(intervalo)
}

# simulación para múltiples intérvalos, y construcción del dataset simulado
# se corren 100 simulaciones, podemos probar con más, y ver que ocurre!
# también puedes probar con distintos tamaños muestrales, actualmente está en 30
lista <- list()
for(i in 1:100){
  lista[[i]] <- get.intervalo(i, pop.control$Bodyweight)
}
simulacion <- do.call("rbind", lista)
colnames(simulacion) <- c("iteracion", "ic_menor", "media.m", "media.p", "ic_superior", "contiene")
simulacion <- as.data.frame(simulacion)
rm(lista, i)

# visualización de los intérvalos
ggplot(simulacion, aes(iteracion, media.m, colour = as.factor(contiene))) +
  theme_bw() +
  geom_pointrange(aes(ymin= ic_menor, ymax = ic_superior)) +
  scale_color_manual(values = c("red", "black")) +
  theme(legend.position = "none", text = element_text(size = 20)) +
  geom_hline(yintercept = simulacion$media.p, size = 2, colour = "blue") +
  coord_flip() +
  geom_text(label = paste0(simulacion$contiene %>% sum(), "/100"), 
            y = 26.5, x = 100, size = 10) +
  labs(y = "Media Muestral")

















