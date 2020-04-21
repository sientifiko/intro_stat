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

# === LA HIPOTESIS NULA (Ho) ====

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


# == LAS DISTRIBUCIONES ====

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

# == DISTRIBUCIÓN DE PROBABILIDADES ====

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


# == POBLACIONES, MUESTRAS Y ESTIMACIONES ====

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


# == PRUEBA T DE STUDENT ====
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

# == INTERVALOS DE CONFIANZA EN T DE STUDENT ====

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

# capturar media poblacional
media.pop <- mean(pop.control$Bodyweight) 

# muestra de ratas
muestra <- sample(pop.control$Bodyweight, 30)

# calculamos parámetros
media <- mean(muestra) # \bar{X}
Q <- qnorm(1 - 0.05/2) # Q
se <- sd(muestra)/sqrt(30) # \frac{s_x}{\sqrt{N}}
intervalo <- c(media - Q*se,media, media + Q*se) # \mu_x\pm\bar{X}+Q*\frac{s_x}{\sqrt{N}} 
intervalo[1] <= media.pop & 
  intervalo[3] >= media.pop # intérvalo contiene a la media poblacional??

# función para obtener intérvalo
get.intervalo <- function(i, vector, n.muestra, distribucion){
  #'@param i número de iteración
  #'@param vector vector con valores de la población
  #'@param n.muestra valor entero con tamaño de la muestra
  #'@param distribucion cadena con el tipo de distribución a modelar ("qn"== dist normal, "qt"== dist t-student)
  #'@return vector con intérvalos de confianza, media muestra e iteración
  
  muestra <- sample(vector, n.muestra) # puedes probar cambiando el tamaño muestral
  media.mue <- mean(muestra) 
  Q <- case_when(
    distribucion == "qn" ~ qnorm(1 - 0.05/2),
    distribucion == "qt" ~ qt(1 - 0.05/2, df = 4)
  )
  se <- sd(muestra)/sqrt(n.muestra) 
  contiene <- (media.mue - Q*se) < mean(vector) & (media.mue + Q*se) > mean(vector)
  intervalo <- c(i, media.mue - Q*se, media.mue, media.mue + Q*se, contiene)
  return(intervalo)
}

# generar función que devuelva dataset
get.simulacion <- function(vector, n.simulaciones, n.muestra, distribucion){
  #'@param vector el vector con datos de población
  #'@param n.simulaciones número entero que indica cantidad de simulaciones a correr
  #'@param n.muestra número entero que indica tamaño de la muestra
  #'@param distribucion cadena con el tipo de distribución 
  #'@return data.frame con datos de la simulación
  
  lista <- list()
  for(i in 1:n.simulaciones){
    lista[[i]] <- get.intervalo(i, vector, n.muestra, distribucion)
  }
  simulacion <- do.call("rbind", lista)
  colnames(simulacion) <- c("iteracion", "ic_menor", "media.m","ic_superior", "contiene")
  simulacion <- as.data.frame(simulacion)
  return(simulacion)
  
}# fin get.simulacion

# se corren 100 simulaciones, podemos probar con más, y ver que ocurre!
# también puedes probar con distintos tamaños muestrales, actualmente está en 30
simulacion <- get.simulacion(pop.control$Bodyweight, 100, 30, "qn")


# visualización de los intérvalos
ggplot(simulacion, aes(iteracion, media.m, colour = as.factor(contiene))) +
  theme_bw() +
  geom_pointrange(aes(ymin= ic_menor, ymax = ic_superior)) +
  scale_color_manual(values = c("red", "black")) +
  theme(legend.position = "none", text = element_text(size = 20)) +
  geom_hline(yintercept = media.pop, size = 2, colour = "blue") +
  coord_flip() +
  geom_text(label = paste0(simulacion$contiene %>% sum(), "/100"), 
            y = 26.5, x = 100, size = 10) +
  labs(y = "Media Muestral")

# visualicemos que ocurre con una muestra mucho más pequeña
simulacion <- get.simulacion(pop.control$Bodyweight, 100, 5, "qn")

# visualización de los intérvalos
ggplot(simulacion, aes(iteracion, media.m, colour = as.factor(contiene))) +
  theme_bw() +
  geom_pointrange(aes(ymin= ic_menor, ymax = ic_superior)) +
  scale_color_manual(values = c("red", "black")) +
  theme(legend.position = "none", text = element_text(size = 20)) +
  geom_hline(yintercept = media.pop, size = 2, colour = "blue") +
  coord_flip() +
  geom_text(label = paste0(simulacion$contiene %>% sum(), "/100"), 
            y = 26.5, x = 100, size = 10) +
  labs(y = "Media Muestral")

# si achicamos los tamaños muestrales, vemos que el CLT falla, ya que ésto introduce mayor
# variabilidad en los datos, e infla las colas de la distribución (fat tails). 
# El CLT, está contenido en éste elemento
Q <- qnorm(1 - 0.05/2) # el cual asume que distribución es normal

# Pero podemos hace ésta aproximación, bajo condiciones de colas gordas, por medio de
# una distribución t, para lo cual cambiamos éste término, que incorpora grados de libertad
Q <- qt(1 - 0.05/2, df = 4)

# cambiaremos el término final de la función, para simular esa distribución
simulacion <- get.simulacion(pop.control$Bodyweight, 100, 5, "qt")

# visualización de los intérvalos
ggplot(simulacion, aes(iteracion, media.m, colour = as.factor(contiene))) +
  theme_bw() +
  geom_pointrange(aes(ymin= ic_menor, ymax = ic_superior)) +
  scale_color_manual(values = c("red", "black")) +
  theme(legend.position = "none", text = element_text(size = 20)) +
  geom_hline(yintercept = media.pop, size = 2, colour = "blue") +
  coord_flip() +
  geom_text(label = paste0(simulacion$contiene %>% sum(), "/100"), 
            y = 26.5, x = 100, size = 10) +
  labs(y = "Media Muestral")

# con ello recuperamos el intérvalo de confianza del caso anterior.

# == VÍNCULO P.VALOR E INTÉRVALO DE CONFIANZA ====

# por ejemplo en una prueba t, 
t.test(dat$Bodyweight[dat$Sex=="F" & dat$Diet=="hf"],
       dat$Bodyweight[dat$Sex=="F" & dat$Diet=="chow"])$conf.int 
# dado que estamos comparando diferencias medias,se espera que tal diferencia
# nunca sea 0, ya que ello implicaría que variable aleatoria (diferencia media muestral)
# es un valor extremo respecto de la distribución bajo supuesto de Ho (hacer memoria)
# y OJO, en este caso, que esperamos que valor > 0 en el intérvalo nos da resultado significativo,
# en otros puede esperar que valor estimado, sea por ejemplo, !=1, en cuyo
# caso se espera que intérvalo no contenga 1 para ser significativo (pudiendo ser menor o mayor a 1)


# ====================== POWER CALCULATIONS ===========================

# importamos población de ratas
dat <- read.csv("mice_pheno.csv")

# calcularemos una especie de "tamaño de efecto" del tratamiento de dieta
# alta en grasas en el peso de las ratas

# sacamos las problaciones de control y tratamiento en ratas hembras
ctrlPop <- dat %>% filter(Sex == "F" & Diet == "chow") %>%
  dplyr::select(Bodyweight) %>% unlist()

tratPop <- dat %>% filter(Sex == "F" & Diet == "hf") %>%
  dplyr::select(Bodyweight) %>% unlist()

# calculamos el porcentaje de aumento en peso dado el tipo de dieta
(mean(tratPop) - mean(ctrlPop))/mean(ctrlPop)

# nos da un ~de 10% 

# veamos si encontramos diferencias medias significativas en muestras pequeñas de N = 5
t.test(sample(ctrlPop, 5), sample(tratPop, 5) )$p.value
 
# ?? No es significativa la diferencia, pese a que lo es a nivel poblacional, es falsa
# entonces diferencia? Cometimos un error? No... bueno si, un error de Tipo II
# lo que nos faltó acá es más "poder"

# acá entran éstos errores, dado que estamos en un mundo de variables aleatorias
# es posible que justo en nuestra muestra no podamos encontrar tal diferencia

# Los errores son 2:
# Error de tipo I -> rechazar la Ho cuando es verdadera, aka falso positivo
# Error de tipo II -> no rechazar la Ho cuando ésta es falsa, aka falso negativo

# Este proceso de aplicación estadística, está mediado por éstos 2 factores
# Si minimizamos la probabilidad de un error, la del otro error aumenta, para remediar
# medianamente eso, ocupamos el cálculo de poder que és "la probabilidad de rechazar
# la hipótesis nula cuando ésta es falsa", la cual es, para variar, otra convención

# Este poder está directamente relacionado al tamaño muestral (en realidad tiene que ver 
# también con el alpha de <.05 definido arbitrariamente, pero dado que es convención, 
# nos remitiremos solo al tamaño muestral)

# Crearemos una función que nos devolverá los p.valores

p.valor <- function(n, tratamiento, control){
  return(
    t.test(sample(tratamiento, n), sample(control, n))$p.value
  )
}

p.valor(12, tratPop, ctrlPop) < .05

# repliquemos la función múltiples veces para ver la curva p
p.valores <- replicate(2000,p.valor(12, tratPop, ctrlPop)) %>% as.data.frame()

# grafiquemos
ggplot(p.valores, aes(.)) +
  theme_bw() +
  geom_histogram() +
  # geom_density() +
  geom_vline(xintercept = .05, color = "red", size = 1)

# notar que aunque grueso de resultados quedan por debajo del p.valor <.05, sigue existiendo
# una gran proporción que rechaza esa relación, incluso podemos calcular esa proporción

mean(p.valores < .05)

# Ésto nos da un poder de poco más del 20%, bastante bajo
# Veamos como se ven distintasa curvas p, con distintos tamños muestrales
lista <- list()
j <- 1
for (i in seq(10, 40, 10)) {
  p.valores <- replicate(2000,p.valor(i, tratPop, ctrlPop)) 
  lista[[j]] <- cbind(p.valores, paste0("N",i)) %>% as.data.frame()
  j <- j + 1
}
muestras <- do.call("rbind", lista)
muestras$p.valores <- as.numeric(paste0(muestras$p.valores))
rm(i,j,lista)

# grafiquemos
ggplot(muestras, aes(p.valores)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  geom_density() +
  # geom_histogram() +
  facet_wrap(.~V2, ncol = 2) +
  geom_vline(xintercept = .05, color = "red", size = 1)


# Y veamos el cuadro general de poder estadístico para diversos tamaños muestrales
poder <- sapply(seq(10, 100, 10), function(N){
  p.valores <- replicate(2000, p.valor(N, tratPop, ctrlPop))
  mean(p.valores < .05)
})

# Creemos una tabla de poder
tabla.poder <- poder %>% as.data.frame() 
tabla.poder$N <- seq(10, 100, 10)

# Grafiquemos
ggplot(tabla.poder, aes(N, poder, group =1)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  # geom_bar(stat = "identity")
  geom_line() +
  geom_point()


# así también podemos visualizar el trade off que se da entre el poder estadístico
# y el p.valor asignado. 

# manipulemos la convención de la significancia de <.05, por otros niveles y veamos
# si existe capacidad de rechazar Ho cuando es falsa

# definimos varios alfa o bandas de significancia
alphas <- c(.1, .05, .01, .001, .0001)

# hacemos la simulación, replicamos 2000 muestreos de 30 sujetos cada uno, sacamos
# diferencias medias, y promediamos para obtener el poder
power <- sapply(alphas, function(alpha){
  rechazos <- replicate(2000, p.valor(30, tratPop, ctrlPop) < alpha)
  mean(rechazos)
})

# generamos el df con datos de alfa y sus respectivos poderes
df.alphatradeoff <- data.frame(alpha = alphas, poder = power)

# ploteamos
ggplot(df.alphatradeoff, aes(alpha, poder)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  geom_line() +
  geom_point()


# cuando la H1 es verdadera, el p.valor es arbitrario, podemos hacerlo mas y mas
# pequeño solo aumentando el N

# sabemos que en nuestra población ese es el caso (existe diferencia media), por lo que
# podemos ver que pasa con la relación p.valor N

# generamos una secuencia de tamaños de muestra con 10 repeticiones cada una
Ns <- seq(10, 200, by = 10)
Ns_rep <- rep(Ns, each=10)

# recuerden que la función p.valor incluye random sampling
# calculamos el p.valor de múltiples muestreos de distinto tamaño
ps <- sapply(Ns_rep, p.valor, tratamiento = tratPop, control=ctrlPop)

# generamos df de tamaños y p.valores
df.p_vs_n <- data.frame(ns = Ns_rep, p.valores = ps)


# ploteamos relación (aplicamos escala logaritmica en p.valores para achicar distancias y
# relación se vea más clara)
ggplot(df.p_vs_n, aes(ns, log(p.valores))) +
  geom_jitter() +
  # marcamos una línea al alpha = .01
  geom_hline(yintercept = log(.01)) +
  # marcamos otra línea con el alfa convencional de .05
  geom_hline(yintercept = log(.05))

# si resultado es consistente, p.valores más pequeños no tienen más sentido!!!

# y aquí aparaece otro concepto genial, muy subvalorado: el tamaño de efecto!
# esto es, que tanto impacta la variable independiente en el output que tenemos
# en este caso dieta hf -> peso

# hay varias medidas para calcularla, pero la más conocida es la d de Cohen, que
# básucamente es la diferencia absoluta de medias dividida en la desviación estándar


(abs(mean(tratPop) - mean(ctrlPop)))/
  sd(dat$Bodyweight[dat$Sex=="F"]) # --> este es el dataset de la pobla completa















# ====================== SIMULACIONES DE MONTECARLO <3 ====

