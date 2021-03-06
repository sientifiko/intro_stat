# CONFIGURACIONES

library(tidyverse); library(UsingR)
options(scipen = 999)

# =========== PRE UNIDAD: EXPLORATORY DATA ANALYSIS =============






# ============= UNIDAD 1 COMPARACI�N DE MEDIAS T-TEST ==============

# importar la data de la muestra
dat <- read.csv("femaleMiceWeights.csv")

# crear vector de control y tratamiento
control <- dat$Bodyweight[dat$Diet=="chow"] %>% mean()
tratamiento <- dat$Bodyweight[dat$Diet=="hf"] %>% mean()

# calcular diferencias 
obsdiff <- mean(tratamiento) - mean(control)

# obtener diferencias medias
mean(obsdiff) # <- variable aleatoria

# importar la data de "la poblaci�n"
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

# Simulaci�n Monte Carlo 10.000 muestreos aleatorios. 
# Esto nos dar� la distribuci�n bajo Ho
null.dist <- replicate(10000,{
  control <- sample(poblacion$Bodyweight, 12)
  tratamiento <- sample(poblacion$Bodyweight, 12)
  mean(tratamiento) - mean(control) 
})


# Y llegamos al gran tema �probabilidad de obtener un resultado m�s extremo
# asumiendo que Ho = TRUE ??
mean(null.dist >= obsdiff) # <- p.valor


# == LAS DISTRIBUCIONES ====

data("father.son")

father.son <- father.son %>% na.omit()

# qu� es una distribuci�n??
father.son$sheight %>% sample(10) %>% round(1)

# = VISUALIZANDO DISTRIBUCIONES

# Funci�n de distribuci�n acumulada, F(a) ~ P(X <= a)
ggplot(father.son, aes(sheight)) +
  stat_ecdf() +
  labs(x = "Altura del hijo", y = "Frecuencia acumulada")

# Histograma (bins determinado por m�quina)
ggplot(father.son, aes(sheight)) +
  geom_histogram() +
  labs(x = "Altura del hijo", y = "Frecuencia")

# == DISTRIBUCI�N DE PROBABILIDADES ====

# Simulaci�n de Monte Carlo y ploteo de distribuci�n nula
null.dist <- replicate(100, {
  control <- sample(poblacion$Bodyweight,12)
  tratamiento <- sample(poblacion$Bodyweight,12)
  mean(tratamiento) - mean(control)
})

# convertir vector a df
null.dist <- as.data.frame(null.dist)

# plotear histograma de distribuci�n nula
ggplot(null.dist, aes(null.dist)) +
  geom_histogram() +
  labs(x="Peso", y = "Frecuencia")


# == DISTRIBUCI�N NORMAL
# rnorm(), solo requiere N, media y ds
# pnorm(), chance de obtener valor m�s extremo dado meadia y ds

pnorm(obsdiff, mean(null.dist$null.dist), 
               sd(null.dist$null.dist), 
               lower.tail = F)


# == POBLACIONES, MUESTRAS Y ESTIMACIONES ====

library(rafalib) # usaremos esta librer�a pa trabajar con sd y varianza
                 # poblacionales

# importar datos de ratas, ante experimento de dieta normal vs high fat
dat <- read.csv("mice_pheno.csv")

# capturar hembras en dieta de control
control.pop <- dat %>% filter(Sex == "F", Diet == "chow") %>%
  dplyr::select(Bodyweight) %>% unlist() # se sobrepone select() con librer�a MASS

# capturar hembras bajo hf
tratamiento.pop <- dat %>% filter(Sex == "F", Diet == "hf") %>%
  dplyr::select(Bodyweight) %>% unlist()

mean(tratamiento.pop) - mean(control.pop)

# teorema central del l�mite (CLT)
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
  labs(x = "Cuantiles te�ricos", y = "Cuantiles observados")


# Simulamos distribuci�n para m�ltiples muestreos bajo supuesto de 
# poblaci�n conocida, y por lo tanto su desviaci�n
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

# qq plot para m�ltiples N
ggplot(df.res, aes(sample =.)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(.~N, nrow = 2) +
  labs(x = "Cuantiles te�ricos", y = "Cuantiles observados")

# Pero bajo supuesto de poblaci�n desconocida,con desviaci�n 
# est�ndar estimada <- t de student

t.student.diff.n <- function(n){
  y <- sample(tratamiento.pop,n)
  x <- sample(control.pop,n)
  t.test(y, x)$statistic
}

# simulaci�n de Monte Carlo de t de student, para distintos N
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
  labs(x = "Cuantiles te�ricos", y = "Cuantiles observados t-student")


# == PRUEBA T DE STUDENT ====
# (dataset espec�fico de ratas hembras)
dat <- read.csv("femaleMiceWeights.csv")

# capturar hembras en dieta de control
control.pop <- dat %>% filter(Diet == "chow") %>%
  dplyr::select(Bodyweight) %>% unlist() 

# capturar hembras bajo hf
tratamiento.pop <- dat %>% filter(Diet == "hf") %>%
  dplyr::select(Bodyweight) %>% unlist()

# analizando funci�n t de student, t.test()
resultado <-t.test(dat$Bodyweight[dat$Diet=="hf"], 
                   dat$Bodyweight[dat$Diet=="chow"])

# imprimir resultado
resultado

# como se ve �sta diferencia bajo distribuci�n normal??
pnorm(resultado$statistic, lower.tail = F) * 2 # por 2 porque distribuci�n es
                                               # sim�trica

# diferencias?? Si, supuestos de CLT y t-student son distintos. 
# CLT -> dice que hay efecto cuando no lo hay (error de tipo I)
# t student -> dice que NO hay efecto cuando SI lo hay (error de tipo II)

# == INTERVALOS DE CONFIANZA EN T DE STUDENT ====

# imprimir CI
resultado$conf.int

# calculando CI a mano
# recorda que operamos bajo int�rvalo de 
# \bar{X}-Q*\frac{s_x}{\sqrt{N}}\leq\mu_x\leq\bar{X}+Q*\frac{s_x}{\sqrt{N}}  

# importamos poblaci�n de ratas
dat <- read.csv("mice_pheno.csv")

# extramos poblaci�n de ratas femeninas en dieta normal
pop.control <- dat %>% 
  filter(Sex == "F", Diet == "chow") %>% 
  dplyr::select(Bodyweight)

# capturar media poblacional
media.pop <- mean(pop.control$Bodyweight) 

# muestra de ratas
muestra <- sample(pop.control$Bodyweight, 30)

# calculamos par�metros
media <- mean(muestra) # \bar{X}
Q <- qnorm(1 - 0.05/2) # Q
se <- sd(muestra)/sqrt(30) # \frac{s_x}{\sqrt{N}}
intervalo <- c(media - Q*se,media, media + Q*se) # \mu_x\pm\bar{X}+Q*\frac{s_x}{\sqrt{N}} 
intervalo[1] <= media.pop & 
  intervalo[3] >= media.pop # int�rvalo contiene a la media poblacional??

# funci�n para obtener int�rvalo
get.intervalo <- function(i, vector, n.muestra, distribucion){
  #'@param i n�mero de iteraci�n
  #'@param vector vector con valores de la poblaci�n
  #'@param n.muestra valor entero con tama�o de la muestra
  #'@param distribucion cadena con el tipo de distribuci�n a modelar ("qn"== dist normal, "qt"== dist t-student)
  #'@return vector con int�rvalos de confianza, media muestra e iteraci�n
  
  muestra <- sample(vector, n.muestra) # puedes probar cambiando el tama�o muestral
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

# generar funci�n que devuelva dataset
get.simulacion <- function(vector, n.simulaciones, n.muestra, distribucion){
  #'@param vector el vector con datos de poblaci�n
  #'@param n.simulaciones n�mero entero que indica cantidad de simulaciones a correr
  #'@param n.muestra n�mero entero que indica tama�o de la muestra
  #'@param distribucion cadena con el tipo de distribuci�n 
  #'@return data.frame con datos de la simulaci�n
  
  lista <- list()
  for(i in 1:n.simulaciones){
    lista[[i]] <- get.intervalo(i, vector, n.muestra, distribucion)
  }
  simulacion <- do.call("rbind", lista)
  colnames(simulacion) <- c("iteracion", "ic_menor", "media.m","ic_superior", "contiene")
  simulacion <- as.data.frame(simulacion)
  return(simulacion)
  
}# fin get.simulacion

# se corren 100 simulaciones, podemos probar con m�s, y ver que ocurre!
# tambi�n puedes probar con distintos tama�os muestrales, actualmente est� en 30
simulacion <- get.simulacion(pop.control$Bodyweight, 100, 30, "qn")


# visualizaci�n de los int�rvalos
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

# visualicemos que ocurre con una muestra mucho m�s peque�a
simulacion <- get.simulacion(pop.control$Bodyweight, 100, 5, "qn")

# visualizaci�n de los int�rvalos
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

# si achicamos los tama�os muestrales, vemos que el CLT falla, ya que �sto introduce mayor
# variabilidad en los datos, e infla las colas de la distribuci�n (fat tails). 
# El CLT, est� contenido en �ste elemento
Q <- qnorm(1 - 0.05/2) # el cual asume que distribuci�n es normal

# Pero podemos hace �sta aproximaci�n, bajo condiciones de colas gordas, por medio de
# una distribuci�n t, para lo cual cambiamos �ste t�rmino, que incorpora grados de libertad
Q <- qt(1 - 0.05/2, df = 4)

# cambiaremos el t�rmino final de la funci�n, para simular esa distribuci�n
simulacion <- get.simulacion(pop.control$Bodyweight, 100, 5, "qt")

# visualizaci�n de los int�rvalos
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

# con ello recuperamos el int�rvalo de confianza del caso anterior.

# == V�NCULO P.VALOR E INT�RVALO DE CONFIANZA ====

# por ejemplo en una prueba t, 
t.test(dat$Bodyweight[dat$Sex=="F" & dat$Diet=="hf"],
       dat$Bodyweight[dat$Sex=="F" & dat$Diet=="chow"])$conf.int 
# dado que estamos comparando diferencias medias,se espera que tal diferencia
# nunca sea 0, ya que ello implicar�a que variable aleatoria (diferencia media muestral)
# es un valor extremo respecto de la distribuci�n bajo supuesto de Ho (hacer memoria)
# y OJO, en este caso, que esperamos que valor > 0 en el int�rvalo nos da resultado significativo,
# en otros puede esperar que valor estimado, sea por ejemplo, !=1, en cuyo
# caso se espera que int�rvalo no contenga 1 para ser significativo (pudiendo ser menor o mayor a 1)


# == POWER CALCULATIONS ===========================

# importamos poblaci�n de ratas
dat <- read.csv("mice_pheno.csv")

# calcularemos una especie de "tama�o de efecto" del tratamiento de dieta
# alta en grasas en el peso de las ratas

# sacamos las problaciones de control y tratamiento en ratas hembras
ctrlPop <- dat %>% filter(Sex == "F" & Diet == "chow") %>%
  dplyr::select(Bodyweight) %>% unlist()

tratPop <- dat %>% filter(Sex == "F" & Diet == "hf") %>%
  dplyr::select(Bodyweight) %>% unlist()

# calculamos el porcentaje de aumento en peso dado el tipo de dieta
(mean(tratPop) - mean(ctrlPop))/mean(ctrlPop)

# nos da un ~de 10% 

# veamos si encontramos diferencias medias significativas en muestras peque�as de N = 5
t.test(sample(ctrlPop, 5), sample(tratPop, 5) )$p.value
 
# ?? No es significativa la diferencia, pese a que lo es a nivel poblacional, es falsa
# entonces diferencia? Cometimos un error? No... bueno si, un error de Tipo II
# lo que nos falt� ac� es m�s "poder"

# ac� entran �stos errores, dado que estamos en un mundo de variables aleatorias
# es posible que justo en nuestra muestra no podamos encontrar tal diferencia

# Los errores son 2:
# Error de tipo I -> rechazar la Ho cuando es verdadera, aka falso positivo
# Error de tipo II -> no rechazar la Ho cuando �sta es falsa, aka falso negativo

# Este proceso de aplicaci�n estad�stica, est� mediado por �stos 2 factores
# Si minimizamos la probabilidad de un error, la del otro error aumenta, para remediar
# medianamente eso, ocupamos el c�lculo de poder que �s "la probabilidad de rechazar
# la hip�tesis nula cuando �sta es falsa", la cual es, para variar, otra convenci�n

# Este poder est� directamente relacionado al tama�o muestral (en realidad tiene que ver 
# tambi�n con el alpha de <.05 definido arbitrariamente, pero dado que es convenci�n, 
# nos remitiremos solo al tama�o muestral)

# Crearemos una funci�n que nos devolver� los p.valores

p.valor <- function(n, tratamiento, control){
  return(
    t.test(sample(tratamiento, n), sample(control, n))$p.value
  )
}

p.valor(12, tratPop, ctrlPop) < .05

# repliquemos la funci�n m�ltiples veces para ver la curva p
p.valores <- replicate(2000,p.valor(12, tratPop, ctrlPop)) %>% as.data.frame()

# grafiquemos
ggplot(p.valores, aes(.)) +
  theme_bw() +
  geom_histogram() +
  # geom_density() +
  geom_vline(xintercept = .05, color = "red", size = 1)

# notar que aunque grueso de resultados quedan por debajo del p.valor <.05, sigue existiendo
# una gran proporci�n que rechaza esa relaci�n, incluso podemos calcular esa proporci�n

mean(p.valores < .05)

# �sto nos da un poder de poco m�s del 20%, bastante bajo
# Veamos como se ven distintasa curvas p, con distintos tam�os muestrales
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


# Y veamos el cuadro general de poder estad�stico para diversos tama�os muestrales
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


# as� tambi�n podemos visualizar el trade off que se da entre el poder estad�stico
# y el p.valor asignado. 

# manipulemos la convenci�n de la significancia de <.05, por otros niveles y veamos
# si existe capacidad de rechazar Ho cuando es falsa

# definimos varios alfa o bandas de significancia
alphas <- c(.1, .05, .01, .001, .0001)

# hacemos la simulaci�n, replicamos 2000 muestreos de 30 sujetos cada uno, sacamos
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
# peque�o solo aumentando el N

# sabemos que en nuestra poblaci�n ese es el caso (existe diferencia media), por lo que
# podemos ver que pasa con la relaci�n p.valor N

# generamos una secuencia de tama�os de muestra con 10 repeticiones cada una
Ns <- seq(10, 200, by = 10)
Ns_rep <- rep(Ns, each=10)

# recuerden que la funci�n p.valor incluye random sampling
# calculamos el p.valor de m�ltiples muestreos de distinto tama�o
ps <- sapply(Ns_rep, p.valor, tratamiento = tratPop, control=ctrlPop)

# generamos df de tama�os y p.valores
df.p_vs_n <- data.frame(ns = Ns_rep, p.valores = ps)


# ploteamos relaci�n (aplicamos escala logaritmica en p.valores para achicar distancias y
# relaci�n se vea m�s clara)
ggplot(df.p_vs_n, aes(ns, log(p.valores))) +
  geom_jitter() +
  # marcamos una l�nea al alpha = .01
  geom_hline(yintercept = log(.01)) +
  # marcamos otra l�nea con el alfa convencional de .05
  geom_hline(yintercept = log(.05))

# si resultado es consistente, p.valores m�s peque�os no tienen m�s sentido!!!

# y aqu� aparaece otro concepto genial, muy subvalorado: el tama�o de efecto!
# esto es, que tanto impacta la variable independiente en el output que tenemos
# en este caso dieta hf -> peso

# hay varias medidas para calcularla, pero la m�s conocida es la d de Cohen, que
# b�sucamente es la diferencia absoluta de medias dividida en la desviaci�n est�ndar


(abs(mean(tratPop) - mean(ctrlPop)))/
  sd(dat$Bodyweight[dat$Sex=="F"]) # --> este es el dataset de la pobla completa















# == SIMULACIONES DE MONTECARLO ====

# y si en vez de levantar datos, los simulamos usando un algoritmo?

dat <- read.csv("mice_pheno.csv")
ctrlPop <- dat %>% filter(Sex == "F" & Diet == "chow") %>%
  dplyr::select(Bodyweight) %>% unlist()

tratPop 

# por ejemplo, podemos similar la distribuci�n de diferencias medias encontradas
# en una poblaci�n, por el puro azar

# creamos una funci�n que nos estad�sticos de t de student
generador.ttest <- function(n, pop){
  return(t.test(sample(pop, n), sample(pop, n))$statistic)
}

# simulamos mil de estos test con simulaci�n de Monte Carlo
ttests <- replicate(1000, generador.ttest(10, ctrlPop))

# graficamos
ggplot() +
  geom_histogram(aes(ttests))

# se puede apreciar que la diferenciar por distribuci�n t de student, se
# asimila bastante a la normal 
ggplot() +
  stat_qq(aes(sample = ttests)) +
  stat_qq_line(aes(sample = ttests))

# pero aqu� estamos trabajando con el vector de la poblaci�n, a la cual no tenemos
# acceso en el mundo real, pero s� podemos jugar con par�metros como el promedio
# o desviaci�n est�ndar para intengar replicar esa data. Veamos

mean(ctrlPop) # <- ~24
sd(ctrlPop) # <- ~3.5

# repitamos lo de arriba editando nuetra funci�n

# creamos una funci�n que nos estad�sticos de t de student
generador.ttest <- function(n, mean, sd){
  # generamos distribuciones ficticias de casos y controles solo con esos
  # par�metros
  casos <- rnorm(n, mean, sd)
  controles <- rnorm(n, mean, sd)
  
  return(t.test(casos, controles)$statistic)
}

# replicamos con los nuevos par�metros
ttests <- replicate(1000, generador.ttest(10, 24, 3.5))

# y graficamos �se parece a nuestra distribuci�n previa? (mas o menos)
ggplot() +
  geom_histogram(aes(ttests))

# simulaci�n con permutaci�n, hata ahora hemos simulado distribuci�n aleatoria de
# diferencias medias, con muestras sobre puestas, pero �quedar� igual si los sujetos
# de una muestra, no est�n en la otra?

dat <- read.csv("femaleMiceWeights.csv")

control <- dat %>% filter(Diet == "chow") %>%
  dplyr::select(Bodyweight) %>% unlist()

tratamiento <- dat %>% filter(Diet == "hf") %>%
  dplyr::select(Bodyweight) %>% unlist()



# simulaci�n montecarlo, hay varias formas de hacer esto, esta es solo
# una de ellas
dif.medias <- replicate(1000, {
  muestra.conjunta <- sample(c(control, tratamiento)) # por defecto sample() toma 12 valores
                                                      # por lo que entre las 2 son 24
  # tomo los primeros 12
  n.control <- muestra.conjunta[1:12]
  # tomo los 12 restantes
  n.tratamiento <- muestra.conjunta[13:24]
  return(mean(n.tratamiento) - mean(n.control))
  })

# grafiquemos y dibujemos que proporci�n de valores m�s extremos podr�an obtenerse
# por el puro azar
ggplot() +
  geom_histogram(aes(dif.medias)) +
  geom_vline(xintercept = mean(tratamiento) - mean(control))

# con esta formula calculamos el porcentaje de valores m�s extremos bajo el puro azar
# se le suma 1 al segundo t�rmino en numerador y al denominador, no tengo idea porque,
# lo dice la literatura
sum(
  abs(dif.medias) > abs(mean(tratamiento) - mean(control)+1)
)/ (length(dif.medias) + 1)

# ensayen con muestras m�s peque�as, y vean qu� proporci�n de valores m�s extremos son
# esperables por el puro azar



# ==================== UNIDAD 2 �LGEBRA MATRICIAL ==========================

# Partamos explorando relaciones que parecen poder ser descritas por asociaciones
# lineales

# digamos que lanzamos 25 pelotas desde la Torre de Pisa, y registramos
# la distancia en que se encuentran en X momento en el tiempo tt

tt <- seq(0, 3.4, len = 25) # tiempo en segundos, de 0 a 3.4 segundos dividido en
                            # 25 instancias

    #altura   #aceleraci�n    #algo de aleatoriedad a cada pelota
d <- 56.67 -   .5*9.8*tt^2   +  rnorm(25, sd = 1)

# la relaci�n se aprecia as�
ggplot() +
  geom_point(aes(x = tt, y = d)) +
  geom_smooth(aes(x = tt, y = d))


# Veamos otro ejemplo, como la heredabilidad de la estatura

# traigamos los datos de estaturas de una muestra de padres e hijos
data("father.son")

# grafiquemos
ggplot(father.son, aes(fheight, sheight)) +
  geom_point() +
  geom_smooth(method = "lm")

# veamos la relaci�n entre una dieta alta en grasas vs una normal
dat <- read.csv("femaleMiceWeights.csv")

ggplot(dat, aes(Diet, Bodyweight, colour = Diet)) +
  geom_boxplot() +
  geom_jitter() +
  geom_smooth(aes(group = 1), method = "lm")


# como se aprecia, todas estas relaciones parecen ser relativamente bien modeladas
# por una recta, algunas con m�s o menos error (ver �ltima relaci�n)
# esta recta es un coeficiente (beta) que minimiza las distancias entre cada punto
# la que se conoce como "m�nimos cuadrados"

# vamos a ver como manipular matricialmente este tipo de relaciones

# == VECTORES, MATRICES Y ESCALARES ====

# Cada uno de los valores por casos en una columna de un dataset, pueden ser
# pensados como vectores, ejemplo

father.son$fheight # esto puede ser visto como un vector

# de tal forma que el dataset completo, pueda ser pensado como una matriz
# y a su vez una matriz, como una colecci�n de vectores

# Por cierto R tiene el objeto Matriz para trabajarlo
mat <- matrix(1:100, nrow = 10, ncol = 10)

# explorando una matriz: dimensiones
dim(mat)
# obtener columnas o filas
mat[,3] # columna
mat[7,] # fila
mat[1,10] # valor espec�fico

# por �ltimo un escalar, es solo un n�mero

# == OPERACIONES MATRICIALES ====

# multiplicar por un escalar
mat <- matrix(1:12,3,4)
mat
2 * mat

# trasponer
t(mat)

# multiplicar matrices
mat <- matrix(c(1,3,2,1,-2,1,1,1,-1),3,3)
vec <- c(3:1)

mat %*% vec

# la matriz de identidad
# es analoga al n�mero 1. Cualquier matrix multiplicada por la I, da
# la misma matriz
diag(5)

# la matriz inversa o X^-1
# la propiedad de esta matriz, es que al multiplicar una matriz por su
# inverso, da la I
# pero no toda matriz tiene un inverso
mat <- matrix(c(1,3,2,1,-2,1,1,1,-1),3,3)
solve(mat)

# breve ejercicio
# Digamos que tenemos el siguiente sistema de ecuaciones

# 1a + 1b + 1c = 6
# 3a - 2b + 1c = 2
# 2a + 1b - 1c = 1

# queremos despejar a, b y c, para ello usaremos algebra matricial
# primero crearemos una matriz con los valores enteros a la izquierda
# del sistema de ecuaciones
mat <- matrix(c(1,3,2,1,-2,1,1,1,-1),3,3)
mat
# luego creamos el vector con las soluciones
result <- c(6, 2, 1)

# finalmente aplicamos la f�rmula, que es multiplicar el vector de resultado por X^-1
solve(mat)%*%result


















