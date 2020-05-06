
library(rethinking)
library(tidyverse)
options(scipen = 999)

# GRID APROXIMATION  for bayessian analysis

# define the grid
p_grid <- seq(0,1, length.out = 20)

# define prior
prior <- rep(1, 20)

# definir verosimilitud o likelihood (en este caso, obtener 6 "éxitos" en 9 "lanzamientos",
# dada una probabilidad "p")
# recordar que binom es 0 y 1
likelihood <- dbinom(6, size = 9, prob = p_grid)

# cheat sheet:
#              d"distribucion" = density
#              p"distirbucion" = p cumulativa
#              r"distribucion" = muestreo aleatorio

# sacamos la probabilidad posterior (dividimos por suma pa convertir en probabilidad)
posterior <- (likelihood * prior) / sum(likelihood * prior)

df.pior_post <- data.frame(p_grid, posterior)

ggplot(df.pior_post, aes(p_grid, posterior)) +
  geom_point() +
  geom_line()


# encapsular en función que cambie el parámetro 20, por tamaños más pequeños


# APROXIMACIÓN CUADRÁTICA





