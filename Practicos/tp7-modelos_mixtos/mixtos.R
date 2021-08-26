library(nlme)
library(ggplot2)
library(tidyverse)
library(magrittr)

rm(list = ls())

p1 = ggplot(data = Rail, aes(x=travel, y=Rail)) + geom_point()

# Analizar los datos con un modelo de efectos fijos


# ajusto modelo lineal
fm1Rail.lm <- lm(travel ~ 1, data = Rail)


# el modelo me da una estimación de beta y de sigma
beta  = fm1Rail.lm$coefficients["(Intercept)"]
sigma = summary(fm1Rail.lm)$sigma

# obtengo los residuos, los guardo en un dataset propio
Rail_datos <- Rail %>% mutate(res = resid(fm1Rail.lm)) %>% tibble()

p2 = ggplot(Rail_datos, aes(x = res, y = Rail)) + geom_boxplot()


# mis datos están agrupados, los efectos debido a los grupos están incorporados
# los residuos (cada residuo tiene un "sesgo" debido al riel que me dió esa medición)

# Voy a incorporar los grupos en mi modelo (pero ojo que sigo laburando con efectos fijos!!)

# $$ y_{ij} = \beta_{i} + \epsilon_{ij} (con i y j igual que antes) $$

# lo que cambia es que ahora tengo un beta para cada riel (pero no estoy considerando a los rieles aleatorios!)
# epsilon y sigma son iguales que antes

fm2Rail.lm <- lm(travel ~ Rail - 1, data = Rail)

# el -1 en la formula elimina una ordenada en el modelo

# el sigma es mucho menor que en el caso anterior
summary(fm2Rail.lm)$sigma

# ¿cuantos grados de libertad gasté? df = 12, antes eran 17, gasté 5, (uno por cada riel menos 1)
Rail_datos %<>% mutate(res2 = resid(fm2Rail.lm))

p3 = ggplot(Rail_datos, aes(x = res2, y = Rail)) + geom_boxplot()

# como son los beta?
summary(fm2Rail.lm)

# Tengo un beta para cada riel: estoy modelando la muestra particular de rieles que obtuve
# me gustaria modelar la población de rieles (ya que los obtuve aleatoriamente)
# ¿como es la estimacion de la variabilidad de los rieles?
# y como dijimos antes, este enfoque me hace perder grados de libertad (cada riel que incluyo en la muestra me cuesta un parametro extra)

# Vamos a plantear un modelo de efectos mixtos. Vamos a empezar reescribiendo el modelo
# de efectos fijos 2:

# $$ y_{ij} = \bar{beta} + (\bar{\beta} - \beta_i) + \epsilon_{ij} $$
# donde \hat{\beta} es el promedio de los beta de los rieles

# Vamos a definir un modelo de efectos mixtos donde
# \hat{\beta} lo reemplazamos por el tiempo medio de viaje sobre la poblacion de rieles
# y las desviaciones beta_i - \bar{\beta} por una variable aleatoria que viene de una
# distribucion que vamos a tratar de estimar

# El nuevo modelo es este:

#$$y_{ij} = \beta + b_i + \epsilon_{ij}$$

#donde beta es (una estimación de) el tiempo medio sobre la poblacion de rieles
# y b_i es (una realizacion de) una variable aleatoria que representa la variabilidad
# de cada riel presente en nuestra muestra respecto a la poblacion de rieles
# y epsilon es lo mismo de siempre

# Para terminar de definir el modelo, debemos especificar las distribuciones que generan
# las variables aleatorias

# $$b_i = N(0, \sigma_b) $$
# $$\epsilon_ij = N(0, \sigma) $$

# esto es un modelo jerarquico (porque tiene dos niveles), ya que representa datos que tambien
# son jerarquicos

# Tengo tres paraemtros (puedo incluir 1000 rieles y siempre voy a tener tres parametros)
# beta, sigma_b y sigma

fm1Rail.lme <- lme(travel ~ 1, data = Rail, random = ~ 1 | Rail)

# Tengo beta = 66.5
# sigma de rieles = 24.8
# sigma = 4.02 (curiosamente es el mismo del modelo de efectos fijos 2) (esto no siempre va a ser asi, en este caso se debe a que tenemos la misma cantidad de observaciones para cada riel)

Rail_datos %<>% mutate(res3 = resid(fm1Rail.lme, level=1))

p4 = ggplot(Rail_datos, aes(x = res3, y = Rail)) + geom_boxplot()

Rail_datos %<>% mutate(pred_fijo = predict(fm1Rail.lme, level=0),
                       pred_random = predict(fm1Rail.lme, level=1))




%%%%%%% PARTE 2

library(tidyverse)
library(nlme)

n_suj = 500

a=3
b=1

x = -3:3



pendientes = rnorm(n=n_suj, mean=0, sd=1) + a
ordenadas  = rnorm(n=n_suj, mean=0, sd=2) + b

sujetos = tibble(id_suj = 1:n_suj, a = pendientes, b=ordenadas, s=5)

datos = sujetos %>%
  uncount(length(x)) %>%
  mutate(x = rep(x, times=n_suj)) %>%
  mutate(y = x*a + b + rnorm(n=length(x), mean=0, sd=s)) %>%
  select(-a,-b,-s)

p1 = ggplot(datos, aes(x=x, y=y, group=id_suj)) + geom_point(aes(color=id_suj)) + geom_line()


model = lme(y ~ x, random = ~ x|id_suj, data=datos)

