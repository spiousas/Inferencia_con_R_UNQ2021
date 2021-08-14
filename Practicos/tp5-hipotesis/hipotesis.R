library(tidyverse)
library(magrittr)

rm(list =ls())


## Ejercicio 1

# El siguiente codigo genera una cantidad de muestras de cierto tamaño,
# tomadas de una distribución Gaussiana. Vamos a almacenarlas en un tibble
# que nos permita trabajar con cada muestra:

# lo primero que hacemos es generar la estructura del "experimento"
n_muestras = 10000
n_datos    = 5

datos <- list(id_muestra = 1:n_muestras, id_data = 1:n_datos) %>% 
  cross_df() %>% arrange(id_muestra)

mu    = 100
# mu2   = 110
sigma = 10
set.seed(101)
datos %<>% mutate(val=rnorm(n=n_muestras*n_datos, mean=mu2, sd=sigma))

# a. Calcular la media y el desvío estándar muestrales para cada
# muestra. Para pensar: conviene almacenar esta info en una nueva
# columna en el tibble datos, o en un nuevo tibble?

estadistica <- datos %>% 
  group_by(id_muestra) %>%
  summarise(M = mean(val), S = sd(val), n = n(), .groups="keep")

# b. A partir de los datos del item anterior, graficar la distribución de la
# media muestral. Comparar el histograma obtenido con una función de densidad
# Gaussiana. ¿Qué media y desvío estándar corresponden al histograma?

histog_M <- estadistica %>%
            with(hist(M, breaks = 30, plot = TRUE)) %$%
            tibble(from    = head(breaks, -1),
                   to      = tail(breaks, -1),
                   mids    = mids,
                   counts  = counts,
                   density = density)

density_M <- tibble(x = seq(from=80, to=120, by=0.1)) %>%
             mutate(density = dnorm(x, mean=mu, sd=sigma/sqrt(n_datos)))

# c. Verificar que la distribución de medias muestrales es bien representada
# por una función gaussiana con media mu y desvío estandar sigma/sqrt(n_datos).
# Para hacer esto, analizar la distribución correspondiente a muestras de distintos
# tamaños. ¿A qué corresponde sigma / sqrt(n)?

p1 <- ggplot() +
      geom_col(data=histog_M, aes(x=mids, y=density)) +
      geom_line(data=density_M, aes(x=x, y=density), color='red')

# d. Graficar la distribución del desvío estándar muestral para diferentes
# tamaños de muestra. Discutir si es posible describirlo mediante una distribución
# gaussiana (Para pensar, ¿es posible obtener S < 0?)

histog_S <- estadistica %>%
            with(hist(S, breaks=40, plot=TRUE)) %$% 
            tibble(from    = head(breaks, -1),
                   to      = tail(breaks, -1), 
                   mids    = mids,
                   counts  = counts, 
                   density = density)

# e. Obtener la estadística t correspondiente a cada muestra. Luego obtener
# su distribución, graficarla y compararla con la distribución t de Student
# con los correspondientes grados de libertad. Repetir para diferentes tamaños
# de muestra, y analizar a partir de qué tamaño de muestra la distribución de t
# puede ser bien representada por una distribución gaussiana (pensar con qué
# media y con qué desvío estándar)

estadistica %<>% mutate(t = (M-mu)/S*sqrt(n))

histog_t <- estadistica %>%
            with(hist(t, breaks=300, plot=TRUE)) %$%
            tibble(from    = head(breaks, -1),
                   to      = tail(breaks, -1),
                   mids    = mids,
                   counts  = counts,
                   density = density)

density_t <- tibble(x = seq(from=-5, to=5, by=0.01)) %>%
             mutate(density = dt(x, df=n_datos-1),
                    snd     = dnorm(x, mean=0, sd=1))

p2 <- ggplot() +
  geom_col(data=histog_t %>% filter(mids > -5, mids < 5), aes(x=mids, y=density)) +
  geom_line(data=density_t, aes(x=x, y=density), color='red') +
  geom_line(data=density_t, aes(x=x, y=snd), color='blue')

# f. Obtener la función de probabilidad acumulada de la distribucion de t y
# graficarla. Recordar que para esto es necesario antes calcular la frecuencia
# relativa en cada intervalo del histograma. 

histog_t %<>% 
  mutate(f = counts/sum(counts)) %>% 
  mutate(p_acum = cumsum(f))

density_t %<>% 
  mutate(p_acum = pt(x, df = n_datos-1))

p3 <- ggplot() +
  geom_line(data=histog_t, aes(x=to, y=p_acum), color='red') +
  geom_line(data=density_t, aes(x=x, y=p_acum), color='blue') +
  coord_cartesian(xlim=c(-6, 6),default=TRUE)

# ¿En qué intervalo está comprendido el 5% de valores de t más extremos?
# Comparar para diferentes grados de libertad

# a partir de la simulacion
t_crit_izq = histog_t %>% filter(p_acum < 0.025) %>% select(to) %>% max()
t_crit_der = histog_t %>% filter(p_acum > 1-0.025) %>% select(to) %>% min()
# valor teorico
t_crit = qt(0.025, df=n_datos-1)

p3_izq = p3 +
  geom_hline(yintercept = 0.025, linetype="dashed") +
  geom_vline(xintercept=t_crit, linetype="dashed") +
  geom_vline(xintercept=t_crit_izq) +
  coord_cartesian(xlim=c(1.1*t_crit,0.9*t_crit), 
                  ylim=c(0.02, 0.03), default=TRUE)
  
p3_der = p3 +
  geom_hline(yintercept = 1-0.025, linetype="dashed") +
  geom_vline(xintercept=-t_crit, linetype="dashed") +
  geom_vline(xintercept=t_crit_der) +
  coord_cartesian(xlim=c(-0.9*t_crit,-1.1*t_crit), 
                  ylim=c(0.97, 0.98), default=TRUE)

# g. Calcular el probabilidad de ocurrencia de cada muestra obtenida. Elegir
# las más extremas (menos probables) y graficar algunas de ellas. Comparar con
# las menos extremas.

estadistica %<>% mutate(pval = 2*pt(-abs(t), df = n_datos-1)) %>% arrange(-pval)

# el mas distinto
datos %>%
  filter(id_muestra == (estadistica %>% pull(id_muestra))[1]) %>%
  with(hist(val, plot = TRUE))

# el mas parecido
datos %>%
  filter(id_muestra == (estadistica %>% pull(id_muestra))[1000]) %>%
  with(hist(val, plot = TRUE))

p4 <- ggplot(estadistica, aes(x=pval, y=M)) + geom_point()

# h. Graficar la distribucion de p-values. ¿Qué proporcion de p-values son menores que
# alfa = 0.05? ¿y menores que 0.1, 0.2, 0.5, etc.? Pruebe distintos valores

estadistica %>%
  with(hist(pval, plot=TRUE))

proporcion <- (estadistica %$% sum(pval < 0.05)) / n_muestras * 100

# i. Obtenga un dataset generado por una distribucion con diferente
# media (mu2). Obtenga la estadistica t para cada muestra, y los
# correspondientes p-values, pero haga de cuenta que ud. no sabe que 
# la media de la distribucion de origen fue modificada. (Esto
# significa que al calcular t debe usar el valor de mu empleado
# inicialmente, ya que Ud. no está al tanto de que el mismo fue
# modificado). Analice la distribucion de p-values en este caso.

# Qué proporcion de p-values son menores que alfa=0.05? (Pruebe
# con distintos valores de alfa)

# ¿Como se modifica todo esto para distintos valores de mu2?

# ¿Como influye el tamaño de muestra?

