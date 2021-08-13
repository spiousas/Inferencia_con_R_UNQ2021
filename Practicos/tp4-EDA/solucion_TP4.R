library(tidyverse)
library(magrittr)
library(livecode)

server <- livecode::serve_file()

# leer los datos
setwd("~/Dropbox/Estadistica/Inferencia_con_R_UNQ2021/Practicos/tp4-EDA/")

netflix <- read_csv("data/NetflixOriginals.csv")

# 1 - Mirar los datos ----------------------------------------------------------
# 
# Usemos las funciones `summary()`, `str()` y `glimpse()` para ver qué estructura 
# y qué tipos de variables tiene nuestro dataset 
# 
# - ¿Tiene valores `NA`? 
# - ¿Alguna de las variables no es del tipo que corresponde? 
# - ¿Hay algún valor sospechoso?
#   
# Una vez corregidos los problemas del dataset imprimir un resumen usando la función `skim()` del paquete *{skimr}*.
# 
# ¿Cuáles son los tres géneros con más estrenos? 
#   
# ¿Y los tres idiomas con más estrenos?

summary(netflix)
str(netflix)
glimpse(netflix)

library(lubridate)
netflix <- netflix %>% 
  mutate(Premiere = mdy(Premiere))
glimpse(netflix)

library(skimr)
skim(netflix)

netflix %>%
  count(Genre) %>%
  arrange(desc(n)) %>%
  slice_head(n = 3)

netflix %>%
  count(Language) %>%
  arrange(desc(n)) %>%
  slice_head(n = 3)

# 2 - Variación ----------------------------------------------------------------
# 
# Exploremos con un simple gráfico de barras la cantidad de películas de cada género. 
# 
# - ¿Cuántos géneros hay?
# - ¿Cómo es la distribución por género?
# 
# Ahora miremos la distribución de duraciones y de rating de **IMDB** ¿Qué podemos decir al respecto?
# 
# Veamos cómo se distribuyen los ratings de **IMDB** para los géneros *Drama* y *Comedy*.
# 
# Por último: ¿Qué pasa con las distribuciones de duraciones para *Comedy* y *Documentary*?
  
netflix %>%
  group_by(Genre) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(Genre, N),
             y = N)) +
  geom_col() +
  coord_flip()

netflix %>%
  ggplot(aes(x = `IMDB Score`)) +
  geom_histogram(bins = 10) +
  theme_minimal()

netflix %>%
  ggplot(aes(x = Runtime)) +
  geom_histogram() +
  theme_minimal()

netflix %>%
  ggplot(aes(x = Premiere)) +
  geom_histogram() +
  theme_minimal()

generos <- c("Drama", "Comedy")
netflix %>%
  filter(Genre %in% generos) %>%
  ggplot(aes(x = `IMDB Score`,
             color = Genre)) +
  geom_freqpoly() +
  theme_minimal()

netflix %>% 
  filter(Genre %in% generos) %>%
  group_by(Genre) %>%
  summarise(mean_rating = mean(`IMDB Score`),
            sd_rating = sd(`IMDB Score`))

generos <- c("Documentary", "Comedy")
netflix %>%
  filter(Genre %in% generos) %>%
  ggplot(aes(x = Runtime,
             color = Genre)) +
  geom_freqpoly() +
  theme_minimal()

netflix %>% 
  filter(Genre %in% generos) %>%
  group_by(Genre) %>%
  summarise(mean_rating = mean(`IMDB Score`),
            sd_rating = sd(`IMDB Score`))

# 3 - Covariación --------------------------------------------------------------
# 
# Utilizando un boxplot veamos si hay alguna relación entre las películas de los géneros *Comedy*, *Drama* y *Documentary* y su rating de **IMDB**.
# 
# Luego, usando la función `geom_tile()` miremos la cantidad de muestras para las combinaciones de los tres géneros y los tres idiomas con más estrenos.
# 
# Ahora vamos a ver la covariación entre dos variables continuas. Vemos si existe alguna relación entre la fecha de estreno y el rating de **IMDB**.
# 
# ¿Y si nos quedamos con los tres géneros más populares y lo vemos por género?
#   
# Por último, utilicemos la función `ggpairs()` de *{GGally}* para ver las distribuciones y correlaciones de todas las variables numéricas de `netflix`.

generos <- c("Drama", "Comedy", "Documentary")
netflix %>%
  filter(Genre %in% generos) %>%
  ggplot(aes(y = `IMDB Score`,
             x = Genre,
             color = Genre)) +
  geom_boxplot() +
  scale_fill_viridis_b() +
  theme_minimal()

idiomas <- c("English", "Spanish", "Hindi")
netflix %>%
  filter(Genre %in% generos) %>%
  filter(Language %in% idiomas) %>%
  count(Genre, Language) %>%
  ggplot(aes(y = Language,
             x = Genre,
             fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal()

netflix %>%
  ggplot(aes(x = Premiere,
             y = `IMDB Score`)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = lm) +
  theme_minimal()

netflix %>%
  filter(Genre %in% generos) %>%
  ggplot(aes(x = Premiere,
             y = `IMDB Score`,
             color = Genre)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = lm,
              se = FALSE) +
  theme_minimal()

library(GGally)
netflix %>% 
  select(all_of(c("Premiere", "Runtime", "IMDB Score"))) %>% 
  ggpairs() +
  theme_minimal()

# 4 - Outliers -----------------------------------------------------------------
# 
# Usemos la librería *{Routliers}* para ver si tenemos *outliers* univariados en las variables `Runtime` (duración) y `IMDB Score` (rating de IMDB)
# 
# ¿Qué podemos decir de los *outliers* de `Runtime`? ¿Los podemos categorizar de alguna forma? ¿Nos pueden dar alguna información sobre las producciones de **Netflix**?
#   
# ¿Y sobre los *outliers* de rating de **IMDB**?
#   
# Por último, analicemos si hay outliers multivariados en ambas variables.

library(Routliers)

outliers_runtime <- outliers_mad(x = netflix$Runtime)
outliers_runtime

plot_outliers_mad(outliers_runtime, 
                  x = netflix$Runtime)

netflix %>% filter(Runtime>outliers_runtime$limits[1] & Runtime<outliers_runtime$limits[2])

netflix %>%
  filter(Runtime < outliers_runtime$limits[1]) %>%
  count(Genre) %>% 
  arrange(desc(n))

netflix %>%
  filter(Runtime > outliers_runtime$limits[2])

outliers_IMDB <- outliers_mad(x = netflix$`IMDB Score`)
outliers_IMDB

plot_outliers_mad(outliers_IMDB, 
                  x = netflix$`IMDB Score`)

netflix %>%
  filter(`IMDB Score` < outliers_IMDB$limits[1])

outliers_multi <- outliers_mcd(x = cbind(netflix$Runtime,
                                         netflix$`IMDB Score`))
outliers_multi

