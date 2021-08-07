library(tidyverse)
library(magrittr)
library(livecode)

# leer los datos
setwd("~/Dropbox/Estadistica/Inferencia_con_R_UNQ2021/Practicos/ggplot2/")

dictionary_tbl <- read_csv("../tidyverse/data/dictionary.csv")
summer_tbl <- read_csv("../tidyverse/data/summer.csv")
winter_tbl <- read_csv("../tidyverse/data/winter.csv")

# Acomodemos el tibble
summer_por_disciplina_tbl <- summer_tbl %>% 
  group_by(Year, City, Sport, Country, Discipline, Event, Gender, Medal) %>%
  summarise() # No hace ninguna operación

# Acomodemos el tibble
winter_por_disciplina_tbl <- winter_tbl %>% 
  group_by(Year, City, Sport, Country, Discipline, Event, Gender, Medal) %>%
  summarise() # No hace ninguna operación

# 1. #### 
# Retomemos `summer_por_disciplina_tbl` y grafiquemos la cantidad de medallas de oro de los 10 países con 
# más medallas usando barras.

summer_por_disciplina_tbl %>%   
  filter(Medal == "Gold") %>%
  group_by(Country) %>%
  summarise(Medallas = n()) %>%
  arrange(desc(Medallas)) %>%
  slice_head(n=10) %>%
  ggplot(aes(x = reorder(Country, Medallas), 
             y = Medallas)) +
  geom_col(colour = "white",
           fill = "#1380A1",
           width = 1) +
  labs(x = "País",
       y = "Medallas de oro acumuladas",
       title = "Top 10 países con más medallas de oro aculumadas") +
  coord_flip() +
  theme_minimal()

# 2. #### 
# Ahora veamos la distribución de las medallas de oro acumuladas por país ¿Qué podemos decir al respecto?

labels_outliers <- tibble(x = c(979, 396),
                          y = c(2, 2),
                          label = c("USA", "URSS"))

summer_por_disciplina_tbl %>%
  group_by(Country) %>%
  filter(Medal == "Gold") %>%
  summarise(Medallas = n()) %>%
  ggplot(aes(x = Medallas)) + 
  geom_histogram(colour = "white",
                 fill = "#1380A1") +
  geom_text(data = labels_outliers,
             aes(x = x,
                 y = y,
                 label = label),
             vjust = 0) +
  labs(x = "Medallas de oro acumuladas",
       y = NULL,
       title = "Distribución de medallas de oro") +
  theme_minimal()

# 3. #### 
# ¿Y si queremos ver las distribuciones de los tres tipos de medallas?

histograma_base <- summer_por_disciplina_tbl %>% 
  group_by(Country, Medal) %>% 
  summarise(Medallas = n()) %>%
  ggplot(aes(x = Medallas,
             fill = Medal)) +
  geom_histogram(position = "dodge") +
  labs(x = "Medallas acumuladas",
       y = NULL,
       fill = NULL,
       title = "Distribución de medallas acumuladas") +
  scale_fill_manual(breaks = c("Gold", "Silver", "Bronze"),
                    values = c("#BC9B69", "#C4C5C7", "#B88748")) +
  theme_minimal() +
  theme(legend.position = "top")
histograma_base

# 4. #### 
# Tratemos de hacer algo más informativo utilizando utilizando `facet_grid()`.

histograma_base + facet_grid(Medal ~ ., labeller = label_both) +
  theme(legend.position = "none")

# 5. #### 
# ¿Y si queremos ver la distribución de medallas de oro entre países a lo largo de la historia de los juegos de invierno?
   
winter_por_disciplina_tbl %>%
  filter(Medal == "Gold") %>%
  group_by(Country, Year) %>%
  summarise(Medallas = n()) %>%
  ggplot(aes(x = Medallas)) +
  geom_histogram(fill = "#BC9B69",
                 position = "dodge") +
  labs(x = "Medallas de oro por juego",
       y = NULL,
       fill = NULL,
       title = "Distribución de medallas acumuladas") +
  theme_minimal() + 
  facet_wrap(. ~ Year)

# 6.  ####
# Vayamos de nuevo a algo un poquito más complicado. Usemos `dictionary_tbl` para hacer una figura que nos muestre la 
# relación entre las  medallas de oro acumuladas por país entre juegos de invierno y de verano y su **PBI per cápita**.
 
summer_por_disciplina_tbl %>% 
  rbind(winter_por_disciplina_tbl) %>% 
  filter(Medal == "Gold") %>%
  group_by(Country) %>%
  summarise(Medallas = n()) %>%
  rename("Code" = "Country") %>%
  left_join(dictionary_tbl) %>%
  drop_na() %>%
  filter(Code != "USA") %>%
  ggplot(aes(x = `GDP per Capita`,
             y = Medallas)) +
  geom_point() +
  labs(x = "PBI per cápita",
       y = "Medallas de oro acumuladas",
       title = "Medallas de oro acumuladas vs. PBI per cápita") +
  geom_point(color = "#1380A1") +
  theme_minimal()

# 7.  ####
# ¿Y si queremos ver si hay diferencias entre juegos de invierno y de verano?
 
summer_por_disciplina_tbl %>% 
  mutate(Season = "Summer") %>%
  rbind(winter_por_disciplina_tbl %>% mutate(Season = "Winter")) %>% 
  filter(Medal == "Gold") %>%
  group_by(Country, Season) %>%
  summarise(Medallas = n()) %>%
  rename("Code" = "Country") %>%
  left_join(dictionary_tbl) %>%
  drop_na() %>%
  filter(Code != "USA") %>%
  ggplot(aes(x = `GDP per Capita`,
             y = Medallas,
             color = Season)) +
  geom_point() + 
  labs(x = "PBI per cápita",
       y = "Medallas de oro acumuladas",
       color = NULL,
       title = "Medallas de oro acumuladas vs. PBI per cápita") +
  scale_color_manual(values = c("#FAAB18", "#1380A1")) +
  theme_minimal() +
  theme(legend.position = "top")

# 8. #### 
# Ahora veamos los tres tipos de medallas acumuladas para los juegos de verano vs. el **PBI per cápita** y 
# agreguemos una capa de `geom_smooth()` ¿Qué pasa si sacamos a `USA`?
   
summer_por_disciplina_tbl %>% 
  group_by(Country, Medal) %>%
  summarise(Medallas = n()) %>%
  rename("Code" = "Country") %>%
  left_join(dictionary_tbl) %>%
  drop_na() %>%
  filter(Code != "USA") %>%
  ggplot(aes(x = `GDP per Capita`,
             y = Medallas,
             color = Medal)) +
  geom_point(size = 2,
             alpha = .7) +
  geom_smooth(method = lm,
              se = FALSE) +
  labs(x = "PBI per cápita",
       y = "Medallas de oro acumuladas",
       color = NULL,
       title = "Medallas de oro acumuladas vs. PBI per cápita") +
  scale_color_manual(breaks = c("Gold", "Silver", "Bronze"),
                     values = c("#BC9B69", "#C4C5C7", "#392916")) +
  theme_minimal() +
  #facet_grid(Medal ~ .) +
  theme(legend.position = "top")

# 9. ####  
# Ahora concentrémonos en Argentina`r emo::ji("Argentina")`. Veamos el histórico de medallas de 
# Argentina en los Juegos Olímpicos de verano.

summer_por_disciplina_tbl %>% 
  filter(Country == "ARG") %>%
  mutate(Juego = paste0(Year, " - ", City),
         Medal = factor(Medal, levels=c("Gold","Silver","Bronze"))) %>%
  group_by(Juego, Medal) %>%
  summarise(Medallas = n()) %>%
  ggplot(aes(x = Juego,
             y = Medallas,
             color = Medal)) +
  geom_point(size = 4) +
  geom_line(aes(group = Medal)) +
  labs(x = NULL,
       y = "# Medallas",
       color = NULL,
       title = "Medallero histórico de Argentina") +
  scale_x_discrete(limits = rev) +
  coord_flip() +
  scale_color_manual(breaks = c("Gold", "Silver", "Bronze"),
                     values = c("#BC9B69", "#C4C5C7", "#392916")) +
  theme_minimal() +
  facet_grid(. ~ Medal) +
  theme(legend.position = "top",
        plot.title.position = "plot")
  
# 10. ####  
# Grafiquemos el nombre de los paises que forman el podio del medallero para todos los Juegos Olímpicos de Verano.
 
library(ggpubr)
summer_por_disciplina_tbl %>% 
  mutate(Juego = paste0(Year, " - ", City),
         Medal = factor(Medal, levels=c("Gold","Silver","Bronze"))) %>%
  filter(Medal == "Gold") %>%
  group_by(Juego, Country) %>%
  summarise(Medallas = n()) %>%
  ungroup() %>%
  group_by(Juego) %>%
  arrange(desc(Medallas), .by_group = TRUE) %>%
  slice_head(n = 3) %>%
  mutate(Posicion = 1:3,
         label = paste0(Country, " ", Medallas)) %>%
  ggplot(aes(x = Juego,
             y = Posicion,
             color = factor(Posicion))) +
  geom_text(aes(label = label)) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(breaks = 1:3,
                     labels = c("Primero", "Segundo", "Tercero")) +
  coord_flip(clip = "off") +
  scale_color_manual(breaks = 1:3,
                     values = c("#BC9B69", "#C4C5C7", "#392916")) +
  labs(x = NULL,
       y = NULL,
       color = NULL,
       title = "Medallero histórico") +
  theme_pubclean() +
  theme(legend.position = "none",
        plot.title.position = "plot",
        axis.ticks.y = element_blank())

# 11. ####  
# ¿Podremos reemplazar los nombres en el medallero por sus banderas?

library(ggflags)
library(countrycode)

summer_por_disciplina_tbl %>% 
  mutate(Juego = paste0(Year, " - ", City),
         Country = case_when(Country == "URS" ~ "RUS",
                             Country == "GDR" ~ "GER",
                             Country == "ROU" ~ "ROM",
                             Country == "FRG" ~ "GER",
                             Country == "EUN" ~ "RUS",
                             TRUE ~ Country)) %>%
  filter(Medal == "Gold") %>%
  group_by(Juego, Country) %>%
  summarise(Medallas = n()) %>%
  arrange(desc(Medallas), .by_group = TRUE) %>%
  slice_head(n = 3) %>%
  rename("Code" = "Country") %>% 
  left_join(dictionary_tbl %>% select(all_of(c("Code", "Country")))) %>%
  mutate(Posicion = 1:3,
         country_id = countrycode(Country,
                                  origin = "country.name",
                                  destination = "genc2c") %>% tolower()) %>%
  ggplot(aes(x = Juego,
             y = Posicion,
             color = factor(Posicion))) +
  geom_flag(aes(country = country_id),
            size = 5) +
  labs(x = NULL,
       y = NULL,
       color = NULL,
       title = "Medallero histórico") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(breaks = 1:3,
                     labels = c("Primero", "Segundo", "Tercero")) +
  coord_flip(clip = "off") +
  scale_color_manual(breaks = 1:3,
                     values = c("#BC9B69", "#C4C5C7", "#392916")) +
  theme_pubclean() +
  theme(legend.position = "none",
        plot.title.position = "plot",
        axis.ticks.y = element_blank())
