library(tidyverse)
library(magrittr)
library(livecode)

server <- livecode::serve_file()

# leer los datos
setwd("~/Dropbox/Estadistica/Inferencia_con_R_UNQ2021/Practicos/tidyverse")

dictionary_tbl <- read_csv("./data/dictionary.csv")
summer_tbl <- read_csv("./data/summer.csv")
winter_tbl <- read_csv("./data/winter.csv")

# 1. ####
# Empecemos con algo sencillo. Usemos `View()` para visualizar `summer_tbl`. 
# En la columna `Athlete` ¿Qué pasa en los deportes de equipo? 
# ¿Esta bien si sumamos las medallas tal cual están en el `tibble`? ¿Cómo podemos hacer para solucionarlo?

# Acomodemos el tibble
summer_por_disciplina_tbl <- summer_tbl %>% 
  group_by(Year, City, Sport, Country, Discipline, Event, Gender, Medal) %>%
  summarise() # No hace ninguna operación

# Acomodemos el tibble
winter_por_disciplina_tbl <- winter_tbl %>% 
  group_by(Year, City, Sport, Country, Discipline, Event, Gender, Medal) %>%
  summarise() # No hace ninguna operación

# 2. ####
# Ahora que ya tenemos las medallas por país (y no por atleta `r emo::ji("wink")`) para cada disciplina, 
# usando `group_by()` y los verbos de *{dplyr}*, creen un nuevo tibble (`oro_por_pais`) que tenga, 
# de forma ordenada, la cantidad de medallas de oro que ganó cada país en toda la historia de los Juegos Olímpicos de verano.

oro_por_pais <- summer_por_disciplina_tbl %>%
  filter(Medal == "Gold") %>%
  group_by(Country) %>%
  summarise(medallas = n()) %>%
  arrange(desc(medallas))
oro_por_pais
  
# 3. ####
# ¿Y si ahora queremos ver cuántas de Oro, de Plata y de Bronce? 
# ¿Cómo deberíamos modificar el *pipe* `%>%` de análisis?

medallas_por_pais <- summer_por_disciplina_tbl %>% 
  group_by(Country, Medal) %>% 
  summarise(medallas = n())
medallas_por_pais

medallas_por_pais_wide <- medallas_por_pais %>% 
  pivot_wider(names_from = Medal, 
              values_from = medallas, 
              values_fill = 0)
medallas_por_pais_wide

# 4. #### 
# ¿Y para quedarnos con los países en `oro_por_pais` con más de 5 medallas de oro? 
# ¿Está Argentina`r emo::ji("Argentina")` (`ARG`) en ese grupo?

paises_5_medallas <- oro_por_pais %>% 
  filter(medallas > 5)
  
# 5. #### 
# ¿Quién ganó más medallas de oro en Hockey sobre hielo (`Ice Hockey`) masculino, Canadá (`CAN`) o la Unión Soviética (`URS`)? 

winter_por_disciplina_tbl %>% 
  #filter(Country %in% c("CAN", "URS"))
  filter(Country=="CAN" | Country=="URS") %>%
  filter(Discipline == "Ice Hockey") %>% 
  filter(Medal == "Gold") %>% 
  filter(Gender == "Men") %>%
  group_by(Country) %>% 
  summarise(medallas = n())

# 6. ####
# Armemos un `tibble` con los nombres de los medallistas olímpicos argentinos en deportes acuáticos y la disciplina, evento y medalla que ganaron. 
 
summer_tbl %>% filter(Country == "ARG" & Sport == "Aquatics") %>%
  select(all_of(c("Athlete", "Discipline", "Event", "Medal")))

# 7. ####
# ¿Cuáles son los tres países con más medallas de oro en total (juegos de invierno y verano)? 
   
full_tbl <- rbind(winter_por_disciplina_tbl, summer_por_disciplina_tbl)
  
full_tbl %>%
  filter(Medal == "Gold") %>%
  group_by(Country) %>%
  summarise(medallas = n()) %>%
  arrange(desc(medallas)) %>%
  slice_head(n = 3)
  
# 8. #### 
# Aprovechemos los datos de `dictionary_tbl` para pensar un poco en la cantidad de medallas por millón de habitantes. 
# Armemos un `tibble` que contenga la cantidad de medallas *per cápita* por país. ¿Qué pasó con la Unión Soviética (`URS`)?

full_tbl %>% group_by(Country) %>%
  summarise(medallas = n()) %>%
  rename("Code" = "Country") %>%
  left_join(dictionary_tbl) %>% 
  drop_na() %>%
  mutate(medals_per_million = medallas/Population * 1e6) %>%
  select(all_of(c("Country", "medals_per_million"))) %>%
  arrange(desc(medals_per_million))

# 9. #### 
# ¿Cuántos medallistas olímpicos de invierno que ganaron bronce tienen de nombre **John**? 
   
juanes <- winter_tbl %>%
  filter(Medal == "Bronze") %>%
  group_by(Athlete) %>%
  summarize() %>%
  mutate(es_john = str_detect(Athlete, "John", negate = FALSE)) %>%
  filter(es_john) %>%
  select(Athlete)
juanes
  

server$stop() 
