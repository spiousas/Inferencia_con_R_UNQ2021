library(tidyverse)
library(magrittr)
library(skimr)
library(corrplot)

setwd("~/Dropbox/Estadistica/Inferencia_con_R_UNQ2021/Presentaciones/7-1/example")

pm <- read_csv("./data/pm25_data.csv")
save(pm, file = "./data/imported_pm.rda")

pm %>%
  dplyr::glimpse()


# Convertir esas tres columnas numéricas a factores
pm %<>%
  mutate(across(c(id, fips, zcta), as.factor)) 

glimpse(pm)


skimr::skim(pm)

pm %>% 
  dplyr::distinct(state) 

pm %>% dplyr::filter(city == "Albuquerque")
pm %>% dplyr::filter(city == "Baltimore")

pm %>% 
  dplyr::filter(city %in% c("Baltimore", "Albuquerque")) %>% 
  select(all_of(c("city", "county_area", "county_pop")))

PM_cor <- cor(pm %>% dplyr::select_if(is.numeric))
corrplot::corrplot(PM_cor, tl.cex = 0.5)

corrplot(abs(PM_cor), order = "hclust", tl.cex = 0.5, cl.lim = c(0, 1))
         
         