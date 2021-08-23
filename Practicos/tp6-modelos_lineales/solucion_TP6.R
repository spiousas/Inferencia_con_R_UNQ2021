# install.packages("openintro")
library(tidyverse)
library(openintro)

# Ejercicio 1 ------------------------------------------------------------------
# El modelo
model_starbucks <- lm(calories ~ fat, starbucks)
model_starbucks

summary(model_starbucks)

#Ploteo de los datos
starbucks %>% ggplot(aes(x = fat,
                         y = calories)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# La ecuación y la estimación
# calories = 11.27 * fat + 183.73
calories_19 <- 11.27 * 19 + 183.73
calories_19

# Modelos alternativos
model_starbucks_protein <- lm(calories ~ protein, starbucks)
model_starbucks_protein

model_starbucks_carb <- lm(calories ~ carb, starbucks)
model_starbucks_carb

model_starbucks_fiber <- lm(calories ~ fiber, starbucks)
model_starbucks_fiber

# El modelo con todo
model_starbucks_full <- lm(calories ~ fiber + protein + carb + fat, starbucks)
summary(model_starbucks_full)

# Los parámetros
library(parameters)
model_parameters(model_starbucks_full)

summary(model_starbucks)
summary(model_starbucks_full)

model_starbucks_full_2 <- lm(calories ~ protein + carb + fat, starbucks)
summary(model_starbucks_full_2)

# Outliers
model_starbucks
starbucks_outlier1 <- starbucks %>% 
  select(all_of(c("calories", "fat"))) %>%
  rbind(tibble(fat=25, calories=150))

starbucks_outlier1 %>% ggplot(aes(x = fat,
                                  y = calories)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

model_starbucks_outlier1 <- lm(calories ~ fat, starbucks_outlier1)
model_starbucks_outlier1

starbucks_outlier2 <- starbucks %>% 
  select(all_of(c("calories", "fat"))) %>%
  rbind(tibble(fat=50, calories=150))

starbucks_outlier2 %>% ggplot(aes(x = fat,
                                  y = calories)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

model_starbucks_outlier2 <- lm(calories ~ fat, starbucks_outlier2)
model_starbucks_outlier2

starbucks_outlier3 <- starbucks %>% 
  select(all_of(c("calories", "fat"))) %>%
  rbind(tibble(fat=50, calories=700))

starbucks_outlier3 %>% ggplot(aes(x = fat,
                                  y = calories)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

model_starbucks_outlier3 <- lm(calories ~ fat, starbucks_outlier3)
model_starbucks_outlier3

# Residuos
cbind(starbucks, resid(model_starbucks)) %>%
  rename("residuals" = "resid(model_starbucks)") %>%
  ggplot(aes(x = fat,
             y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color= "#ED6A5A", linetype = "dashed")

# Ejercicio 2 ------------------------------------------------------------------
library(palmerpenguins)

# El modelo
model_penguins <- lm(body_mass_g ~ bill_depth_mm + species, penguins %>% drop_na())
model_penguins

# Peso = -1000.846 + 256.551 * bill_depth_mm + 8.111 * speciesChinstrap + 2245.878 * speciesGentoo
# Especie Adelie
# Peso_Adelie = -1000.846 + 256.551 * bill_depth_mm + 8.111 * 0 + 2245.878 * 0
# Peso_Adelie = -1000.846 + 256.551 * bill_depth_mm 
# Especie Chinstrap
# Peso_Chinstrap = -1000.846 + 256.551 * bill_depth_mm + 8.111 * 1 + 2245.878 * 0
# Peso_Chinstrap = (-1000.846 + 8.111) + 256.551 * bill_depth_mm 
# Especie Gentoo
# Peso_Gentoo = -1000.846 + 256.551 * bill_depth_mm + 8.111 * 0 + 2245.878 * 1
# Peso_Gentoo = (-1000.846 + 2245.878) + 256.551 * bill_depth_mm 

# El modelo con el nivel de base de especie cambiado
penguins_2 <- penguins %>% drop_na()
penguins_2$species <- factor(penguins_2$species, levels = c("Gentoo", "Adelie", "Chinstrap"))
model_penguins_2 <- lm(body_mass_g ~ bill_depth_mm + species, penguins_2)
model_penguins_2

# Las estimaciones
Peso_Chinstrap_18 <- (-1000.846 + 8.111) + 256.551 * 18 
Peso_Chinstrap_18

Peso_Gentoo_16 <- (-1000.846 + 2245.878) + 256.551 * 16 
Peso_Gentoo_16

# Estadística de los parámetros y variables
summary(model_penguins)
Anova(model_penguins, type=3)

# El modelo con interacción
# model_penguins_interaccion <- lm(body_mass_g ~ bill_depth_mm + species + bill_depth_mm:species, 
#                                  penguins %>% drop_na())
model_penguins_interaccion <- lm(body_mass_g ~ bill_depth_mm * species, 
                                 penguins %>% drop_na())
model_penguins_interaccion

# Peso = -297.38 + 218.21 * bill_depth_mm + 261.16 * speciesChinstrap - 124.43 * speciesGentoo - 13.58 * bill_depth_mm X speciesChinstrap + 149.49 *bill_depth_mm X speciesGentoo
# Especie Adelie
# Peso_Adelie = -297.38 + 218.21 * bill_depth_mm + 261.16 * 0 - 124.43 * 0 - 13.58 * bill_depth_mm X 0 + 149.49 *bill_depth_mm X 0
# Peso_Adelie = -297.38 + 218.21 * bill_depth_mm
# Especie Chinstrap
# Peso_Chinstrap = -297.38 + 218.21 * bill_depth_mm + 261.16 * 1 - 124.43 * 0 - 13.58 * bill_depth_mm X 1 + 149.49 *bill_depth_mm X 0
# Peso_Chinstrap = -297.38 + 218.21 * bill_depth_mm + 261.16 * 1 - 13.58 * bill_depth_mm 
# Peso_Chinstrap = -297.38 + 218.21 * bill_depth_mm + 261.16 * 1 - 13.58 * bill_depth_mm 
# Peso_Chinstrap = (-297.38 + 261.16) + (218.21 - 13.58) * bill_depth_mm 
# Especie Gentoo
# Peso_Gentoo = -297.38 + 218.21 * bill_depth_mm + 261.16 * 0 - 124.43 * 1 - 13.58 * bill_depth_mm X 0 + 149.49 *bill_depth_mm X 1
# Peso_Gentoo = (-297.38 - 124.43) + (218.21 + 149.49) * bill_depth_mm 

# Comparando los modelos
anova(model_penguins_interaccion, model_penguins)

# Las nuevas estimaciones
Peso_Chinstrap_18 <- (-297.38 + 261.16) + (218.21 - 13.58) * 18 
Peso_Chinstrap_18

Peso_Gentoo_16 <- (-297.38 - 124.43) + (218.21 + 149.49) * 16
Peso_Gentoo_16

# Ejercicio 3 ------------------------------------------------------------------
set.seed(4)
# Datos creados sumando ruido
data_dientes <- tibble(Dientes = round(rnorm(2000) * 10+50),
                       Peso = (500 +  1 * Dientes) + rnorm(2000) * 30)

# Los ploteo
data_dientes %>% ggplot(aes(x = Dientes,
           y = Peso)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)
  
# Modelo de los 2000 datos
model_dientes <- lm(Peso ~ Dientes, data_dientes)
summary(model_dientes)

model_parameters(model_dientes)

# Simulación de 200 "experimentos" tomando de a 20
set.seed(4)
CI_in <- tibble(index = 1:200,
                low = 0,
                high = 0,
                isin = FALSE)

for (i in 1:200) {
  data_sample <- data_dientes %>% sample_n(size = 20)  
  model_sample <- lm(Peso ~ Dientes, data_sample)
  CI_in$low[i] <- model_parameters(model_sample)$CI_low[2]
  CI_in$high[i] <- model_parameters(model_sample)$CI_high[2]
  CI_in$isin[i] <- between(1, model_parameters(model_sample)$CI_low[2], model_parameters(model_sample)$CI_high[2])
  print(paste0(i, " low:", CI_in$low[i], " high:", CI_in$high[i], "IN:", CI_in$isin[i]))
}
# Porcentaje de "experimentos" en los que el CI contiene a 1
sum(CI_in$isin)/200 * 100

# Simulación de 200 "experimentos" tomando de a 50
set.seed(4)
CI_in_50 <- tibble(index = 1:200,
                   low = 0,
                   high = 0,
                   isin = FALSE)

for (i in 1:200) {
  data_sample <- data_dientes %>% sample_n(size = 50)  
  model_sample <- lm(Peso ~ Dientes, data_sample)
  CI_in_50$low[i] <- model_parameters(model_sample)$CI_low[2]
  CI_in_50$high[i] <- model_parameters(model_sample)$CI_high[2]
  CI_in_50$isin[i] <- between(1, model_parameters(model_sample)$CI_low[2], model_parameters(model_sample)$CI_high[2])
  print(paste0(i, " low:", CI_in$low[i], " high:", CI_in$high[i], "IN:", CI_in$isin[i]))
}
# Porcentaje de "experimentos" en los que el CI contiene a 1
sum(CI_in_50$isin)/200 * 100

# Ploteo el ancho de los CIs (mayor n conlleva a intervalos más angostos)
ggplot() +
  geom_ribbon(data = CI_in, aes(x = index, ymin = low, ymax = high), fill = "#1380A1", alpha = 0.6) + 
  geom_ribbon(data = CI_in_50, aes(x = index, ymin = low, ymax = high), fill = "#ED6A5A", alpha = 0.6) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed")

# Ejercicio 4 ------------------------------------------------------------------
#install.packages("MASS")
library(MASS)

# El modelos
model_cats <- lm(Hwt ~ Bwt * Sex, cats %>% drop_na())
model_cats
summary(model_cats)

# Ploteo los datos
cats %>% drop_na() %>% ggplot(aes(x = Bwt,
           y = Hwt,
           color = Sex)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# La fórmula y la estimación
# Peso_Corazon = 2.981 + 2.636 Peso_Total - 4.165 SexM + 1.676 * PesoTotal X SexM
Peso_Corazon_M <- 2.981 + 2.636 * 3 - 4.165 * 1 + 1.676 * 3 * 1
Peso_Corazon_H = 2.981 + 2.636 * 2 - 4.165 * 0 + 1.676 * 2 * 0

# El efecto de la interacción sobre las pendientes
# Peso_Corazon_M = 2.981 + 2.636 Peso_Total - 4.165 1 + 1.676 * PesoTotal X 1
# Peso_Corazon_M = (2.981 - 4.165)  + (2.636 + 1.676) Peso_Total
# Peso_Corazon_H = 2.981 + 2.636 Peso_Total - 4.165 0 + 1.676 * PesoTotal X 0
# Peso_Corazon_H = 2.981 + 2.636 Peso_Total

# Ejercicio 5 ------------------------------------------------------------------
# Los datos
head(births14)

# El modelo
modelo_births <- lm(weight ~ weeks + mage + sex + habit + visits, births14)
modelo_births
# Peso = -3.81806 + 0.26451 * weeks + 0.01552 * mage + 0.36981 * sexmale - 0.42928 * habitsmoker + 0.01822 * visits

# Effect size estandarizado
#install.packages(effectsize)
library(effectsize)
effectsize(modelo_births)

# Residuo de la primera fila
residuo <- abs(predict.lm(modelo_births, births14[1,])- births14$weight[1])

# Predicciones
predict.lm(modelo_births, births14)
modelo_births$fitted.values

tibble_muestra <- tibble(sex = "female", weeks = 39, mage = 38, habit = "smoker", visits = 12)
predict.lm(modelo_births, tibble_muestra) * 0.453592
