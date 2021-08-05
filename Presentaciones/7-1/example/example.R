library(tidyverse)
library(magrittr)
library(skimr)
library(corrplot)
library(livecode)
library(GGally)
library(tidymodels)

server <- livecode::serve_file()
server$stop()

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

select(pm, contains("imp")) %>%
  ggcorr(palette = "RdBu", label = TRUE)

select(pm, contains("imp")) %>%
  ggpairs()         

select(pm, contains("pri")) %>%
  ggcorr(palette = "RdBu", hjust = .85, size = 3,
         layout.exp=2, label = TRUE)

select(pm, contains("nei")) %>%
  ggcorr(palette = "RdBu", hjust = .85, size = 3,
         layout.exp=2, label = TRUE)

select(pm, contains("nei")) %>%
  ggpairs()

pm %>%
  select(log_nei_2008_pm25_sum_10000, popdens_county, 
         log_pri_length_10000, imp_a10000) %>%
  ggcorr(palette = "RdBu",  hjust = .85, size = 3,
         layout.exp=2, label = TRUE)

pm %>%
  select(log_nei_2008_pm25_sum_10000, popdens_county, 
         log_pri_length_10000, imp_a10000, county_pop) %>%
  ggpairs()

pm %>%
  mutate(log_popdens_county= log(popdens_county)) %>%
  select(log_nei_2008_pm25_sum_10000, log_popdens_county, 
         log_pri_length_10000, imp_a10000) %>%
  ggcorr(palette = "RdBu",  hjust = .85, size = 3,
         layout.exp=2, label = TRUE)


save(pm, file = "./data/wrangled_pm.rda")
write_csv(pm, file = "data/wrangled_pm.csv")

# Machine Learning ####
library(tidymodels)

load("./data/wrangled_pm.rda")

# Split
set.seed(1234)
pm_split <- rsample::initial_split(data = pm, prop = 2/3)
pm_split

count(pm, state)

train_pm <-rsample::training(pm_split)
test_pm <-rsample::testing(pm_split)

count(train_pm, state)

# Feature engineering

simple_rec <- train_pm %>%
  recipes::recipe(value ~ .)

simple_rec

simple_rec <- train_pm %>%
  recipes::recipe(value ~ .) %>%
  recipes::update_role(id, new_role = "id variable")

simple_rec

summary(simple_rec)

simple_rec %<>%
  update_role("fips", new_role = "county id") %>%
  step_dummy(state, county, city, zcta, one_hot = TRUE) %>%
  step_corr(all_predictors(), - CMAQ, - aod)%>%
  step_nzv(all_predictors(), - CMAQ, - aod)

simple_rec

prepped_rec <- prep(simple_rec, verbose = TRUE, retain = TRUE )

baked_train <- bake(prepped_rec, new_data = NULL)
glimpse(baked_train)

baked_test_pm <- recipes::bake(prepped_rec, new_data = test_pm)
glimpse(baked_test_pm)

traincities <- train_pm %>% distinct(city)
testcities <- test_pm %>% distinct(city)

#get the number of cities that were different
dim(dplyr::setdiff(traincities, testcities))
dim(dplyr::intersect(traincities, testcities))

# El pipe de prepro final 
pm %<>%
  mutate(city = case_when(city == "Not in a city" ~ "Not in a city",
                          city != "Not in a city" ~ "In a city"))

set.seed(1234) # same seed as before
pm_split <-rsample::initial_split(data = pm, prop = 2/3)
pm_split

train_pm <-rsample::training(pm_split)
test_pm <-rsample::testing(pm_split)


novel_rec <-recipe(train_pm) %>% # Nueva recipe
  update_role(everything(), new_role = "predictor") %>%
  update_role(value, new_role = "outcome") %>%
  update_role(id, new_role = "id variable") %>%
  update_role("fips", new_role = "county id") %>%
  step_dummy(state, county, city, zcta, one_hot = TRUE) %>%
  step_corr(all_numeric()) %>%
  step_nzv(all_numeric()) 

prepped_rec <- prep(novel_rec, verbose = TRUE, retain = TRUE)

baked_train <- bake(prepped_rec, new_data = NULL)
glimpse(baked_train)

baked_test_pm <- recipes::bake(prepped_rec, new_data = test_pm)
glimpse(baked_test_pm)

# Modelo ####
PM_model <- parsnip::linear_reg() # PM was used in the name for particulate matter
PM_model

lm_PM_model <- 
  PM_model  %>%
  parsnip::set_engine("lm")
lm_PM_model

lm_PM_model <- 
  PM_model  %>%
  parsnip::set_engine("lm") %>%
  set_mode("regression")

lm_PM_model

PM_wflow <-workflows::workflow() %>%
  workflows::add_recipe(novel_rec) %>%
  workflows::add_model(lm_PM_model)
PM_wflow

PM_wflow_fit <- parsnip::fit(PM_wflow, data = train_pm)
PM_wflow_fit

wflowoutput <- PM_wflow_fit %>% 
  extract_fit_parsnip() %>% 
  broom::tidy() 
wflowoutput

PM_wflow_fit %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 10)

# Performance
wf_fit <- PM_wflow_fit %>% 
  extract_fit_parsnip()

wf_fitted_values <- fitted(wf_fit[["fit"]])
head(wf_fitted_values)

wf_fitted_values <- 
  broom::augment(wf_fit[["fit"]], data = baked_train) %>% 
  select(value, .fitted:.std.resid)

head(wf_fitted_values)

values_pred_train <- 
  predict(PM_wflow_fit, train_pm) %>% 
  bind_cols(train_pm %>% select(value, fips, county, id)) 

values_pred_train

wf_fitted_values %>% 
  ggplot(aes(x =  value, y = .fitted)) + 
  geom_point() + 
  xlab("actual outcome values") + 
  ylab("predicted outcome values")

yardstick::metrics(wf_fitted_values, 
                   truth = value, estimate = .fitted)

# k-fold
set.seed(1234)
vfold_pm <- rsample::vfold_cv(data = train_pm, v = 4)
vfold_pm

pull(vfold_pm, splits)
resample_fit <- tune::fit_resamples(PM_wflow, vfold_pm)

tune::show_best(resample_fit, metric = "rmse")

# Random Forest
RF_rec <- recipe(train_pm) %>%
  update_role(everything(), new_role = "predictor")%>%
  update_role(value, new_role = "outcome")%>%
  update_role(id, new_role = "id variable") %>%
  update_role("fips", new_role = "county id") %>%
  step_novel("state") %>%
  step_string2factor("state", "county", "city") %>%
  step_rm("county") %>%
  step_rm("zcta") %>%
  step_corr(all_numeric())%>%
  step_nzv(all_numeric())

PMtree_model <- 
  parsnip::rand_forest(mtry = 10, min_n = 3)
PMtree_model

RF_PM_model <- 
  PMtree_model %>%
  set_engine("randomForest") %>%
  set_mode("regression")
RF_PM_model

RF_wflow <- workflows::workflow() %>%
  workflows::add_recipe(RF_rec) %>%
  workflows::add_model(RF_PM_model)
RF_wflow

RF_wflow_fit <- parsnip::fit(RF_wflow, data = train_pm)
RF_wflow_fit

RF_wflow_fit %>% 
  pull_workflow_fit() %>% 
  vip(num_features = 10)

set.seed(456)
resample_RF_fit <- tune::fit_resamples(RF_wflow, vfold_pm)
collect_metrics(resample_RF_fit)

collect_metrics(resample_fit)

# Model Tuning
tune_RF_model <- rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine("randomForest") %>%
  set_mode("regression")

tune_RF_model

RF_tune_wflow <- workflows::workflow() %>%
  workflows::add_recipe(RF_rec) %>%
  workflows::add_model(tune_RF_model)
RF_tune_wflow

doParallel::registerDoParallel(cores=2)
set.seed(123)
tune_RF_results <- tune_grid(object = RF_tune_wflow, resamples = vfold_pm, grid = 20)
tune_RF_results

tune_RF_results%>%
  collect_metrics()

show_best(tune_RF_results, metric = "rmse", n =1)

# Model Performance
tuned_RF_values<- select_best(tune_RF_results, "rmse")
tuned_RF_values

RF_tuned_wflow <-RF_tune_wflow %>%
  tune::finalize_workflow(tuned_RF_values)

overallfit <-RF_wflow %>%
  tune::last_fit(pm_split)

collect_metrics(overallfit)

test_predictions <-collect_predictions(overallfit)
test_predictions
