
rm(list=ls())
setwd("~/Desktop/2nd Year PhD/Machine learning/ML-HW2/Machine_Learning_HW2")

library(tidyverse)
library(ggplot2)
library(tidymodels)
library(yardstick)


abalone <- read_csv("abalone.csv")

#1
abalone <- abalone %>%
  mutate(age = rings + 1.5)

#2
set.seed(1)

    #splitting the data
abalone_split <- initial_split(abalone, prop = 0.80, strata = age)
abalone_train <- training(abalone_split)
abalone_test  <- testing(abalone_split)

#3
abalone_recipe <- recipe(age ~ ., data = abalone_train) %>%
  step_rm(rings) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_interact(terms = ~ type_M:shucked_weight) %>%
  step_interact(terms = ~ longest_shell:diameter) %>%
  step_interact(terms = ~ shucked_weight:shell_weight) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

#4  
lm_model <- linear_reg() %>%
  set_engine("lm")

#5
lm_workflow <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(abalone_recipe)

#6 Still something to check ---------------------------
lm_fit <- fit(lm_workflow, abalone_train)

lm_fit %>% 
  extract_fit_parsnip() %>%
  tidy()

abalone_train_res <- predict(lm_fit, new_data = data.frame(
  type = "I" , longest_shell = 0.5, diameter = 0.1, 
  height = 0.3, whole_weight = 4, shucked_weight = 1,
  viscera_weight = 2, shell_weight = 1 , rings = 1))

abalone_train_res

#7

abalone_train_res2 <- predict(lm_fit, new_data = abalone_train %>% select(-age)) 

abalone_train_res2 <- bind_cols(abalone_train_res2, abalone_train %>% select(age))

abalone_metrics <- metric_set(rmse,rsq, mae)

abalone_metrics(abalone_train_res2, truth = age, estimate = .pred)

####Interpret the result

#Graphing

abalone_train_res2 %>%
  ggplot(aes(x=.pred, y = age)) +
  geom_point(alpha = 0.2) +
  geom_abline(lty=2) +
  theme_bw() +
  coord_obs_pred() 
  
  
  
  
  
  
  
  

