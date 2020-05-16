library(tidymodels)
library(tidyverse)
library(magrittr)
library(rpart.plot)

set.seed(101)

# Reading and Preprocessing

#setwd("~/Documents/CIMAT/DataScienceR/tarea6/")
wine.data.train = read_tsv("wine.train.txt")
wine.data.test = read_tsv("wine.test.txt")

# Set label 1 when wine is of type Grignolio and zero otherwise
wine.data.train <- wine.data.train %>% mutate(classdigit = case_when(classdigit == 1 | classdigit == 3 ~ 0,
                                                  classdigit == 2 ~ 1,))
wine.data.test <-wine.data.test %>% mutate(classdigit = case_when(classdigit == 1 | classdigit == 3 ~ 0,
                                                  classdigit == 2 ~ 1,))

wine.data.train %<>% mutate(classdigit = as.factor(classdigit))
wine.data.test %<>% mutate(classdigit = as.factor(classdigit))
wine.data.test = select (wine.data.test, -c(class))


wine.recipe = recipe(classdigit ~ ., data = wine.data.train) %>%
  #step_center(all_predictors(), -all_outcomes()) %>%
  #step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

wine.train = juice(wine.recipe)
wine.test = wine.recipe %>%
  bake(wine.data.test)

# ----------------------------
# Define Model - Decision Tree
# ----------------------------

wine.dt = decision_tree(mode = "classification",
                        min_n = tune(),
                        cost_complexity = tune()) %>%
  set_engine("rpart")

folds <- vfold_cv(wine.train, repeats = 2)

tree.wflow =
  workflow() %>%
  add_model(wine.dt) %>%
  add_recipe(wine.recipe)

tree.set <- parameters(tree.wflow)

search_res =
  tune_bayes(
    tree.wflow, 
    resamples = folds,
    param_info = tree.set,
    initial = 5,
    iter = 30,
    metrics = metric_set(accuracy),
    control = control_bayes(no_improve = 10, verbose = TRUE)
  )

estimates = collect_metrics(search_res) %>% arrange(.iter)

show_best(search_res, metric = "accuracy")
autoplot(search_res, type = "performance")

# Model fitting after hyperparameter tuning

optim.params <- show_best(search_res, metric = "accuracy")[1,]

final_rec <- 
  wine.recipe %>%
  finalize_recipe(optim.params) %>%
  prep()

model.tuned <-
  wine.dt %>%
  finalize_model(optim.params) %>%
  fit(classdigit ~ ., data = wine.train)

# Evaluation metrics

model.tuned %>%
  predict(wine.test) %>%
  bind_cols(wine.test) %>%
  metrics(truth = classdigit, estimate = .pred_class)

# Analyze results

summary(wine.dt.model$fit)
rpart.plot(wine.dt.model$fit, roundint=FALSE)

# ----------------------------------
# Define Model - Logistic Regression
# ----------------------------------

wine.lr.model = logistic_reg(mode = "classification") %>%
  set_engine("glm") %>% 
  fit(classdigit ~ ., data = wine.train)

# Evaluation metrics

wine.lr.model %>%
  predict(wine.test) %>%
  bind_cols(wine.test) %>%
  metrics(truth = classdigit, estimate = .pred_class)

# Analyze results

wine.lr.model

# -------------------------------------
# Define Model - Multi Layer Perceptron
# -------------------------------------

wine.mlp = mlp(mode = "classification",
               activation = "relu",
               hidden_units = tune(),
               dropout = 0.4,
               epochs = 10) %>% 
  set_engine("nnet")

folds <- vfold_cv(wine.train, repeats = 2)

mlp.wflow =
  workflow() %>%
  add_model(wine.mlp) %>%
  add_recipe(wine.recipe)

mlp.set <- parameters(mlp.wflow)

search_res =
  tune_bayes(
    mlp.wflow, 
    resamples = folds,
    param_info = mlp.set,
    initial = 5,
    iter = 20,
    metrics = metric_set(accuracy),
    control = control_bayes(no_improve = 10, verbose = TRUE)
  )

estimates = collect_metrics(search_res) %>% arrange(.iter)

show_best(search_res, metric = "accuracy")
autoplot(search_res, type = "performance")

# Model fitting after hyperparameter tuning

optim.params <- show_best(search_res, metric = "accuracy")[1,]

final_rec <- 
  wine.recipe %>%
  finalize_recipe(optim.params) %>%
  prep()

model.tuned <-
  wine.mlp %>%
  finalize_model(optim.params) %>%
  fit(classdigit ~ ., data = wine.train)

# Evaluation metrics

model.tuned %>%
  predict(wine.test) %>%
  bind_cols(wine.test) %>%
  metrics(truth = classdigit, estimate = .pred_class)


# ----------------------------------

wine.lr.model = mlp(mode = "classification",
                    activation = "relu",
                    hidden_units = 2,
                    dropout = 0,
                    epochs = 10) %>% 
  set_engine("keras") %>% 
  fit(classdigit ~ ., data = wine.train)

# Evaluation metrics

wine.lr.model %>%
  predict(wine.test) %>%
  bind_cols(wine.test) %>%
  metrics(truth = classdigit, estimate = .pred_class)
