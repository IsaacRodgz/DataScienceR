library(tidymodels)
library(tidyverse)
library(magrittr)
library(rpart.plot)
library(ggcorrplot)
library(gridExtra)
library(ica)
library(fastICA)
library(rsample)
library(caret)
library(discrim)
library(klaR)

set.seed(101)

# Análisis datos 1: Vino.

# Reading and Preprocessing

setwd("~/Documents/CIMAT/DataScienceR/tarea6/")
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

wine.recipe = recipe(classdigit ~ ., data = wine.data.train) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

wine.train = juice(wine.recipe)
wine.test = wine.recipe %>%
  bake(wine.data.test)


wine.mlp = mlp(mode = "classification",
               activation = "relu",
               hidden_units = tune(),
               dropout = 0.0,
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
                    hidden_units = 1,
                    dropout = 0.,
                    epochs = 10) %>% 
  set_engine("nnet") %>% 
  fit(classdigit ~ ., data = wine.train)

# Evaluation metrics

wine.lr.model %>%
  predict(wine.test) %>%
  bind_cols(wine.test) %>%
  metrics(truth = classdigit, estimate = .pred_class)

# ----------------------
# Análisis datos 2: Spam.
# ----------------------

spam.data = read_csv("spambase.data", col_names = FALSE)
spam.data <- spam.data %>% rename(class = X58)
spam.data %<>% mutate(class = as.factor(class))
log_m <- function(x) log(x+0.001)
spam.data %<>% mutate_if(is.numeric, log1p)

# EDA

# Correlation plot 

# Visualize correlation plot

corr=round(cor(spam.data[-58]), 1)

ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlation of wine variables")

# Distribution

p1 <- ggplot(spam.data, aes(x=X1)) +
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.1,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + labs(x = "word_freq_make")

p2 <- ggplot(spam.data, aes(x=X10)) +
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.2,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + labs(x = "word_freq_mail")

p3 <- ggplot(spam.data, aes(x=X20)) +
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.2,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + labs(x = "word_freq_credit")

p4 <- ggplot(spam.data, aes(x=X55)) +
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=0.3,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + labs(x = "capital_run_length_average")

grid.arrange(p1, p2, p3, p4, nrow = 2)

# PCA

pr = prcomp(spam.data[-58])
plot(pr, type = "l")
plot(cumsum(pr$sdev[1:5]^2)/sum(pr$sdev^2), xlab = "Num of PC", ylab = "Explained variance", type = "b")
summary(pr)

# Data PCA
spam.pca <- as.data.frame(predict(pr, newdata = spam.data))
spam.pca <- cbind(spam.pca[,c(1,2)], spam.data$class)
names(spam.pca)[3] <- "class"

spam.data_s <- scale(spam.data[,-58])
pr = princomp(spam.data_s)
pairs(pr$scores[,1:3], col=rainbow(2)[pull(spam.data, class)], asp=1)

# ---------
# Modelo LR
# ---------

# Train - Test split
spam.split <- createDataPartition(spam.data$class, p = .8, 
                                                list = FALSE, 
                                                times = 1)
spam.train <- spam.data[as.vector(spam.split),]
spam.test <- spam.data[-as.vector(spam.split),]

spam.recipe = recipe(class ~ ., data = spam.train) %>%
  #step_log(all_predictors(), offset = 1) %>%
  prep()

spam.train.r = bake(spam.recipe, spam.train)
spam.test.r = bake(spam.recipe, spam.test)

spam.lr.model = logistic_reg(mode = "classification") %>%
  set_engine("glm") %>% 
  fit(class ~ ., data = spam.train.r)

# Evaluation metrics

spam.lr.model %>%
  predict(spam.test.r) %>%
  bind_cols(spam.test.r) %>%
  metrics(truth = class, estimate = .pred_class)

# Analyze results

with(spam.lr.model$fit, null.deviance - deviance)
with(spam.lr.model$fit, df.null - df.residual)
with(spam.lr.model$fit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# ---------
# Modelo Naive Bayes
# ---------

# Train - Test split
spam.split <- createDataPartition(spam.data$class, p = .8, 
                                  list = FALSE, 
                                  times = 1)
spam.train <- spam.data[as.vector(spam.split),]
spam.test <- spam.data[-as.vector(spam.split),]

spam.recipe = recipe(class ~ ., data = spam.train) %>%
  step_factor2string(all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_log(all_predictors(), offset = 1) %>%
  prep()

spam.train.r = bake(spam.recipe, spam.train)
spam.test.r = bake(spam.recipe, spam.test)

spam.nb.model <- naive_Bayes(mode = "classification") %>%
  set_engine("klaR") %>%
  fit(class ~ ., data = spam.train.r)
  #fit_xy(x = spam.train.r[,-58], y = spam.train.r[,58])

#naive_Bayes(smoothness = .8) %>%
#  set_engine("klaR") %>%
#  translate()

# Evaluation metrics

spam.nb.model %>%
  predict(spam.test.r) %>%
  bind_cols(spam.test.r) %>%
  metrics(truth = class, estimate = .pred_class)
