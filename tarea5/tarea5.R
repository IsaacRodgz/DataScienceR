library(tidyverse)
library(magrittr)
library(ggplot2)
library(GGally)
library(ggcorrplot)
library(gridExtra)
library(cluster)
library(tree)
library(tidymodels)
library(rpart.plot)
library(C50)

set.seed(101)

# Read data

setwd("~/Documents/CIMAT/DataScienceR/tarea5/")

wine.data.train = read_tsv("wine.train.txt")

wine.data.train %<>% mutate(classdigit = as.factor(classdigit))

# First look at data

head(wine.data.train)
map(wine.data.train, ~sum(is.na(.)))

# Visualize pairsplot

fill.color='#154360'  #"#1579D2"
fill.color2="#D35400" 
vline.color="#D0DBE5"
axis.line.color="#e5e7e9"
lab.color="#D68910"

all.theme=theme_bw()+
  theme(
    text = element_text(family="Courier"),#,size=16
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = axis.line.color),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(family='Courier',vjust=0.6,hjust =0.2 ), 
    axis.text.y = element_text(family = 'Courier',vjust = 0.6,hjust = 0.2),
    plot.subtitle = element_text(hjust=0.5)
  )

ggpairs(wine.data.train, columns=1:13, axisLabels="show", title='Red Wine Quality Data')+all.theme

# Visualize correlation plot

corr=round(cor(wine.data.train[-14]), 1)

ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlation of wine variables")

# Variables distribution

# Histogram overlaid with kernel density curve
p1 <- ggplot(wine.data.train, aes(x=Alcohol)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

p2 <- ggplot(wine.data.train, aes(x=MalicAcid)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

p3 <- ggplot(wine.data.train, aes(x=Ash)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.2,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

p4 <- ggplot(wine.data.train, aes(x=AlcAsh)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=2,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

p5 <- ggplot(wine.data.train, aes(x=Mg)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=7,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

p6 <- ggplot(wine.data.train, aes(x=Phenols)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.3,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

p7 <- ggplot(wine.data.train, aes(x=Flav)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.4,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

p8 <- ggplot(wine.data.train, aes(x=NonFlavPhenols)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.07,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

p9 <- ggplot(wine.data.train, aes(x=Proa)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.3,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

p10 <- ggplot(wine.data.train, aes(x=Color)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

p11 <- ggplot(wine.data.train, aes(x=Hue)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.1,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

p12 <- ggplot(wine.data.train, aes(x=OD)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.3,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

p13 <- ggplot(wine.data.train, aes(x=Proline)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=130,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, nrow = 5)

# Visualize boxplots of  attributes vs target class

ggplot(data = wine.data.train, mapping = aes(x = classdigit, y = Alcohol)) +
  geom_boxplot() + xlab("cultivar")

ggplot(data = wine.data.train, mapping = aes(x = classdigit, y = Phenols)) +
  geom_boxplot() + xlab("cultivar")

ggplot(data = wine.data.train, mapping = aes(x = classdigit, y = Flav)) +
  geom_boxplot() + xlab("cultivar")

ggplot(data = wine.data.train, mapping = aes(x = classdigit, y = OD)) +
  geom_boxplot() + xlab("cultivar")

ggplot(data = wine.data.train, mapping = aes(x = classdigit, y = Proline)) +
  geom_boxplot() + xlab("cultivar")

ggplot(data = wine.data.train, mapping = aes(x = classdigit, y = Color)) +
  geom_boxplot() + xlab("cultivar")

# Clustering

wine.data.train.sd <- scale(wine.data.train[-14])

k.means.fit <- kmeans(wine.data.train.sd, centers = 3)

summary(k.means.fit)

k.means.fit$cluster

clusplot(wine.data.train.sd, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=FALSE,
         labels=2, lines=0)

# PCA

pr = prcomp(wine.data.train[-14])
plot(x = wine.data.train$classdigit, y = predict(pr)[,1], xlab = "Score", ylab = "Proyección PC1")
plot(pr, type = "l")
plot(cumsum(pr$sdev)/sum(pr$sdev), xlab = "Num of PC", ylab = "Explained variance")
summary(pr)

# Reading and Preprocessing

setwd("~/Documents/CIMAT/DataScienceR/tarea5/")
wine.data.train = read_tsv("wine.train.txt")
wine.data.test = read_tsv("wine.test.txt")

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

# Define Model

wine.dt = decision_tree(mode = "classification",
                        min_n = tune(),
                        cost_complexity = tune()) %>%
  set_engine("rpart")

#cultivar.tree = tree(classdigit~.-classdigit, data=wine.data.train)
#summary(cultivar.tree)
#plot(cultivar.tree)
#text(cultivar.tree, pretty = 0)

# Hyperparameter tuning

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
    metrics = metric_set(roc_auc, accuracy),
    control = control_bayes(no_improve = 20, verbose = TRUE)
  )

estimates = collect_metrics(search_res) %>% arrange(.iter)

show_best(search_res, metric = "accuracy")
show_best(search_res, metric = "roc_auc")
autoplot(search_res, type = "performance")

# Model fitting after hyperparameter tuning

wine.recipe = recipe(classdigit ~ ., data = wine.data.train) %>%
  #step_center(all_predictors(), -all_outcomes()) %>%
  #step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

wine.train = juice(wine.recipe)
wine.test = wine.recipe %>%
  bake(wine.data.test)

wine.dt.model = decision_tree(mode = "classification",
                        min_n = 8,
                        cost_complexity = 0.0000000937) %>% 
  set_engine("rpart") %>% 
  fit(classdigit ~ ., data = wine.train)

# Evaluation metrics

wine.dt.model %>%
  predict(wine.test) %>%
  bind_cols(wine.test) %>%
  metrics(truth = classdigit, estimate = .pred_class)

# Analyze results

summary(wine.dt.model$fit)
rpart.plot(wine.dt.model$fit, roundint=FALSE)


# Parte 1 Tarea

# b) Generar muestras (X, Y) de tamaño 50, 100, 500

n = 100
A = matrix(c(1, 0.4, 0, 0.9165), nrow = 2, ncol = 2)
y = rbinom(n, 1, 0.5)
X = matrix(nrow = 2)

for (e in y) {
  if (e == 0) {
    X = cbind(X, c(rnorm(1, mean = 0, sd = 1), rnorm(1, mean = 0, sd = 1)))
  }
  
  else {
    X = cbind(X, c(rnorm(1, mean = 1, sd = 1), rnorm(1, mean = 2, sd = 1)))
  }
}

X = X[,-1]

#z = rbind(rnorm(n, mean = 0, sd = 1), rnorm(n, mean = 0, sd = 1))
X = t(A %*% X)
plot(X)

# c) Árboles de decisión

colnames(X) = c("x1", "x2")
data = as_tibble(cbind(X, y))
data %<>% mutate(y = as.factor(y))

normal.recipe = recipe(y ~ ., data = data)

normal.dt = decision_tree(mode = "classification",
                        min_n = tune(),
                        cost_complexity = tune()) %>%
  set_engine("rpart")

folds <- vfold_cv(data, repeats = 2)

tree.wflow =
  workflow() %>%
  add_model(normal.dt) %>%
  add_recipe(normal.recipe)

tree.set <- parameters(tree.wflow)

search_res =
  tune_bayes(
    tree.wflow, 
    resamples = folds,
    param_info = tree.set,
    initial = 5,
    iter = 30,
    metrics = metric_set(accuracy),
    control = control_bayes(no_improve = 20, verbose = TRUE)
  )

show_best(search_res, metric = "accuracy")

# Build optimal model

tree.model = decision_tree(mode = "classification",
                              min_n = 30,
                              cost_complexity = 0) %>% 
  set_engine("rpart") %>% 
  fit(y ~ ., data = data)

# Evaluation metrics

tree.model %>%
  predict(data) %>%
  bind_cols(data) %>%
  metrics(truth = y, estimate = .pred_class)

# Analyze results

summary(tree.model$fit)
rpart.plot(tree.model$fit, roundint=FALSE)

# d) SVM

normal.svm = svm_rbf(mode = "classification",
                     cost = tune(),
                     rbf_sigma = tune()) %>%
  set_engine("kernlab")

folds <- vfold_cv(data, repeats = 2)

svm.wflow =
  workflow() %>%
  add_model(normal.svm) %>%
  add_recipe(normal.recipe)

svm.set <- parameters(svm.wflow)

search_res =
  tune_bayes(
    svm.wflow, 
    resamples = folds,
    param_info = svm.set,
    initial = 5,
    iter = 30,
    metrics = metric_set(accuracy),
    control = control_bayes(no_improve = 20, verbose = TRUE)
  )

show_best(search_res, metric = "accuracy")

# Build optimal model

svm.model = svm_rbf(mode = "classification",
                     cost = 0.472,
                     rbf_sigma = 0.0935) %>% 
  set_engine("kernlab") %>% 
  fit(y ~ ., data = data)

# Evaluation metrics

svm.model %>%
  predict(data) %>%
  bind_cols(data) %>%
  metrics(truth = y, estimate = .pred_class)
