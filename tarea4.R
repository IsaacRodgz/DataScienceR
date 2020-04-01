library(ISLR)
library(kernlab)
library(tidyverse)
library(reshape2)

# Carga datos
car.data <- as_tibble(Auto)

str(car.data)

# Se revisan los rangos de los datos
summary(car.data)

# Pairs plot
car.data %>% select(-c(cylinders, origin, name)) %>% pairs(lower.panel = NULL, main="Vehicles information")

# Se estandarizan las variables no categóricas debido a la variabilidad en los rangos
scale2 <- function(x, na.rm = FALSE) (x - min(x, na.rm = na.rm)) / (max(x, na.rm = na.rm)-min(x, na.rm = na.rm))
car.data <- car.data %>% mutate_at(c("mpg", "displacement", "horsepower", "weight", "acceleration"), scale2)

# Se cambia el valor de la variable origen de [1, 2, 3] a ["USA", "Europe", "Japan"] para que sea más entendible dicha columna
car.data$origin <- as.factor(car.data$origin)
car.data <- car.data %>%
  mutate(origin = recode_factor(origin, "1" = "USA", "2" = "Europe", "3" = "Japan"))

# Pairs plot by origin
cols <- character(nrow(car.data))
cols[car.data$origin == "USA"] <- "red"
cols[car.data$origin == "Europe"] <- "blue"
cols[car.data$origin == "Japan"] <- "green"
car.data %>% select(-cylinders, -origin, -name) %>% pairs(lower.panel = NULL, main="Vehicles information", col=cols)

# Correlation betweeen variables
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
cormat <- round(cor(car.data[,-c(2,8,9)]),2)
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm=TRUE)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Histograma variable origen
ggplot(car.data, aes(x=origin)) + geom_bar(fill="steelblue") + theme_minimal() + scale_x_discrete(limits=c("Europe", "Japan", "USA"))

# Se quita variable mpg y se transforma a -1 si es menor o igual a 23 y 1 en caso contrario
# Además se pone el resto de atributos en variable X

y <- car.data %>% mutate(mpg = ifelse(mpg <= 23, -1, ifelse(mpg > 23, 1, 0))) %>% select(mpg)
y <- as.vector(y)
X <-  car.data %>% select(-cylinders, -origin, -name, -mpg)
X <- as.matrix(X)
d <- cbind(X,y)

# SVM

s<-ksvm(x=X, y=y, data=d, kernel="polydot", cost=1, kpar=list(degree=3,offset=0))
s<-ksvm(x=X, y=y, data=d, kernel="rbfdot", C=10, kpar=list(sigma = 5))
s
