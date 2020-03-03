# Apéndice. Código R.

# Se cargan bibliotecas y datos

library(Rtsne)
library(ggplot2)
library(ggimage)

setwd("~/Documents/CIMAT/DataScienceR/")
train.data <- read.csv("fashion-mnist_train.csv")
train.data.shirt <- train.data[train.data$label == 6,][-1]

rotate <- function(x) t(apply(x, 2, rev))
set.seed(42)

# Se seleccionan los datos de playeras

shirt.matrix <- as.matrix(train.data.shirt)

# Se reduce la dimensionalidad mediante t-SNE y se grafican los puntos

tsne_out <- Rtsne(shirt.matrix, pca=FALSE, perplexity=250, theta=0.0, max_iter = 1000, check_duplicates = FALSE)
reduced_data <- as.data.frame(tsne_out$Y)
ggplot(reduced_data, aes(V1, V2)) + geom_point(alpha = 0.5)

# Se grafican las imágenes sobre los puntos

plot_images <- function(x, y, images, width = 0.1*diff(range(x)), 
                       height = 0.1*diff(range(y))){
  
  images <- replicate(length(x), images, simplify=FALSE)
  stopifnot(length(x) == length(y))
  
  for (i in seq_along(x)){
    m<-matrix(t(train.data.shirt[i,(1:(28*28))]),ncol=28)
    m <- rotate(m)/max(as.numeric(unlist(m)))
    
    rasterImage(m, xleft=x[i] - 0.5*width,
                ybottom= y[i] - 0.5*height,
                xright=x[i] + 0.5*width, 
                ytop= y[i] + 0.5*height, interpolate=FALSE)
  }
}

par(mfrow=c(1,1))

plot(reduced_data, t="n")
plot_images(reduced_data[,1], reduced_data[,2], train.data.shirt)

# Se grafican imágenes en orden de acuerdo a la coordenada x

reduced_index <- cbind(tsne_out$Y, seq(1, 6000))
reduced_ordered <- reduced_index[order(reduced_index[,1]),]

par(mfrow=c(4, 4), mar=c(0, 0.2, 1, 0.2))
for (i in seq(1, 6000, 375)) {
  m<-matrix(t(train.data.shirt[i,(1:(28*28))]),ncol=28)
  image(rotate(rotate(m)), col=grey(seq(0, 1, length=256)))
}
