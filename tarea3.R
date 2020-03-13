library(MVA)
library(mclust)

# Load data
data("USairpollution")

# Remove SO2 feature
usair.data <- USairpollution[-1]
s02.data <- USairpollution

# Visualize data
pairs(usair.data, lower.panel = NULL, main="US air pollution")

# Obtain outlier data
subset(usair.data, manu > 2000)

# Visualize outlier
pch <- rep("o", nrow(usair.data))
pch[7] <- "+"
col <- rep("black", nrow(usair.data))
col[7] <- "red"
pairs(usair.data, lower.panel = NULL, main="US air pollution", pch=pch, col=col)

# Remove outlier
usair.data <- subset(usair.data, manu < 2000)
s02.data <- subset(s02.data, manu < 2000)
s02.data <- s02.data$SO2

# Standardize dataset
rge <- sapply(usair.data, function(x) diff(range(x)))
usair.data <- sweep(usair.data, 2, rge, FUN = "/")

# EM clustering
mc <- Mclust(usair.data)
summary(mc)
groups <- mc$classification

# Visualize clustering of EM with PCA
pr <- prcomp(usair.data)$x[, 1:2]
plot(pr, col=groups)
text(pr, labels=rownames(usair.data), cex=0.75, pos=2, xpd=NA)

for (i in 1:length(unique(groups))) {
  group.names <- names(groups[groups==i])
  cat(group.names)
  group.SO2 <- USairpollution[group.names,]$SO2
  cat(sprintf(fmt = "\nMean SO2 for group %s: %s\n\n", i, mean(group.SO2)))
}

boxplot(s02.data~groups,
        main="Mean S02 per group",
        xlab="Group number",
        ylab="Mean S02",
        col="cadetblue3",
        border="black"
)

# Hierarchical clustering

dm <- dist(usair.data)
plot(cs <- hclust(dm, method = "single"))
plot(ca <- hclust(dm, method = "average"))
plot(cc <- hclust(dm, method = "complete"))

usair_pc <- princomp(dm, cor = TRUE)
xlim <- range(usair_pc$scores[,1])
plot(usair_pc$scores[,1:2], type = "n", xlim = xlim, ylim = xlim)
lab <- cutree(cc, h = 1.2)
text(usair_pc$scores[,1:2], labels = lab, cex = 0.6)

boxplot(s02.data~lab,
        main="Mean S02 per group",
        xlab="Group number",
        ylab="Mean S02",
        col="cadetblue3",
        border="black"
)

# K-means 

n <- nrow(usair.data_s)
wss <- rep(0, 6)
wss[1] <- (n-1)*sum(sapply(usair.data_s, var))

for (i in 2:6)
  wss[i] <- sum(kmeans(usair.data_s, centers=i)$withinss)

plot(1:6, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")

km <- kmeans(usair.data, centers=3)
km.labels <- km$cluster

pr <- prcomp(usair.data_s)$x[, 1:2]
plot(pr, col=km.labels)
text(pr, labels=rownames(usair.data), cex=0.75, pos=2, xpd=NA)

boxplot(s02.data~km.labels,
        main="Mean S02 per group",
        xlab="Group number",
        ylab="Mean S02",
        col="cadetblue3",
        border="black"
)