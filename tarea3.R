library(MVA)
library(mclust)

# Load data

data("USairpollution")
usair.data <- USairpollution[-1]

pairs(usair.data, lower.panel = NULL, main="US air pollution")
subset(usair.data, manu > 1500)

pch <- rep("o", nrow(usair.data))
pch[c(7,30)] <- "+"
col <- rep("black", nrow(usair.data))
col[c(7,30)] <- "red"
pairs(usair.data, lower.panel = NULL, main="US air pollution", pch=pch, col=col)

usair.data <- subset(usair.data, manu < 1500)
usair.data <- scale(usair.data ,center=FALSE, scale=TRUE)

# EM clustering

mc <- Mclust(usair.data)
summary(mc)
groups <- mc$classification

pr <- prcomp(usair.data)$x[, 1:2]
plot(pr)
text(pr, labels=groups, cex=0.75, pos=2, col="red")

for (i in 1:length(unique(groups))) {
  group.names <- names(groups[groups==i])
  cat(group.names)
  group.SO2 <- USairpollution[group.names,]$SO2
  cat(sprintf(fmt = "\nMean SO2 for group %s: %s\n\n", i, mean(group.SO2)))
}

boxplot(subset(USairpollution, manu < 1500)$SO2~groups,
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
lab <- cutree(cc, h = 1.6)
text(usair_pc$scores[,1:2], labels = lab, cex = 0.6)

# K-means

rge <- sapply(usair.data, function(x) diff(range(x)))
usair.data_s <- sweep(usair.data, 2, rge, FUN = "/")

n <- nrow(usair.data_s)
wss <- rep(0, 6)
wss[1] <- (n-1)*sum(sapply(usair.data_s, var))

for (i in 2:6)
  wss[i] <- sum(kmeans(usair.data_s, centers=i)$withinss)

plot(1:6, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")

km <- kmeans(usair.data_s, centers=3)

pr <- prcomp(usair.data_s)$x[, 1:2]
plot(pr)
text(pr, labels=km$cluster, cex=0.75, pos=2, col="red")