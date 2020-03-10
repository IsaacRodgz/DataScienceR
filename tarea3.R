library(MVA)
library(mclust)

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

dm <- dist(usair.data)
plot(cs <- hclust(dm, method = "single"))
plot(cs <- hclust(dm, method = "complete"))
plot(cs <- hclust(dm, method = "average"))