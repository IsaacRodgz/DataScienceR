library(MVA)
library(mclust)

data("USairpollution")
usair.data <- USairpollution[-1]

pairs(usair.data, lower.panel = NULL, main="US air pollution")

mc <- Mclust(usair.data)
summary(mc)
groups <- mc$classification

pr <- prcomp(usair.data)$x[, 1:2]

plot(pr)
text(pr, labels=groups, cex=0.75, pos=2, col="red")

for (i in 1:7) {
  group.names <- names(groups[groups==i])
  cat(group.names)
  group.SO2 <- USairpollution[group.names,]$SO2
  cat(sprintf(fmt = "\nMean SO2 for group %s: %s\n\n", i, mean(group.SO2)))
}

boxplot(USairpollution$SO2~groups,
        main="Mean S02 per group",
        xlab="Group number",
        ylab="Mean S02",
        col="cadetblue3",
        border="black"
)