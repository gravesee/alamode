library(isofor)


data(mtcars)

## randomly "hash" the data using isolation forest
mod <- iForest(titanic[-1], nt=100, phi=64)
nodes <- predict(mod, titanic[-1], sparse = TRUE)

km <- sparse_kmodes(nodes, k=10, iter.max = 100)

tapply(titanic$Survived, km$cluster, mean)

## demonstrat an example somehow ...
plot(mtcars[,c(1,3)], col=km$cluster)
