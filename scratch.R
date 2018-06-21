library(isofor)


data(mtcars)

## randomly "hash" the data using isolation forest
mod <- iForest(titanic[-1], nt=500, phi=8)
nodes <- predict(mod, titanic[-1], sparse = TRUE)

# tf <- (nodes / rowSums(nodes))
# ○idf <-

km <- sparse_kmodes(nodes, k=10, iter.max = 100)

tapply(titanic$Survived, km$cluster, mean)


predict(km, nodes)

