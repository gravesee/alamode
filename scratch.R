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

library(haven)

d <- haven::read_sas("F:/sas-dev/SYNTH1801_0001/data/shell_perf_jefferson.sas7bdat")

## convert chars to facs
chars <- sapply(d, is.character)
d[chars] <- lapply(d[chars], factor)

iso <- iForest(d[9:221], nt=500, phi=16)
nodes <- predict(iso, d[9:221], sparse = TRUE)

km <- sparse_kmodes(nodes, k=10, iter.max = 100)
