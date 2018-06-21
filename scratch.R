library(isofor)
library(rulekit)


## randomly sample a variable
## randomly sample a cut point

random_val <- function(v) {
  sample(v, 1L)
}

random_var <- function(d) {
  sample(d, 1L)
}


random_partitions <- function(d, depth=1, n=3, pred=NULL) {

  if (depth > n || nrow(d) <= 1L) return(pred)

  var <- random_var(d)
  val <- random_val(var[[1]])

  if (is.factor(var[[1]])) {
    pred <- pcat(names(var), "in", as.character(val), all=levels(var[[1]]))
  } else {
    pred <- pnum(names(var), "lte", val)
  }

  ## get true/false
  bool <- predict(pred, d)

  ## recurse
  ## go left
  Reduce("&", list(pred, random_partitions(d[bool,], depth+1, n=n, pred)))

}


titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm=T)

rules <- replicate(500, random_partitions(titanic[-1], depth = 2), simplify = F)


nodes <- lapply(unique(rules), predict, titanic, type="sparse")
nodes <- do.call(cbind, nodes)

nodes <- as(nodes + 0, "dgTMatrix")

km <- sparse_kmodes(nodes, k=10, iter.max = 100, weighted = TRUE)


tapply(titanic$Survived, km$cluster, mean)
