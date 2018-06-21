library(gower)

data(titanic, package="onyx")

## update centers
update_mode <- function(x) UseMethod("update_mode")

update_mode.data.frame <- function(x) {
  data.frame(lapply(x, update_mode))
}

update_mode.numeric <- function(x) {
  median(x, na.rm=T)
}

update_mode.factor <- function(x) {
  factor(levels(x)[which.max(tabulate(x))], levels=levels(x))
}

calculate_clusters <- function(d, modes) {

  dists <- lapply(modes, gower_dist, d)

  apply(do.call(cbind, dists), 1, which.min)
}


update_modes <- function(d, clusters) {
  modes <- list()
  for (k in unique(clusters)) {
    f <- clusters == k
    modes[[k]] <- update_mode(d[f,])
  }
  modes
}



k_modes <- function(d, k) {

  i <- sample.int(nrow(d), k)
  modes <- split(d[i,], seq.int(k))

  clusters <- calculate_clusters(d, modes)

  repeat {

    modes <- update_modes(d, clusters)
    new_clusters <- calculate_clusters(d, modes)

    if (all(clusters == new_clusters)) break

    clusters <- new_clusters

  }

  clusters

}



d <- read

for(i in 1:8) titanic <- rbind(titanic, titanic)


d <- haven::read_sas("F:/sas-dev/SYNTH1801_0001/data/shell_perf_jefferson.sas7bdat")

X <- d[9:221]

res <- k_modes(X, 25)

plot(X, col=res)


ksd <- data.frame(paid=d$paid, fpd=d$fpd_flag, cluster=ave(d$fpd_flag, res, FUN=function(x) mean(x, na.rm=T)))

library(ks)

tbl <- ks_table(paid+fpd~cluster, data=ksd)





