#' @export
update_mode <- function(x) UseMethod("update_mode")

#' @export
update_mode.default <- function(x) {
  stop("Update methods not implemented for data type:", class(x))
}

#' @export
update_mode.data.frame <- function(x) {
  data.frame(lapply(x, update_mode), check.names = F, stringsAsFactors = F)
}

#' @export
update_mode.numeric <- function(x) {
  median(x, na.rm=T)
}

#' @export
update_mode.logical <- function(x) {
  if (mean(x) > 0.5) TRUE else FALSE
}

#' @export
update_mode.character <- function(x) {
  tbl <- table(x)
  names(tbl)[which.max(tbl)]
}

#' @export
update_mode.factor <- function(x) {
  factor(levels(x)[which.max(tabulate(x))], levels=levels(x))
}

calculate_gower_distance <- function(d, modes) lapply(modes, gower_dist, d)

all_dups <- function(x) duplicated(x) | duplicated(x, fromLast = TRUE)

assign_clusters <- function(dists, current_clusters) {

  dists <- do.call(cbind, dists)
  mins <- apply(dists, 1, min)

  ## candidate clusters
  candidates <- apply(dists == mins, 1, which)
  f <- mapply(`%in%`, current_clusters, candidates)

  ## If current cluster is found in candidates, take it, otherwise
  ## take a random cluster among the ties
  ifelse(f, current_clusters, sapply(candidates, sample, 1L))
}

calculate_clusters <- function(d, modes, current_clusters) {
  dists <- calculate_gower_distance(d, modes)
  assign_clusters(dists, current_clusters)
}

update_modes <- function(d, clusters) {
  modes <- list()
  for (k in unique(clusters)) {
    f <- clusters == k
    modes[[k]] <- update_mode(d[f,])
  }
  modes
}

zeroth_row <- function(d) {
  res <- d[0,,drop=F]
  row.names(res) <- NULL
  res
}

modes_are_valid <- function(d, modes) {
  proto <- zeroth_row(d)
  isTRUE(all(sapply(lapply(modes, zeroth_row), identical, proto)))
}

dist_to_clusters <- function(d, cluster, modes) {
  sums <- numeric(length(modes))
  for (k in seq_along(modes)) {
    f <- cluster == k
    sums[[k]] <- sum(suppressWarnings(gower_dist(d[f,], modes[[k]])))
  }
  sums
}

#' @export
setGeneric("kmodes_gower", function(data, modes, max.iter=100, verbose=TRUE) standardGeneric("kmodes_gower"))

#' @export
setMethod(
  "kmodes_gower",
  c("data.frame", "numeric"),
  function(data, modes, max.iter, verbose) {
    kmodes_gower(data, as.integer(modes[[1]]), max.iter, verbose)
  })

#' @export
setMethod(
  "kmodes_gower",
  c("data.frame", "data.frame"),
  function(data, modes, max.iter, verbose) {
    kmodes_gower(data, split(modes, seq.int(nrow(modes))), max.iter)
})

## For some reason, using `duplicated` to sample unique modes
## was taking a very long time. This is much quicker in practice.
sample_modes_without_duplicates <- function(data, k) {
  repeat {
    i <- sample.int(nrow(data), k)
    modes <- data[i,]
    if (!any(duplicated(modes))) break
  }
  split(modes, seq.int(k))
}

#' @export
setMethod(
  "kmodes_gower",
  c("data.frame", "integer"),
  function(data, modes, max.iter, verbose) {
    stopifnot(identical(length(modes), 1L))
    modes <- sample_modes_without_duplicates(data, modes)
    kmodes_gower(data, modes, max.iter, verbose)
  })

#' @export
setMethod(
  "kmodes_gower",
  c("data.frame", "list"),
  function(data, modes, max.iter, verbose) {

    ## check that list of modes are valid
    if (!modes_are_valid(data, modes)) {
      stop("`modes`` must all have the same spec as `data`")
    }

    dists <- calculate_gower_distance(data, modes)
    clusters <- assign_clusters(dists, 0)

    if (isTRUE(verbose)) cat("Iter   % Changed Cluster", sep = "\n")

    iter <- 1
    repeat {
      modes <- update_modes(data, clusters)
      dists <- calculate_gower_distance(data, modes)
      new_clusters <- assign_clusters(dists, clusters)

      n_changed <- sum(clusters != new_clusters)

      if (isTRUE(verbose)) {
        cat(sprintf("%3d%19.2f", iter, n_changed*100/length(clusters)), sep = "\n")
      }

      if (n_changed == 0 || iter >= max.iter) break

      clusters <- new_clusters
      iter <- iter + 1
    }

    ## summary stats
    sum_distance_total <-sum(gower_dist(data, update_mode(data)))
    distance_within <- dist_to_clusters(data, clusters, modes)
    sum_distance_within <- sum(distance_within)

    structure(
      list(
        clusters = clusters,
        modes = modes,
        sum_distance_total = sum_distance_total,
        sum_distance_within = sum_distance_within,
        distance_within_cluster = distance_within),
      class = "kmodes_gower")

})



#' Predict KModes Gower Object on New Data
#' @param object \code{kmodes_gower} object produces by \link{kmodes_gower} function.
#' @param newdata data.frame on which to predict.
#' @param type What to predict on the new data. Either the assigned cluster or the distance matrix
#' containining the distance for each objservation to each cluster mode.
#' @param normalize a logical value for whether \code{rowSums(dists)} should all equal one.
#' @export
predict.kmodes_gower <- function(object, newdata, type=c("cluster", "distance"), normalize=TRUE) {

  if (!modes_are_valid(newdata, object$modes)) {
    stop("`modes`` must all have the same spec as `data`")
  }

  dists <- calculate_gower_distance(newdata, object$modes)

  switch(
    match.arg(type),
    "cluster" = assign_clusters(dists, 0),
    "distance" ={
      mat <- do.call(cbind, dists)
      if (isTRUE(normalize)) mat/rowSums(mat) else mat
    })
}

#' @export
setOldClass("kmodes_gower")

#' @export
setMethod("show", "kmodes_gower", function(object) {

  sizes <- paste0(table(km$clusters), collapse = ", ")
  cat(sprintf("K-modes clustering with %d clusters of sizes %s", length(object$modes), sizes), sep="\n")

  modes <- do.call(rbind, object$modes)
  cat("\nCluster modes:", sep="\n")
  print(modes)

  cat("\nWithin cluster sum of distances by cluster", sep="\n")
  print(object$distance_within_cluster)

  pct <- object$sum_distance_within / km$sum_distance_total
  txt <- sprintf("\n within_dist / total_dist = %4.1f %%)", pct * 100)
  cat(txt, sep="\n")
})

#' @export
print.kmodes_gower <- function(x, ...) show(x)
