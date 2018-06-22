update_mode <- function(x) UseMethod("update_mode")

update_mode.default <- function(x) {
  stop("Update methods not implemented for data type:", class(x))
}

update_mode.data.frame <- function(x) {
  data.frame(lapply(x, update_mode))
}

update_mode.numeric <- function(x) {
  median(x, na.rm=T)
}

update_mode.logical <- function(x) {
  if (mean(x) > 0.5) TRUE else FALSE
}

update_mode.character <- function(x) {
  update_mode(factor(x))
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

is_single_integer <- function(n) {
  is.numeric(n) && identical(length(n), 1L) && floor(n) == n
}

setGeneric("kmodes_gower", function(data, modes, max.iter=100) standardGeneric("kmodes_gower"))

#' @export
setMethod(
  "kmodes_gower",
  c("data.frame", "numeric"),
  function(data, modes, max.iter) {
    kmodes_gower(data, as.integer(modes[[1]]), max.iter)
  })

#' @export
setMethod(
  "kmodes_gower",
  c("data.frame", "data.frame"),
  function(data, modes, max.iter) {
    kmodes_gower(data, split(modes, seq.int(nrow(modes))), max.iter)
})

#' @export
setMethod(
  "kmodes_gower",
  c("data.frame", "integer"),
  function(data, modes, max.iter) {
    stopifnot(identical(length(modes), 1L))
    i <- sample(which(!duplicated(data)), modes)
    modes <- split(data[i,], seq.int(modes))
    kmodes_gower(data, modes, max.iter)
  })

#' @export
setMethod(
  "kmodes_gower",
  c("data.frame", "list"),
  function(data, modes, max.iter) {

    ## check that list of modes are valid
    ## TODO: check_modes_valid()

    clusters <- calculate_clusters(data, modes)

    repeat {

      modes <- update_modes(data, clusters)
      new_clusters <- calculate_clusters(data, modes)

      if (all(clusters == new_clusters)) break

      clusters <- new_clusters

    }

    structure(
      list(
        clusters = clusters,
        centers = modes),
      class = "kmodes_gower")

})

#' @export
predict.kmodes_gower <- function(object, newdata, type=c("cluster", "centers", "distance")) {
  stop("Implement!")
}

