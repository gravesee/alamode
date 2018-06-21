
### random k centers
random_sparse_modes_ <- function(k, data) {
  nentries <- sum(data[1,])
  i <- do.call(c, replicate(k, ceiling(runif(nentries, 0, ncol(data))), simplify = FALSE))
  j <- rep(seq.int(k), each=nentries)

  #Matrix::sparseMatrix(i=i, j=j, dims = c(ncol(data), k), x=rep(1, length(i)))

  Matrix::sparseMatrix(i=i, j=j, dims = c(ncol(data), k), x=runif(length(i), 0, 1))
}

## Sample k modes
sample_k_modes_ <- function(k, data) {
  uniq <- unique(split(data@j, data@i))
  eyes <- uniq[sample(seq_along(uniq), k)]
  i <- unlist(eyes)
  Matrix::sparseMatrix(x = runif(length(i), 0.99, 1), i = i + 1,
                       j = rep(seq.int(k), lengths(eyes)), dims = c(ncol(data), k))
}

## update modes with 1 or 0 based on mean > 0.50
update_mode_ <- function(k, data, cluster, weighted=TRUE) {
  means <- Matrix::colMeans(data[cluster == k,,drop=F])
  if (isTRUE(weighted)) means else means > 0.5
}


#' Sparse, Binary K-Modes
#' Cluster sparse, binary matrices from the Matrix package
#' @param data Sparse, binary matrix produced by the Matrix package
#' @param k Number of requested clusters
#' @param iter.max Maximum number of iterations. The algorithm will stop if it
#' converges or reaches this limit -- whichever happens first.
#' @param random_modes Should the initial cluster centers be generated randomly?
#' The default is to randomly sample k centers from the unique rows of the input
#' dataset.
#' @param weighted If TRUE, the default, the updated mode centers use the mean
#' incidence rate for each column rather than the mode.
#' @param verbose Logical for whether to output training progress
#' @return A list object containing the cluster membership and a sparse matrix of
#' cluster centers.
#' @export
sparse_kmodes <- function(data, k, iter.max=10, random_modes=FALSE, weighted=TRUE, verbose=TRUE) {

  stopifnot(is(data, "dgTMatrix"))

  idf <- nrow(data) / (Matrix::colSums(data) + 1)

  ## all clusters start at zero
  cluster <- numeric(nrow(nodes))

  ## random points in data space
  modes <- if (isTRUE(random_modes)) random_sparse_modes_(k, data) else sample_k_modes_(k, data)

  if (isTRUE(weighted)) modes <- modes * idf

  ## Assign initial cluster
  cluster <- apply(data %*% modes, 1, which.max)

  if (verbose) cat("Iter   % Changed", sep = "\n")

  ## keep updating modes until convergence or max iters reached
  iter <- 0
  repeat{

    iter <- iter + 1

    ## update cluster "center"
    for (i in unique(cluster)) modes[,i] <- update_mode_(i, nodes, cluster, weighted)

    cluster_new <- apply(data %*% modes, 1, which.max)

    ## check termination condition
    if (identical(cluster, cluster_new) || iter > iter.max) break

    if (verbose) {
      pct <- mean(cluster != cluster_new)
      cat(sprintf("%4d      %6.2f", iter, pct * 100), sep = "\n")
    }

    cluster <- cluster_new
  }

  ## calculate coherence
  structure(
    list(
      cluster =cluster,
      centers = modes),
    class = "sparsekmodes")
}


#' Predict Sparse K-Modes Object on New Data
#' @param object an object of class "sparsekmodes"
#' @param newdata A sparse, binary matrix with the same number of columns as \code{nrow(object@centers)}
#' @param type what type of prediction to calculate. Cluster returns the id of the closest center.
#' sim returns the similarity matrix between \code{newdata} and \code{object@centers}.
#' @param normalize Whether to normalize the similarity matrix such that \code{all(rowSums(sim) == 1)} is TRUE.
#' @export
predict.sparsekmodes <- function(object, newdata, type=c("cluster", "sim"), normalize=TRUE) {

  sims <- newdata %*% object$centers

  switch(
    match.arg(type),
    "cluster" = apply(sims, 1, which.max),
    "sim" = if (isTRUE(normalize)) sims / Matrix::rowSums(sims) else sims
  )
}




