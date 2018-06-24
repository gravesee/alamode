KModes Clustering Using Gower Distance
--------------------------------------

Most clustering examples use simple datasets with all numeric features.
But what if you need to cluster a dataset with mixed features? Gower
distance is a distance metric that tries to generalize similarity across
different types of features.

    x1 <- data.frame(a="cat", b=1, c=TRUE, stringsAsFactors=FALSE)
    x2 <- data.frame(a="dog", b=0.9, c=FALSE, stringsAsFactors=FALSE)
    x3 <- data.frame(a="goat", b=0.8, c=FALSE, stringsAsFactors=FALSE)

    gower_dist(x1, rbind(x1, x2, x3))

    ## [1] 0.0000000 0.8333333 1.0000000

KModes
------

Unlike KMeans, KModes uses the modes of each cluster rather than the
average. For categorical data, this means taking the value that appears
the most often. For numeric data, this is the median value. If there is
a tie for the most frequent value, the first one is taken.

    km <- kmodes_gower(mtcars, 3)

    ## Iter   % Changed Cluster
    ##   1               9.38
    ##   2               3.12
    ##   3               9.38
    ##   4               6.25
    ##   5               3.12
    ##   6               9.38
    ##   7               3.12
    ##   8               3.12
    ##   9               0.00

    km

    ## $clusters
    ##  [1] 1 1 1 2 2 2 2 1 1 1 1 2 2 2 2 2 2 1 1 1 1 2 2 2 2 1 1 1 2 1 2 1
    ## 
    ## $modes
    ## $modes[[1]]
    ##    mpg cyl   disp hp drat     wt   qsec vs am gear carb
    ## 1 22.8   4 120.65 96 3.92 2.5425 18.605  1  1    4    2
    ## 
    ## $modes[[2]]
    ##     mpg cyl disp  hp drat   wt  qsec vs am gear carb
    ## 1 15.35   8  334 180 3.08 3.65 17.35  0  0    3    3
    ## 
    ## 
    ## $sum_distance_total
    ## [1] 8.562269
    ## 
    ## $sum_distance_within
    ## [1] 6.400316
    ## 
    ## $distance_within_cluster
    ## [1] 3.702761 2.697555
    ## 
    ## attr(,"class")
    ## [1] "kmodes_gower"

    plot(mtcars, col=km$clusters)

![](README_files/figure-markdown_strict/unnamed-chunk-3-1.png)

Prediction Methods
------------------

Once a dataset is cluster with `kmodes_gower` the resulting object can
be used for predictions on new data. There are two kinds of predictions:
Clusters and Distance. `cluster` returns the index of the nearest
cluster for each record of the input dataset. `distance` returns a
matrix with `K` columns each containing the gower distance between the
input records and each mode. The argument `normalize` determines whether
the rows of the distance matrix should sum to one.

    cluster <- predict(km, mtcars) # default returns clusters
    table(cluster)

    ## cluster
    ##  1  2 
    ## 23  9

    d1 <- predict(km, mtcars, type="distance", normalize=FALSE)
    d2 <- predict(km, mtcars, type="distance", normalize=TRUE)

    head(d1)

    ##            [,1]      [,2]
    ## [1,] 0.20857489 0.3465317
    ## [2,] 0.20844162 0.3345437
    ## [3,] 0.02497782 0.5289812
    ## [4,] 0.29572347 0.2581926
    ## [5,] 0.48396926 0.0448337
    ## [6,] 0.32694250 0.2706686

    head(d2)

    ##            [,1]       [,2]
    ## [1,] 0.37573848 0.62426152
    ## [2,] 0.38388075 0.61611925
    ## [3,] 0.04508965 0.95491035
    ## [4,] 0.53387775 0.46612225
    ## [5,] 0.91521662 0.08478338
    ## [6,] 0.54708238 0.45291762
