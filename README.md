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
    ##   1              43.75
    ##   2              12.50
    ##   3              15.62
    ##   4               3.12
    ##   5               0.00

    km

    ## $clusters
    ##  [1] 1 1 1 3 2 3 2 1 1 3 3 2 2 2 2 2 2 1 1 1 3 2 2 2 2 1 1 1 2 1 2 1
    ## 
    ## $modes
    ## $modes[[1]]
    ##    mpg cyl  disp hp drat   wt qsec vs am gear carb
    ## 1 24.4   4 120.3 93 3.92 2.32 18.6  1  1    4    2
    ## 
    ## $modes[[2]]
    ##    mpg cyl  disp    hp  drat    wt   qsec vs am gear carb
    ## 1 15.2   8 350.5 192.5 3.115 3.755 17.175  0  0    3  3.5
    ## 
    ## $modes[[3]]
    ##    mpg cyl  disp  hp drat   wt  qsec vs am gear carb
    ## 1 19.2   6 167.6 110  3.7 3.44 19.44  1  0    3    1
    ## 
    ## 
    ## $sum_distance_total
    ## [1] 8.562269
    ## 
    ## $sum_distance_within
    ## [1] 7.409058
    ## 
    ## $distance_within_cluster
    ## [1] 3.022592 2.772151 1.614315
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
    ##  1  2  3 
    ## 23  6  3

    d1 <- predict(km, mtcars, type="distance", normalize=FALSE)
    d2 <- predict(km, mtcars, type="distance", normalize=TRUE)

    head(d1)

    ##            [,1]       [,2]       [,3]
    ## [1,] 0.22092529 0.34745588 0.33461067
    ## [2,] 0.22079202 0.33546794 0.32262273
    ## [3,] 0.02500652 0.54668028 0.25602122
    ## [4,] 0.30818210 0.27882427 0.06021400
    ## [5,] 0.49631966 0.05126177 0.26502612
    ## [6,] 0.33940112 0.29130025 0.06716408

    head(d2)

    ##            [,1]       [,2]       [,3]
    ## [1,] 0.24465923 0.38478297 0.37055780
    ## [2,] 0.25121899 0.38169820 0.36708281
    ## [3,] 0.03021177 0.66047479 0.30931344
    ## [4,] 0.47616255 0.43080268 0.09303477
    ## [5,] 0.61077411 0.06308306 0.32614282
    ## [6,] 0.48634178 0.41741607 0.09624216
