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

    cats <- c("cyl","vs","am","gear","carb")
    mtcars[cats] <- lapply(mtcars[cats], factor)

    km <- kmodes_gower(mtcars, 3)

    ## Iter   % Changed Cluster
    ##   1              37.50
    ##   2              18.75
    ##   3               6.25
    ##   4               6.25
    ##   5               3.12
    ##   6               0.00

    show(km)

    ## K-modes clustering with 3 clusters of sizes 16, 11, 5
    ## 
    ## Cluster modes:
    ##    mpg cyl   disp  hp drat     wt   qsec vs am gear carb
    ## 1 22.8   4 120.65  96 3.92 2.5425 18.605  1  1    4    2
    ## 2 15.0   8 360.00 215 3.15 3.5700 17.020  0  0    3    4
    ## 3 17.3   8 275.80 180 3.07 3.7300 18.000  0  0    3    3
    ## 
    ## Within cluster sum of distances by cluster
    ## [1] 4.484579 2.259044 1.325879
    ## 
    ##  within_dist / total_dist = 72.2 %)

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
    ## 21  8  3

    d1 <- predict(km, mtcars, type="distance", normalize=FALSE)
    d2 <- predict(km, mtcars, type="distance", normalize=TRUE)

    head(d1)

    ##           [,1]      [,2]      [,3]
    ## [1,] 0.3189645 0.4345832 0.5039349
    ## [2,] 0.3188312 0.4225952 0.4919469
    ## [3,] 0.1028999 0.6566431 0.6138735
    ## [4,] 0.4645546 0.3917196 0.3430850
    ## [5,] 0.5294238 0.1210936 0.1377230
    ## [6,] 0.4957737 0.4041956 0.3547231

    head(d2)

    ##            [,1]      [,2]      [,3]
    ## [1,] 0.25365323 0.3455978 0.4007490
    ## [2,] 0.25850342 0.3426337 0.3988629
    ## [3,] 0.07492257 0.4781092 0.4469682
    ## [4,] 0.38733570 0.3266074 0.2860569
    ## [5,] 0.67165278 0.1536252 0.1747220
    ## [6,] 0.39513564 0.3221472 0.2827172
