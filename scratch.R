
data(mtcars)

## request number of modes
km <- kmodes_gower(mtcars, 3L)

## or specify them as a list of data.frame rows
kmodes_gower(mtcars, mtcars[c(1,10,25),])


km <- kmodes_gower(iris[-5],  3)
table(km$clusters, iris$Species)





