


library(titanic)
X <- subset(titanic::titanic_train, select = c(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked))

## request number of modes
km <- kmodes_gower(X, 3L)

plot(mtcars, col=km$clusters)

## or specify them as a list of data.frame rows
kmodes_gower(mtcars, mtcars[c(1,10,25),])


km <- kmodes_gower(iris[-5],  3)

table(km$clusters, iris$Species)

## mismatches
mm <- c("virginica","versicolor", "setosa")[km$clusters] != iris$Species

dists <- predict(km, iris[,-5], type="distance")





mn <- colMeans(iris[-5])
sum(sqrt((iris[-5] - mn) ^ 2))


for (i in 1:10) X <- rbind(X, X)
for (i in 1:4) X <- cbind(X, X)

km <- kmodes_gower(X[-1], 10)


ks <- 2:10

kms <- lapply(ks, function(k) kmodes_gower(iris[-5], k))
p <- sapply(kms[-1], `[[`, "sum_distance_within")
plot(p)

km <- kmodes_gower(X[-1], 4)

