airlines <- read.csv("AirlinesCluster.csv")
summary(airlines)

library(caret)
preproc <- preProcess(airlines)
airlinesNorm <- predict(preproc, airlines)
summary(airlinesNorm)

airlinesNormVector <- as.vector(airlinesNorm)
distance <- dist(airlinesNormVector, method = "euclidean")
cluster <- hclust(distance, method = "ward.D")
plot(cluster)

clusterGroups <- cutree(cluster, k = 5)
spl <- split(airlinesNorm, clusterGroups)
lapply(spl, nrow)

tapply(airlines$Balance, clusterGroups, mean)
tapply(airlines$QualMiles, clusterGroups, mean)
tapply(airlines$BonusMiles, clusterGroups, mean)
tapply(airlines$BonusTrans, clusterGroups, mean)
tapply(airlines$FlightMiles, clusterGroups, mean)
tapply(airlines$FlightTrans, clusterGroups, mean)
tapply(airlines$DaysSinceEnroll, clusterGroups, mean)

set.seed(88)
KMC <- kmeans(airlinesNorm, centers = 5, iter.max = 1000)
spl <- split(airlines, KMC$cluster)
lapply(spl, nrow)
