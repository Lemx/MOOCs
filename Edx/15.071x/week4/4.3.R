census <- read.csv("census.csv")
set.seed(2000)
split <- sample.split(census$over50k, SplitRatio = 0.6)
train <- subset(census, split == T)
test <- subset(census, split == F)
log <- glm(over50k ~ ., data = train, family = "binomial")
summary(log)
pred <- predict(log, newdata = test, type = "response")
prop.table(table(test$over50k, pred > 0.5))
0.70760691 + 0.14760378

prop.table(table(test$over50k))

prediction <- prediction(predictions = pred, labels = test$over50k)
perf <- performance(prediction, measure = "auc")
as.numeric(performance(prediction, "auc")@y.values)

tree <- rpart(over50k ~ ., data = train, method = "class")
prp(tree)
pred <- predict(tree, newdata = test, type = "class")
prop.table(table(test$over50k, pred))
0.72261747 + 0.12477523

pred <- predict(tree, newdata = test)
prediction <- prediction(predictions = pred[,2], labels = test$over50k)
perf <- performance(prediction, "tpr", "fpr")
as.numeric(performance(prediction, "auc")@y.values)
plot(perf)

set.seed(1)
trainSmall <- train[sample(nrow(train), 2000), ]
set.seed(1)
rndf <- randomForest(over50k ~ ., data = trainSmall)
pred <- predict(rndf, newdata = test)
prop.table(table(pred, test$over50k))
0.74927684 + 0.08521617

vu <- varUsed(rndf, count=TRUE)
vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(rndf$forest$xlevels[vusorted$ix]))
varImpPlot(rndf)

cartGrid <- expand.grid( .cp = seq(0.002,0.1,0.002))
control <- trainControl(method = "cv", number = 10)
set.seed(2)
cross <- train(over50k ~ ., data = train, method = "rpart", trControl = control, tuneGrid = cartGrid)
cross
# bestTree <- cross$finalModel
tree <- rpart(over50k ~ ., data = train, cp = 0.002)
pred <- predict(tree, newdata = test, type = "class")
prop.table(table(test$over50k, pred))
0.71753577 + 0.14369479
prp(tree)
