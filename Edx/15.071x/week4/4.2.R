letters <- read.csv("letters_ABPR.csv")
letters$isB <- as.factor(letters$letter == "B")
set.seed(1000)
spl <- sample.split(letters$isB, SplitRatio = 0.5)
train <- subset(letters, spl == T)
test <- subset(letters, spl == F)
prop.table(table(letters$isB))
CARTb <- rpart(isB ~ . - letter, data=train, method="class")
predB <- predict(CARTb, newdata = test, type = "class")
prop.table(table(predB, test$isB))
0.71758665 + 0.21822850

set.seed(1000)
rndF <- randomForest(formula = isB ~ . - letter, data = train)
predB <- predict(rndF, newdata = test)
prop.table(table(predB, test$isB))
0.747753530 + 0.240693196

letters$letter <- as.factor( letters$letter )

set.seed(2000)
spl <- sample.split(letters$letter, SplitRatio = 0.5)
train <- subset(letters, spl == T)
test <- subset(letters, spl == F)
prop.table(table(test$letter))

CARTb <- rpart(letter ~ . - isB, data=train, method="class")
prp(CARTb)
predB <- predict(CARTb, newdata = test, type = "class")
prop.table(table(predB, test$letter))
0.223363286 + 0.204107831 + 0.232991014 + 0.218228498

set.seed(1000)
rndF <- randomForest(formula = letter ~ . - isB, data = train)
predB <- predict(rndF, newdata = test)
prop.table(table(predB, test$letter))
0.2503209243 + 0.2439024390 + 0.2522464698 + 0.2336328626
