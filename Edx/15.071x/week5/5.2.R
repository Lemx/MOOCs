trials <- read.csv("clinical_trial.csv", stringsAsFactors=FALSE)

max(nchar(trials$abstract))

nrow(trials[nchar(trials$abstract) == 0,])

trials[which.min(nchar(trials$title)), ]$title

corpusTitle <- Corpus(VectorSource(trials$title))
corpusTitle <- tm_map(corpusTitle, tolower)
corpusTitle <- tm_map(corpusTitle, PlainTextDocument)
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle <- tm_map(corpusTitle, stemDocument)
dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmTitle <- as.data.frame(as.matrix(dtmTitle))


corpusAbstract <- Corpus(VectorSource(trials$abstract))
corpusAbstract <- tm_map(corpusAbstract, tolower)
corpusAbstract <- tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, stemDocument)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))

which.max(colSums(x = dtmAbstract))

colnames(dtmTitle) <- paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) <- paste0("A", colnames(dtmAbstract))


dtm <- cbind(dtmTitle, dtmAbstract)
dtm$trial <- trials$trial

library(caTools)
set.seed(144)
spl <- sample.split(dtm$trial, SplitRatio = 0.7)

train <- subset(dtm, spl ==TRUE)
test <- subset(dtm, spl ==FALSE)

prop.table(table(train$trial))

library(rpart)
library(rpart.plot)
cart <- rpart(trial ~ ., data = train, method = "class")
prp(cart)

predCart <- predict(cart)
max(predCart[,2])

prop.table(table(train$trial ,predCart[,2] >= 0.5))
0.33333333 + 0.48463902
prop.table(table(train$trial ,predCart[,2] >= 0.5), 1)

predTest <- predict(cart, newdata = test)
prop.table(table(test$trial, predTest[,2] >= 0.5))
0.46774194 + 0.29032258

library(ROCR)
prediction <- prediction(predictions = predTest[,2], labels = test$trial)
perf <- performance(prediction, measure = "auc")
as.numeric(performance(prediction, "auc")@y.values)
