emails <- read.csv("emails.csv", stringsAsFactors=FALSE)

sum(emails$spam)

max(nchar(emails$text))

which.min(nchar(emails$text))

corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

dtm <- DocumentTermMatrix(corpus)

spdtm <- removeSparseTerms(dtm, 0.95)

emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))

emailsSparse$spam <- emails$spam
length(colSums(emailsSparse)[colSums(emailsSparse[emails$spam == 0, ]) >= 5000])
length(colSums(emailsSparse)[colSums(emailsSparse[emails$spam == 1, ]) >= 1000])

emailsSparse$spam <- as.factor(emailsSparse$spam)

library(caTools)
set.seed(123)
spl <- sample.split(emailsSparse$spam, SplitRatio = 0.7)

train <- subset(emailsSparse, spl ==TRUE)
test <- subset(emailsSparse, spl ==FALSE)

spamLog <- glm(spam ~ ., data = train, family = "binomial")
spamCART <- rpart(spam ~ ., data = train, method = "class")

library(randomForest)
set.seed(123)
spamRF <- randomForest(spam ~ ., data = train)

length(spamLog[spamLog$y < 0.00001])
length(spamLog[spamLog$y > 0.99999])
length(spamLog[spamLog$y < 0.99999 && spamLog$y > 0.00001])

summary(spamLog)

prp(spamCART)

predLog <- predict(spamLog)
prop.table(table(train$spam, predLog >= 0.5))
0.7610972569 + 0.2379052369

library(ROCR)
predictionLog <- prediction(predictions = predLog, labels = train$spam)
perfLog <- performance(predictionLog, measure = "auc")
as.numeric(perfLog@y.values)

predCart <- predict(spamCART)
prop.table(table(train$spam, predCart[,2] >= 0.5))
0.71945137 + 0.22294264

predictionCart <- prediction(predictions = predCart[,2], labels = train$spam)
perfCart <- performance(predictionCart, measure = "auc")
as.numeric(perfCart@y.values)

predRf <- predict(spamRF, type = "prob")
prop.table(table(train$spam, predRf[,2] >= 0.5))
0.751371571 + 0.227930175

predictionRf <- prediction(predictions = predRf[,2], labels = train$spam)
perfRf <- performance(predictionRf, measure = "auc")
as.numeric(perfRf@y.values)





predLog <- predict(spamLog, newdata = test)
prop.table(table(test$spam, predLog >= 0.5))
0.73224680 + 0.21885914

predictionLog <- prediction(predictions = predLog, labels = test$spam)
perfLog <- performance(predictionLog, measure = "auc")
as.numeric(perfLog@y.values)

predCart <- predict(spamCART, newdata = test)
prop.table(table(test$spam, predCart[,2] >= 0.5))
0.71478463 + 0.22467986

predictionCart <- prediction(predictions = predCart[,2], labels = test$spam)
perfCart <- performance(predictionCart, measure = "auc")
as.numeric(perfCart@y.values)

predRf <- predict(spamRF, type = "prob", newdata = test)
prop.table(table(test$spam, predRf[,2] >= 0.5))
0.75087311 + 0.22467986

predictionRf <- prediction(predictions = predRf[,2], labels = test$spam)
perfRf <- performance(predictionRf, measure = "auc")
as.numeric(perfRf@y.values)
