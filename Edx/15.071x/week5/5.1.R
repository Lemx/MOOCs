wiki <- read.csv("wiki.csv", stringsAsFactors=FALSE)
wiki$Vandal <- as.factor(wiki$Vandal)

summary(wiki)


corpusAdded <- Corpus(VectorSource(wiki$Added))
corpusAdded <- tm_map(corpusAdded, PlainTextDocument)
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded <- tm_map(corpusAdded, stemDocument)
dtmAdded <- DocumentTermMatrix(corpusAdded)
dtmAdded

sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
sparseAdded

wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) <- paste("A", colnames(wordsAdded))



corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, PlainTextDocument)
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)
dtmRemoved <- DocumentTermMatrix(corpusRemoved)
dtmRemoved

sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) <- paste("R", colnames(wordsRemoved))


wikiWords <- cbind(wordsAdded, wordsRemoved)

wikiWords$Vandal <- wiki$Vandal

library(caTools)
set.seed(123)

spl <- sample.split(wikiWords$Vandal, SplitRatio = 0.7)

train <- subset(wikiWords, spl ==TRUE)
test <- subset(wikiWords, spl ==FALSE)

prop.table(table(wikiWords$Vandal))

library(rpart)
cart <- rpart(Vandal ~ ., data = train, method = "class")
cartPred <- predict(cart, newdata = test, type = "class")

prop.table(table(test$Vandal, cartPred[, 2]))
0.01031814 + 0.53138435

prp(cart)



wikiWords2 <- wikiWords
wikiWords2$HTTP <- ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
sum(wikiWords2$HTTP)

wikiTrain2 <- subset(wikiWords2, spl==TRUE)
wikiTest2 <- subset(wikiWords2, spl==FALSE)

cart <- rpart(Vandal ~ ., data = wikiTrain2, method = "class")
cartPred <- predict(cart, newdata = wikiTest2, type = "class")
prop.table(table(wikiTest2$Vandal, cartPred))
0.523645744 + 0.049011178

wikiWords2$NumWordsAdded <- rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved <- rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

wikiTrain2 <- subset(wikiWords2, spl==TRUE)
wikiTest2 <- subset(wikiWords2, spl==FALSE)

cart <- rpart(Vandal ~ ., data = wikiTrain2, method = "class")
cartPred <- predict(cart, newdata = wikiTest2, type = "class")
prop.table(table(wikiTest2$Vandal, cartPred))
0.2132416 + 0.4419604


wikiWords3 <- wikiWords2
wikiWords3$Minor <- wiki$Minor
wikiWords3$Loggedin <- wiki$Loggedin

wikiTrain3 <- subset(wikiWords3, spl==TRUE)
wikiTest3 <- subset(wikiWords3, spl==FALSE)

cart <- rpart(Vandal ~ ., data = wikiTrain3, method = "class")
cartPred <- predict(cart, newdata = wikiTest3, type = "class")
prop.table(table(wikiTest3$Vandal, cartPred))
0.20722270 + 0.51160791

prp(cart)
