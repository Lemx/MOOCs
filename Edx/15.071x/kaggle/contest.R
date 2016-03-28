library(randomForest)
library(tm)

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

NewsTest$Popular <- 0
News <- rbind(NewsTrain, NewsTest)

News$PubDate = strptime(News$PubDate, "%Y-%m-%d %H:%M:%S")
News$Weekday = as.factor(News$PubDate$wday)
News$Hour = as.factor(News$PubDate$hour)

News$NewsDesk = as.factor(News$NewsDesk)
News$SectionName = as.factor(News$SectionName)
News$SubsectionName = as.factor(News$SubsectionName)

NewsTrain = head(News, nrow(NewsTrain))
NewsTest = tail(News, nrow(NewsTest))
NewsTest$Popular <- NULL

#---------------------------------------------------------------
#Meta
#---------------------------------------------------------------


metaRf <- randomForest(Popular ~ ., data = NewsTrain[,!colnames(NewsTrain) %in% c("UniqueID", "Abstract", "Snippet", "Headline", "PubDate")])
trainMetaRf <- predict(metaRf, type = "response")
prop.table(table(NewsTrain$Popular, trainMetaRf >= 0.5))
testMetaRf <- predict(metaRf, newdata=NewsTest[,!colnames(NewsTest) %in% c("UniqueID", "Abstract", "Snippet", "Headline", "PubDate")], type="response")


metaSubmission <- data.frame(UniqueID = NewsTest$UniqueID, Probability1 = testMetaRf)
metaSubmission[metaSubmission$Probability1 < 0, ]$Probability1 = 0
write.csv(metaSubmission, "MetaSubmission.csv", row.names=FALSE)

#---------------------------------------------------------------
#Headline
#---------------------------------------------------------------

corpusHeadline <- Corpus(VectorSource(News$Headline))
corpusHeadline <- tm_map(corpusHeadline, tolower)
corpusHeadline <- tm_map(corpusHeadline, PlainTextDocument)
corpusHeadline <- tm_map(corpusHeadline, removePunctuation)
corpusHeadline <- tm_map(corpusHeadline, removeWords, stopwords("english"))
corpusHeadline <- tm_map(corpusHeadline, stemDocument)
corpusHeadline <- DocumentTermMatrix(corpusHeadline)
corpusHeadline <- removeSparseTerms(corpusHeadline, 0.99)
corpusHeadline <- as.data.frame(as.matrix(corpusHeadline))
colnames(corpusHeadline) = make.names(colnames(corpusHeadline))

corpusHeadlineTrain <- head(corpusHeadline, nrow(NewsTrain))
corpusHeadlineTest <- tail(corpusHeadline, nrow(NewsTest))
corpusHeadlineTrain$Popular = NewsTrain$Popular

corpusHeadlineRf <- randomForest(Popular ~ ., data = corpusHeadlineTrain)
trainCorpusHeadlineRf <- predict(corpusHeadlineRf, type = "response")
prop.table(table(corpusHeadlineTrain$Popular, trainCorpusHeadlineRf >= 0.5))
testCorpusHeadlineRf <- predict(corpusHeadlineRf, newdata=corpusHeadlineTest, type="response")

HeadlineSubmission <- data.frame(UniqueID = NewsTest$UniqueID, Probability1 = testCorpusHeadlineRf)
HeadlineSubmission[HeadlineSubmission$Probability1 < 0, ]$Probability1 = 0
write.csv(HeadlineSubmission, "HeadlineSubmissionRf.csv", row.names=FALSE)

#---------------------------------------------------------------
#Snippet
#---------------------------------------------------------------

corpusSnippet <- Corpus(VectorSource(News$Snippet))
corpusSnippet <- tm_map(corpusSnippet, tolower)
corpusSnippet <- tm_map(corpusSnippet, PlainTextDocument)
corpusSnippet <- tm_map(corpusSnippet, removePunctuation)
corpusSnippet <- tm_map(corpusSnippet, removeWords, stopwords("english"))
corpusSnippet <- tm_map(corpusSnippet, stemDocument)
corpusSnippet <- DocumentTermMatrix(corpusSnippet)
corpusSnippet <- removeSparseTerms(corpusSnippet, 0.99)
corpusSnippet <- as.data.frame(as.matrix(corpusSnippet))
colnames(corpusSnippet) = make.names(colnames(corpusSnippet))

corpusSnippetTrain <- head(corpusSnippet, nrow(NewsTrain))
corpusSnippetTest <- tail(corpusSnippet, nrow(NewsTest))
corpusSnippetTrain$Popular = NewsTrain$Popular

corpusSnippetRf <- randomForest(Popular ~ ., data = corpusSnippetTrain)
trainCorpusSnippetRf <- predict(corpusSnippetRf, type = "response")
prop.table(table(corpusSnippetTrain$Popular, trainCorpusSnippetRf >= 0.5))
testCorpusSnippetRf <- predict(corpusSnippetRf, newdata=corpusSnippetTest, type="response")

SnippetSubmission <- data.frame(UniqueID = NewsTest$UniqueID, Probability1 = testCorpusSnippetRf)
SnippetSubmission[SnippetSubmission$Probability1 < 0, ]$Probability1 = 0
write.csv(SnippetSubmission, "SnippetSubmissionRf.csv", row.names=FALSE)



#---------------------------------------------------------------
#Abstract
#---------------------------------------------------------------

corpusAbstract <- Corpus(VectorSource(News$Abstract))
corpusAbstract <- tm_map(corpusAbstract, tolower)
corpusAbstract <- tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, stemDocument)
corpusAbstract <- DocumentTermMatrix(corpusAbstract)
corpusAbstract <- removeSparseTerms(corpusAbstract, 0.99)
corpusAbstract <- as.data.frame(as.matrix(corpusAbstract))
colnames(corpusAbstract) = make.names(colnames(corpusAbstract))

corpusAbstractTrain <- head(corpusAbstract, nrow(NewsTrain))
corpusAbstractTest <- tail(corpusAbstract, nrow(NewsTest))
corpusAbstractTrain$Popular = NewsTrain$Popular

corpusAbstractRf <- randomForest(Popular ~ ., data = corpusAbstractTrain)
trainCorpusAbstractRf <- predict(corpusAbstractRf, type = "response")
prop.table(table(corpusAbstractTrain$Popular, trainCorpusAbstractRf >= 0.5))
testCorpusAbstractRf <- predict(corpusAbstractRf, newdata=corpusAbstractTest, type="response")

AbstractSubmission <- data.frame(UniqueID = NewsTest$UniqueID, Probability1 = testCorpusAbstractRf)
AbstractSubmission[AbstractSubmission$Probability1 < 0, ]$Probability1 = 0
write.csv(AbstractSubmission, "AbstractSubmissionRf.csv", row.names=FALSE)

#---------------------------------------------------------------
#Full text
#---------------------------------------------------------------

News$FullText <- paste(News$Headline, News$Snippet, News$Abstract)
corpusFull <- Corpus(VectorSource(News$FullText))
corpusFull <- tm_map(corpusFull, tolower)
corpusFull <- tm_map(corpusFull, PlainTextDocument)
corpusFull <- tm_map(corpusFull, removePunctuation)
corpusFull <- tm_map(corpusFull, removeWords, stopwords("english"))
corpusFull <- tm_map(corpusFull, stemDocument)
corpusFull <- DocumentTermMatrix(corpusFull)
corpusFull <- removeSparseTerms(corpusFull, 0.99)
corpusFull <- as.data.frame(as.matrix(corpusFull))
colnames(corpusFull) = make.names(colnames(corpusFull))

corpusFullTrain <- head(corpusFull, nrow(NewsTrain))
corpusFullTest <- tail(corpusFull, nrow(NewsTest))
corpusFullTrain$Popular = NewsTrain$Popular

corpusFullRf <- randomForest(Popular ~ ., data = corpusFullTrain)
trainCorpusFullRf <- predict(corpusFullRf, type = "response")
prop.table(table(corpusFullTrain$Popular, trainCorpusFullRf >= 0.5))
testCorpusFullRf <- predict(corpusFullRf, newdata=corpusFullTest, type="response")

FullSubmission <- data.frame(UniqueID = NewsTest$UniqueID, Probability1 = testCorpusFullRf)
FullSubmission[FullSubmission$Probability1 < 0, ]$Probability1 = 0
write.csv(FullSubmission, "FullSubmissionRf.csv", row.names=FALSE)

#---------------------------------------------------------------
testEnsemble <- testMetaRf * 0.9 + testCorpusFullRf * 0.1
EnsembleSubmission <- data.frame(UniqueID = NewsTest$UniqueID, Probability1 = testEnsemble)
EnsembleSubmission[EnsembleSubmission$Probability1 < 0, ]$Probability1 = 0
write.csv(EnsembleSubmission, "EnsembleSubmission.csv", row.names=FALSE)
