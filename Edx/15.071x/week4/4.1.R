gerber <- read.csv("gerber.csv")
prop.table(table(gerber$voting))

cd <- gerber[gerber$civicduty == 1, ]
ht <- gerber[gerber$hawthorne == 1, ]
self <- gerber[gerber$self == 1, ]
nb <- gerber[gerber$neighbors == 1, ]

prop.table(table(cd$voting))
prop.table(table(ht$voting))
prop.table(table(self$voting))
prop.table(table(nb$voting))

log <- glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = "binomial")
summary(log)

pred <- predict(log, type = "response")
prop.table(table(gerber$voting, pred > 0.3))
0.3909307 + 0.1510271

prop.table(table(gerber$voting, pred > 0.5))
0.6841004

prediction <- prediction(predictions = pred, labels = gerber$voting)
perf <- performance(prediction, measure = "auc")
plot(perf)
as.numeric(performance(prediction, "auc")@y.values)







CARTmodel <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 <- rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

CARTmodel4 <- rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)
0.34 - 0.296638

CARTmodel5 <- rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits = 6)
(0.290456 - 0.334176) - (0.302795 - 0.345818)

LogModelSex <- glm(voting ~ sex + control, data = gerber, family = "binomial")
summary(LogModelSex)



Possibilities <- data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")
0.290456 - 0.2908065

LogModel2 <- glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)
predict(LogModel2, newdata=Possibilities, type="response")
0.290456 - 0.2904558
