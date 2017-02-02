library(kernlab)
library(caret)
data(spam)
inTrain <- createDataPartition(y=spam$type,p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

tc <- (spam$type=="spam")*1
pcp <- prcomp(log10(spam[ ,-58]+1))

prepoc <- preProcess(log10(spam[,-58]+1))
trainpc <-predict(prepoc,log10(training[,-58]+1))
modelfit <- train(training$type~.,method ="glm", trainpc)
trainpc <-predict(prepoc,log10(testing[,-58]+1))
confusionMatrix(testing$type, predict(modelfit, testpc))

modelfit <- train(training$type~., method="glm", preProcess="pca", data=training)

d <- segmentationOriginal
training <- d[as.numeric(d$Case)==1,]
testing <- d[as.numeric(d$Case)==2,]

set.seed(125)
cart_model <- train(Class~.,data=training, method="rpart")

a <- data.frame(TotalIntenCh2 = 23000, FiberWidthCh1= 10, PerimStatusCh1=2) 
predict(cart_model,a, type="prob")

library(rpart.plot)


set.seed(13234)
mod <- train(chd~age+alcohol+obesity+tobacco+typea+ldl,data=trainSA, method="glm", family="binomial")

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd, predict(mod,trainSA[ ,-1]))
missClass(testSA$chd, predict(mod,testSA[ ,-1]))








