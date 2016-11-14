
enron=read.csv("energy_bids.csv")
str(enron)
enron=read.csv("energy_bids.csv",stringsAsFactors = FALSE)
str(enron)
enron$email[1]
strwrap(enron$email[1])
enron$responsive[1]
enron$responsive[2]
strwrap(enron$email[2])
table(enron$responsive)
library(tm)
library(SnowballC)
corpus=tm_map(corpus,PlainTextDocument)
corpus=tm_map(corpus,PlainTextDocument)
corpus=tm_map(corpus,tolower)
corpus=tm_map(enron,tolower)
corpus=Corpus(VectorSource(enron$email))
corpus=tm_map(corpus,PlainTextDocument)
corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,stemDocument)
strwarp(corpus[[1]])
strwrap(corpus[[1]])
dtm=DocumentTermMatrix(corpus)
dtm
corpus=tm_map(corpus,PlainTextDocument)
dtm=DocumentTermMatrix(corpus)
dtm
dtm=removeSparseTerms(dtm,0.97)
dtm
labeldataframe=as.data.frame(as.matrix(dtm))
labeldataframe$responsive=enron$reponsive
str(labeldataframe)
library(caTools)
set.seed(144)
split=sample.split(labeldataframe$reponsive,SplitRatio = 0.7)
split=sample.split(labeldataframe$reponsive,0.7)
labeldataframe$responsive=enron$reponsive
library(caTools)
split=sample.split(labeldataframe$reponsive,0.7)
labeldataframe$Responsive=enron$reponsive
library(caTools)
labeldataframe$responsive=enron$responsive
library(caTools)
split=sample.split(labeldataframe$responsive,0.7)
train=subset(labeldataframe,split==TRUE)
test=subset(labeldataframe,split==FALSE)
library(rpart)
library(rpart.plot)
emailCart=rpart(reponsive ~.,data=train,method="class")
emailCart=rpart(responsive ~.,data=train,method="class")
prp(emailCart)
pred=predict(emailCart,newdata=test)
pred[1:10,]
pred.prob=pred[,2]
table(test$responsive,pred.prob >=0.5)
199+17/199+17+16+25
199+17/(199+17+16+25)
(199+17)/(199+17+16+25)
table(test$responsive)
215/(215+42)
liibrary(ROCR)
library(ROCR)
predROCR=prediction(pred.prob,test$responsive)
perfROCR=performance(predROCR,"tpr","fpr")
plot(perfROCR,colorize=TRUE)
performance(predROCR,"auc")@y.values
