setwd("D:/RLabs")
reviewer<-read.csv("D:/RLabs/reviewer_withavg.csv",  header=TRUE)
colnames(reviewer)
setwd("D:/RLabs")
bank<-read.csv("D:/RLabs/bank-full-shuffled.csv",  header=TRUE, sep=";")
colnames(bank)

#install library class, klaR, e1071, rpart, 
#randomForest, leaps, lars, MASS.
install.packages("class")
library(class)
install.packages("klaR")
library(klaR)
install.packages("e1071")
library(e1071)
install.packages("rpart")
library(rpart)
install.packages("randomForest")
library(randomForest)
install.packages("leaps")
library(leaps)
install.packages("lars")
library(lars)
install.packages("MASS")
library(MASS)

#part 1 
reviewer$popular<-ifelse(reviewer$Top10==1|
                         reviewer$Top50==1|
                         reviewer$Top100==1
                         ,1,0)

test<-reviewer[1:50,]
train<-reviewer[51:199,]

#question1
#X: only use these variables as input: 

trainq1=cbind(train$avg_centrality, train$avg_content, 
        train$avg_viewership, train$avg_enhcontent)
testq1=cbind(test$avg_centrality, test$avg_content, 
              test$avg_viewership, test$avg_enhcontent)
#Y: popular
cl_trainnumber= train$popular
cl_testnumber= test$popular


library(class)
#train
err_by_k1 = rep(0, 10)
for(k in 1:10) {
  res = knn(trainq1, trainq1, cl_trainnumber, k = k, prob=TRUE)
  err_by_k1[k] = sum(abs(as.numeric(res)-1-cl_trainnumber))
  
}
cbind(seq(1,10), err_by_k1)

#test
library(class)
err_by_k2 = rep(0, 10)
for(k in 1:10) {
  res = knn(trainq1, testq1, cl_trainnumber, k = k, prob=TRUE)
  err_by_k2[k] = sum(abs(as.numeric(res)-1-cl_testnumber))
  
}
cbind(seq(1,10), err_by_k2)


#Q3
library(klaR)
cl_trainfactor <- as.factor(cl_trainnumber)
cl_testfactor<-as.factor(cl_testnumber)

train$popularfactor<-as.factor(train$popular)
test$popularfactor<-as.factor(test$popular)

res1=NaiveBayes(popularfactor~avg_viewership+avg_content+avg_centrality+avg_enhcontent+avg_content, data=train)
pred2<-predict(res1,test)
#predict vs. true
cbind(as.numeric(pred2$class)-1, as.numeric(test$popular)-1)
#error count
err3 = sum(abs(as.numeric(pred2$class)-as.numeric(test$popular)))
err3

#Q4
library(e1071)

#liner for training 
res4 = svm(trainq1, as.factor(train$popular), kernel = "linear")
pred4 = predict(res4, trainq1)
#predict vs. true
cbind(as.numeric(pred4)-1, train$popular)
err4 = sum(abs(as.numeric(pred4)-1-train$popular))
err4

#polynomial for training
res4.2 = svm(trainq1, as.factor(train$popular), kernel = "polynomial")
pred4.2 = predict(res4.2, trainq1)
#predict vs. true
cbind(as.numeric(pred4.2)-1, train$popular)
err4.2 = sum(abs(as.numeric(pred4.2)-1-train$popular))
err4.2

#linear for testing
res4.3 = svm(trainq1, as.factor(train$popular), kernel = "linear")
pred4.3 = predict(res4.3, testq1)
#predict vs. true
cbind(as.numeric(pred4.3)-1, test$popular)
err4.3 = sum(abs(as.numeric(pred4.3)-1-test$popular))
err4.3

#polynomial for testing
res4.4 = svm(trainq1, as.factor(train$popular), kernel = "polynomial")
pred4.4 = predict(res4.4, testq1)
#predict vs. true
cbind(as.numeric(pred4.4)-1, test$popular)
err4.4 = sum(abs(as.numeric(pred4.4)-1-test$popular))
err4.4
##########################
setwd("D:/RLabs")
bankfull<-read.csv("D:/RLabs/bank-full.csv",  header=TRUE, sep=";")
colnames(bankfull)
bankfulltrain <- bankfull[1:floor(0.8 * nrow(bankfull)),]
bankfulltest <- bankfull[(floor(0.8 * nrow(bankfull))+1):nrow(bankfull),]

library(rpart)
tfit <- rpart(y ~ duration + month + 
                poutcome+job+education+marital,data = bankfulltrain, 
              method = 'class', 
              parms = list(split = 'information'),
              control=rpart.control(cp=0.0005))
print(tfit)
plot(tfit, uniform=TRUE)
text(tfit)

tablefulltrain<-table(predict(tfit,bankfulltrain,type="class"),bankfulltrain$y)
tabletrain
tablefulltest<-table(predict(tfit,bankfulltest,type="class"),bankfulltest$y)
tabletest


###################################
#part2
#q5
banktrain <- bank[1:floor(0.8 * nrow(bank)),]
banktest <- bank[(floor(0.8 * nrow(bank))+1):nrow(bank),]

library(rpart)
tfit <- rpart(y ~ duration + month + 
                   poutcome+job+education+marital,data = banktrain, 
             method = 'class', 
             parms = list(split = 'information'),
control=rpart.control(cp=0.0001))
print(tfit)
plot(tfit, uniform=TRUE)
text(tfit)

tabletrain<-table(predict(tfit,banktrain,type="class"),banktrain$y)
tabletrain
tabletest<-table(predict(tfit,banktest,type="class"),banktest$y)
tabletest

precision1 = tabletrain[2,2]/(tabletrain[2,2]+tabletrain[2,1])
recall1 = tabletrain[2,2]/(tabletrain[2,2]+tabletrain[1,2])
f1train = 2/(1/precision1+1/recall1)
print(f1train)
 
precision2 = tabletest[2,2]/(tabletest[2,2]+tabletest[2,1])
recall2 = tabletest[2,2]/(tabletest[2,2]+tabletest[1,2])
f1test = 2/(1/precision2+1/recall2)
print(f1test)

printcp(tfit)
plotcp(tfit)
#0.00189663
tfit_pruned = prune(tfit, cp=0.00189663)
plot(tfit_pruned, uniform=TRUE)
text(tfit_pruned)

ptabletrain<-table(predict(tfit_pruned, banktrain, type="class"), banktrain$y)
ptabletrain
ptabletest<-table(predict(tfit_pruned, banktest, type="class"), banktest$y)
ptabletest

precision3 = ptabletrain[2,2]/(ptabletrain[2,2]+ptabletrain[2,1])
recall1 = ptabletrain[2,2]/(ptabletrain[2,2]+ptabletrain[1,2])
pf1train = 2/(1/precision3+1/recall1)
print(pf1train)

precision4 = ptabletest[2,2]/(ptabletest[2,2]+ptabletest[2,1])
recall2 = ptabletest[2,2]/(ptabletest[2,2]+ptabletest[1,2])
pf1test = 2/(1/precision2+1/recall2)
print(pf1test)


#random forest
library(randomForest)
rffit <- randomForest(y ~ duration + month + 
                         poutcome+job+education+marital
                      , data = banktrain, importance = TRUE, 
                      na.action=na.omit, ntree = 50)

print(rffit)
plot(rffit)



table(predict(rffit, banktrain), banktrain$y)
#check testing
table(predict(rffit, banktest), banktest$y)


#F1 score
rtabletrain<-table(predict(rffit, banktrain, type="class"), banktrain$y)
rtabletest<-table(predict(rffit, banktest, type="class"), banktest$y)

precision5 = rtabletrain[2,2]/(rtabletrain[2,2]+rtabletrain[2,1])
recall1 = rtabletrain[2,2]/(rtabletrain[2,2]+rtabletrain[1,2])
rf1train = 2/(1/precision5+1/recall1)
print(rf1train)

precision6 = rtabletest[2,2]/(rtabletest[2,2]+rtabletest[2,1])
recall2 = rtabletest[2,2]/(rtabletest[2,2]+rtabletest[1,2])
rf1test = 2/(1/precision6+1/recall2)
print(rf1test)

varImpPlot(rffit)



#part3 

res = lm(avg_content~avg_centrality+avg_viewership+avg_enhcontent+Top10+Top50+Top100+Advisor+Lead, data=reviewer)
summary(res)

library(leaps)

y = reviewer$avg_content
x = cbind(reviewer$avg_centrality, reviewer$avg_viewership, 
          reviewer$avg_enhcontent, reviewer$Top10, reviewer$Top50, 
          reviewer$Top100, reviewer$Advisor, reviewer$Lead)
out = leaps(x,y,method="Cp", nbest = 1)
print(out)


#q2
library(lars)
y = reviewer$avg_content
x = cbind(reviewer$avg_centrality, reviewer$avg_viewership, 
          reviewer$avg_enhcontent,reviewer$Top10, reviewer$Top50, 
          reviewer$Top100, reviewer$Advisor, reviewer$Lead)
res = lars(x, y, type="stepwise")
print(summary(res))
res


###########################################################
#part2
setwd("D:/RLabs")
data<-read.csv("D:/RLabs/bank-full-shuffled.csv",  header=TRUE, sep=";")
colnames(data)


#grab the first 70% of rows as training data and the remaining as testing
data_train <- data[1:floor(0.8 * nrow(data)),]

data_test <- data[(floor(0.8 * nrow(data))+1):nrow(data),]



#q1 decision 
library(rpart)

tfit <- rpart(y ~ duration + month + poutcome+job+education+marital, data = data_train, method = 'class', parms = list(split = 'information'), 
              control=rpart.control(cp=0.0001))
plot(tfit, uniform=TRUE)
text(tfit)

## Actual Values in Columns, predictions in rows
table(predict(tfit, data_train, type="class"), data_train$y)
#predict on the testing data

tpred = predict(tfit, data_train, type="class")
table(tpred, data_train$y)

pred = predict(tfit, data_test, type="class")
table(pred, data_test$y)

#f1 for training
res = table(tpred, data_train$y)
precision = res[2,2]/(res[2,2]+res[2,1])
recall = res[2,2]/(res[2,2]+res[1,2])
precision
recall
myf1score(tpred, data_train$y)

#f1 for testing

pred = predict(tfit, data_test, type="class")
table(pred, data_test$y)

res = table(pred, data_test$y)
precision = res[2,2]/(res[2,2]+res[2,1])
recall = res[2,2]/(res[2,2]+res[1,2])
precision
recall
myf1score(pred, data_test$y)







#how well does it do?
printcp(tfit)
plotcp(tfit)
# Prune
tfit_pruned = prune(tfit, cp=0.00165955)
plot(tfit_pruned, uniform=TRUE)
text(tfit_pruned)
## Actual Values in Columns, predictions in rows
table(predict(tfit_pruned, data_train, type="class"), data_train$y)
#predict on the testing data
table(predict(tfit_pruned, data_test, type="class"), data_test$y)

#f1 score
tpred = predict(tfit_pruned, data_train, type="class")
table(tpred, data_train$y)

pred = predict(tfit_pruned, data_test, type="class")
table(pred, data_test$y)

#f1 for training
res = table(tpred, data_train$y)
precision = res[2,2]/(res[2,2]+res[2,1])
recall = res[2,2]/(res[2,2]+res[1,2])
precision
recall
myf1score(tpred, data_train$y)

#f1 for testing

pred = predict(tfit_pruned, data_test, type="class")
table(pred, data_test$y)

res = table(pred, data_test$y)
precision = res[2,2]/(res[2,2]+res[2,1])
recall = res[2,2]/(res[2,2]+res[1,2])
precision
recall
myf1score(pred, data_test$y)








library(randomForest)
rffit <- randomForest(y ~ duration + month + poutcome+job+education+marital, data = data_train, importance = TRUE, na.action=na.omit, ntree = 50)
plot(rffit)
print(rffit)
varImpPlot(rffit)

table(predict(rffit, data_train), data_train$y)


#check testing data
table(predict(rffit, data_test), data_test$y)


pred = predict(rffit, data_test, type="class")
table(pred, data_test$y)


#
#f1 score
tpred = predict(rffit, data_train, type="class")
table(tpred, data_train$y)

pred = predict(rffit, data_test, type="class")
table(pred, data_test$y)

#f1 for training
res = table(tpred, data_train$y)
precision = res[2,2]/(res[2,2]+res[2,1])
recall = res[2,2]/(res[2,2]+res[1,2])
precision
recall
myf1score(tpred, data_train$y)

#f1 for testing

pred = predict(rffit, data_test, type="class")
table(pred, data_test$y)

res = table(pred, data_test$y)
precision = res[2,2]/(res[2,2]+res[2,1])
recall = res[2,2]/(res[2,2]+res[1,2])
precision
recall
myf1score(pred, data_test$y)



#NB
train$popularfactor<-as.factor(train$popular)
test$popularfactor<-as.factor(test$popular)

library(klaR)
res=NaiveBayes(popularfactor~avg_viewership+avg_content+avg_centrality+avg_enhcontent, data=train)
pred = predict(res, test)

#predict vs. true
cbind(as.numeric(pred$class)-1, as.numeric(test$popularfactor)-1)
#error count
err = sum(abs(as.numeric(pred$class)-as.numeric(test$popularfactor)))
err


#knn 


data_train<-reviewer[1:50,]
data_test<-reviewer[51:199,]

library(class)

train = cbind(data_train$avg_viewership, data_train$avg_content, data_train$avg_centrality, data_train$avg_enhcontent)
test = cbind(data_test$avg_viewership, data_test$avg_content, data_test$avg_centrality, data_test$avg_enhcontent)
#Y: popular
cl = data_train$popular
cl_test = data_test$popular

#k-NN
k = 5
res = knn(train, test, cl, k = k, prob=TRUE)
#predict vs. true
cbind(as.numeric(res)-1, cl_test)
#error count
err = sum(abs(as.numeric(res)-1-cl_test))
err


#var k training

err_by_k = rep(0, 10)
for(k in 1:10) {
  res = knn(train, train, cl, k = k, prob=TRUE)
  err_by_k[k] = sum(abs(as.numeric(res)-1-cl))
}
cbind(seq(1,10), err_by_k)

#vary k
err_by_k = rep(0, 10)
for(k in 1:10) {
  res = knn(train, test, cl, k = k, prob=TRUE)
  err_by_k[k] = sum(abs(as.numeric(res)-1-cl_test))
}
cbind(seq(1,10), err_by_k)


