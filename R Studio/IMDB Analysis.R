setwd("D:/RLabs")
imdb<-read.csv("D:/RLabs/group/IMDb movies.csv",  header=TRUE)
colnames(imdb)

dimdb<-allimdb
colnames(dimdb)


dimdb <- dimdb[setdiff(colnames(dimdb), c("imdb_title_id", 
                                          "title",
                                          "date_published",
                                          "country",
                                          "language",
                                          "writer",
                                          "production_company",
                                          "actors",
                                          "description",
                                          "metascore", 
                                          "director"
                                          ))]
            

imdb$genre <- gsub(",.*$", "", imdb$genre)
summary(imdb$actors)
imdb$actors <- gsub(",.*$", "", imdb$actors)

imdb$genre<-as.factor(imdb$genre)

levels(imdb$genre) <- gsub("Adult","other", levels(imdb$genre))
levels(imdb$genre) <- gsub("Documentary","other", levels(imdb$genre))
levels(imdb$genre) <- gsub("Sport","other", levels(imdb$genre))
levels(imdb$genre) <- gsub("History","other", levels(imdb$genre))
levels(imdb$genre) <- gsub("Film-Noir","other", levels(imdb$genre))
levels(imdb$genre) <- gsub("War","other", levels(imdb$genre))
levels(imdb$genre) <- gsub("Music","other", levels(imdb$genre))
levels(imdb$genre) <- gsub("Musical","other", levels(imdb$genre))

summary(imdb$genre)

#drop rows with 

mean(imdb$avg_vote)

imdb$usa<-ifelse(imdb$country=="USA",1,NA)

#imdb<-imdb[complete.cases(imdb$usa),]

imdb$budget = as.numeric(gsub("[\\$,]", "", imdb$budget))

#imdb$budget<-formatC(imdb$budget, format = "f", digits = 0)

#imdb<-imdb[complete.cases(imdb$budget),]

imdb$usa_gross_income = as.numeric(gsub("[\\$,]", "", imdb$usa_gross_income))
imdb<-imdb[complete.cases(imdb$usa_gross_income),]

imdb$worlwide_gross_income = as.numeric(gsub("[\\$,]", "", imdb$worlwide_gross_income))
imdb<-imdb[complete.cases(imdb$worlwide_gross_income),]

imdb$actors<-as.factor(imdb$actors)

summary(imdb$production_company)

setwd("D:/RLabs")
ratings<-read.csv("D:/RLabs/group/IMDb ratings.csv",  header=TRUE)
colnames(ratings)

allimdb<-merge(imdb,ratings, by="imdb_title_id")

colnames(allimdb)

averagescore<-mean(allimdb$avg_vote)

allimdb$goodmovie<-ifelse(imdb$avg_vote>averagescore,1,0)

#delete NA


#training and testing

data_train <- allimdb[1:floor(0.8 * nrow(allimdb)),]
data_test <- allimdb[(floor(0.8 * nrow(allimdb))+1):nrow(allimdb),]

############################   model ########################################
#part3 
options(scipen=999)

colnames(imdb)
res<-lm(usa_gross_income ~ 
          genre+
          duration+
          avg_vote+
          votes+
          budget+
          reviews_from_users+
          reviews_from_critics, 
          data=data_train)

summary(res)

modelprediction<-predict(res,data=data_test)

#fixed
resfixed<-lm(usa_gross_income~ 
          duration+avg_vote+votes+budget
        +reviews_from_users, data=data_train)
options(scipen=999)
summary(resfixed)


library(Metrics)
#result<-rmse(data_test$usa_gross_income, modelprediction)


reviewer<-allimdb

reviewer<-reviewer[complete.cases(reviewer$duration),]
reviewer<-reviewer[complete.cases(reviewer$avg_vote),]
reviewer<-reviewer[complete.cases(reviewer$reviews_from_users),]
reviewer<-reviewer[complete.cases(reviewer$reviews_from_critics),]
reviewer<-reviewer[complete.cases(reviewer$votes),]
reviewer<-reviewer[complete.cases(reviewer$budget),]
reviewer<-reviewer[complete.cases(reviewer$genre),]


library(leaps)

y = reviewer$usa_gross_income
x = cbind(reviewer$genre, reviewer$duration, 
          reviewer$avg_vote, reviewer$votes, reviewer$budget, 
          reviewer$reviews_from_users, reviewer$reviews_from_critics)
out = leaps(x,y,method="Cp", nbest = 1)
print(out)
#FALSE  TRUE  TRUE  TRUE TRUE  TRUE  TRUE
#q2
library(lars)
y = reviewer$usa_gross_income
x = cbind(reviewer$genre, reviewer$duration, 
          reviewer$avg_vote, reviewer$votes, reviewer$budget, 
          reviewer$reviews_from_users, reviewer$reviews_from_critics)
res = lars(x, y, type="stepwise")
print(summary(res))
res
#5 4 6 2 3

#fixed regression
resfixed<-lm(usa_gross_income ~ 
               duration+
               avg_vote+
               votes+budget+
               reviews_from_users, 
               data=data_train)

summary(resfixed)


#####decision tree allgenders avg_vote
allimdb$y<-allimdb$goodmovie

data_train <- allimdb[1:floor(0.8 * nrow(allimdb)),]
data_test <- allimdb[(floor(0.8 * nrow(allimdb))+1):nrow(allimdb),]

colnames(ratings)

data_train$y<-as.factor(data_train$y)
data_test$y<-as.factor(data_test$y)

library(rpart)

tfit <- rpart(y ~ 
                allgenders_0age_avg_vote + 
                allgenders_18age_avg_vote+ 
                allgenders_30age_avg_vote+
                allgenders_45age_avg_vote, 
              data = data_train, method = 'class'
              , parms = list(split = 'information'), 
              control=rpart.control(cp=0.0001))




plot(tfit, uniform=TRUE)
text(tfit)

#f1 for training
myf1score <- function(pred, actual) {
  res = table(pred, actual)
  precision = res[2,2]/(res[2,2]+res[2,1])
  recall = res[2,2]/(res[2,2]+res[1,2])
  f1 = 2/(1/precision+1/recall)
  return(f1)
}


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
tfit_pruned = prune(tfit, cp=0.00020109)
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
rffit <- randomForest(y ~ allgenders_0age_avg_vote + allgenders_18age_avg_vote
                      + allgenders_30age_avg_vote+allgenders_45age_avg_vote, data = data_train, importance = TRUE, na.action=na.omit, ntree = 50)
plot(rffit)
print(rffit)
varImpPlot(rffit)



table(predict(rffit, data_train), data_train$y)

cbind(pred, data_test$y)

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


###########
#####decision tree allgenders_0age_votes
allimdb$y<-allimdb$goodmovie

colnames(ratings)

library(rpart)

tfit <- rpart(y ~ allgenders_0age_votes + allgenders_18age_votes
              + allgenders_30age_votes+allgenders_45age_votes, data = data_train, method = 'class', parms = list(split = 'information'), 
              control=rpart.control(cp=0.0001))
plot(tfit, uniform=TRUE)
text(tfit)

#f1 for training
myf1score <- function(pred, actual) {
  res = table(pred, actual)
  precision = res[2,2]/(res[2,2]+res[2,1])
  recall = res[2,2]/(res[2,2]+res[1,2])
  f1 = 2/(1/precision+1/recall)
  return(f1)
}


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
tfit_pruned = prune(tfit, cp=  0.00073731)
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
rffit <- randomForest(y ~ allgenders_0age_votes + allgenders_18age_votes
                      + allgenders_30age_votes+allgenders_45age_votes, data = data_train, importance = TRUE, na.action=na.omit, ntree = 50)
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

###########
###########
#####decision tree males
allimdb$y<-allimdb$goodmovie

colnames(ratings)

library(rpart)

tfit <- rpart(y ~ males_0age_avg_vote + males_0age_avg_vote
              + males_18age_avg_vote+males_30age_avg_vote
              +males_45age_avg_vote
              +females_allages_avg_vote+females_0age_avg_vote
              +females_18age_avg_vote+females_30age_avg_vote
              +females_45age_avg_vote, data = data_train, method = 'class', parms = list(split = 'information'), 
              control=rpart.control(cp=0.0001))
plot(tfit, uniform=TRUE)
text(tfit)

#f1 for training
myf1score <- function(pred, actual) {
  res = table(pred, actual)
  precision = res[2,2]/(res[2,2]+res[2,1])
  recall = res[2,2]/(res[2,2]+res[1,2])
  f1 = 2/(1/precision+1/recall)
  return(f1)
}


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
tfit_pruned = prune(tfit, cp=0.00050271)
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
rffit <- randomForest(y~ males_0age_avg_vote + males_0age_avg_vote
                      + males_18age_avg_vote+males_30age_avg_vote
                      +males_45age_avg_vote
                      +females_allages_avg_vote+females_0age_avg_vote
                      +females_18age_avg_vote+females_30age_avg_vote
                      +females_45age_avg_vote, data = data_train, importance = TRUE, na.action=na.omit, ntree = 50)
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


###########
###########
#####decision tree top 10000
allimdb$y<-allimdb$goodmovie

colnames(ratings)

library(rpart)

tfit <- rpart(y ~ top1000_voters_rating + top1000_voters_votes
              + us_voters_rating+us_voters_votes
              +non_us_voters_rating
              +non_us_voters_votes, data = data_train, method = 'class', parms = list(split = 'information'), 
              control=rpart.control(cp=0.0001))
plot(tfit, uniform=TRUE)
text(tfit)

#f1 for training
myf1score <- function(pred, actual) {
  res = table(pred, actual)
  precision = res[2,2]/(res[2,2]+res[2,1])
  recall = res[2,2]/(res[2,2]+res[1,2])
  f1 = 2/(1/precision+1/recall)
  return(f1)
}


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
tfit_pruned = prune(tfit, cp=0.00124533)
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
rffit <- randomForest(y~ top1000_voters_rating + top1000_voters_votes
                      + us_voters_rating+us_voters_votes
                      +non_us_voters_rating
                      +non_us_voters_votes, data = data_train, importance = TRUE, na.action=na.omit, ntree = 50)
plot(rffit)
print(rffit)
varImpPlot(rffit)

rffit$confusion

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


###########
###########
#########
#####decision tree combined

allimdb$y<-allimdb$goodmovie

colnames(ratings)

library(rpart)

tfit <- rpart(y ~ top1000_voters_rating + top1000_voters_votes
              + us_voters_rating+us_voters_votes
              +non_us_voters_rating
              +non_us_voters_votes+
                allgenders_0age_avg_vote + allgenders_18age_avg_vote
              + allgenders_30age_avg_vote+allgenders_45age_avg_vote, data = data_train, method = 'class', parms = list(split = 'information'), 
              control=rpart.control(cp=0.0001))
plot(tfit, uniform=TRUE)
text(tfit)

#f1 for training
myf1score <- function(pred, actual) {
  res = table(pred, actual)
  precision = res[2,2]/(res[2,2]+res[2,1])
  recall = res[2,2]/(res[2,2]+res[1,2])
  f1 = 2/(1/precision+1/recall)
  return(f1)
}


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
tfit_pruned = prune(tfit, cp=0.00155666)
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
rffit <- randomForest(y~ top1000_voters_rating + top1000_voters_votes
                      + us_voters_rating+us_voters_votes
                      +non_us_voters_rating
                      +non_us_voters_votes+
                      allgenders_0age_avg_vote + allgenders_18age_avg_vote
                      + allgenders_30age_avg_vote+allgenders_45age_avg_vote, data = data_train,  importance = TRUE, na.action=na.omit, ntree = 50)
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


########knn

reviewer$y<-as.numeric(reviewer$goodmovie)
rtrain <- reviewer[1:floor(0.8 * nrow(reviewer)),]
rtest <- reviewer[(floor(0.8 * nrow(reviewer))+1):nrow(reviewer),]

library(class)


train = cbind(rtrain$top1000_voters_rating, 
              rtrain$top1000_voters_votes, 
              rtrain$us_voters_rating, 
              rtrain$us_voters_votes,
              rtrain$non_us_voters_rating, 
              rtrain$non_us_voters_votes)

test = cbind(rtest$top1000_voters_rating, 
             rtest$top1000_voters_votes, 
             rtest$us_voters_rating, 
             rtest$us_voters_votes,
             rtest$non_us_voters_rating, 
             rtest$non_us_voters_votes)
#Y: popular
cl = rtrain$y
cl_test = rtest$y

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

# k for testing
err_by_k = rep(0, 10)
for(k in 1:10) {
  res = knn(train, test, cl, k = k, prob=TRUE)
  err_by_k[k] = sum(abs(as.numeric(res)-1-cl_test))
}
cbind(seq(1,10), err_by_k)

###f1 score for knn
k = 1
restrain = knn(train, train, cl, k = k, prob=TRUE)
#predict vs. true
cbind(as.numeric(restrain)-1, cl)
#error count
err = sum(abs(as.numeric(restrain)-1-cl))
err

#f1 score trainging 


