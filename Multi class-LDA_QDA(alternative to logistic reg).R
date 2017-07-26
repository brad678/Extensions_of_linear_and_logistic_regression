
#loading the data

library (ISLR)
names(Smarket)

dim(Smarket)

summary (Smarket)

attach (Smarket)

#create train & test

train =(Year<2005) #boolean vector
Smarket.2005=Smarket[! train ,] #test data
dim(Smarket.2005)
Direction.2005= Direction [! train]

#LDA

library (MASS)
lda.fit=lda(Direction???Lag1+Lag2 ,data=Smarket ,subset =train)
lda.fit

par(mar=c(1,1,1,1))  #if we dont do this we get error as "figure margins too large"
plot(lda.fit)


lda.pred=predict(lda.fit , Smarket.2005) #predict on test data
names(lda.pred) #threshold can be adjusted using posterior. Default threshold is 0.5

library(caret)
confusionMatrix(lda.pred$class,Direction.2005)

sum(lda.pred$posterior [ ,1] >=.5) #same as values from confusion matrix for "predicted" - Down
sum(lda.pred$posterior [,1]<.5) #same as values from confusion matrix for "predicted" - Up


#QDA

qda.fit=qda(Direction???Lag1+Lag2 ,data=Smarket ,subset =train)
qda.fit

qda.pred=predict(qda.fit , Smarket.2005) #predict on test data
names(qda.pred) #threshold can be adjusted using posterior. Default threshold is 0.5

confusionMatrix(qda.pred$class,Direction.2005)

sum(qda.pred$posterior [ ,1] >=.5) #same as values from confusion matrix for "predicted" - Down
sum(qda.pred$posterior [,1]<.5) #same as values from confusion matrix for "predicted" - Up


