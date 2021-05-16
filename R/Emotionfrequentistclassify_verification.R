rm(list=ls(all=T))
dev.off()

##Ajani Blackwood

##This is the script used to crosscheck with Python's Scikit learn


##Misclassification probabilities will be shown at the end but note that they
##can change depending on the intiai data shuffling and stratified sampling
##run the script A couple times and the numbers will look similare to the 
##results from scikit learn


require("randomForest")
require("MASS")
require("SciViews")
require("e1071")
require("rpart")
require("nnet")

##We are running with AllsexesSadHappy.csv but feel free to try out the
##other datasets and modify this script as desired

setwd('C:/Users/ajani/Documents/Bayesian ML/model data/')

Emotion_in <- read.csv("AllsexesSadHappy.csv")
#Emotion_in = Emotion_in[-c(2)]#comment this line if using the others

##############Data Cleaning

#Keeping AUs only for the top face
#Comment the below two lines if you do not want to test only top face
keepcolstopface = c(1,3,4,5,7,9,19)
Emotion_in = Emotion_in[keepcolstopface]

#Uncomment the below lines if you whish to test AUs suggested by Ekman
#Keeping AUs suggesed by Paul Ekman
#keepcolswholeface = c(1,3,11,5,7,13)
#Emotion_in = Emotion_in[keepcolswholeface]


#Make Emotion a factor variable
Emotion_in$Emotion = as.factor(Emotion_in$Emotion)

########marking the classes
#H for Happy
#S for Sad
levels(Emotion_in$Emotion)=c("H","S")

# shuffle the data randomly, so you don't get clunks of class H and S
shuf_ind=sample(seq(1,length(Emotion_in[,1])), replace=FALSE)
Emotion_in=Emotion_in[shuf_ind,]  

# raw data statistics for 2 classes and 6 variables
library(MASS)
parcoord(Emotion_in[,-1],col=rainbow(2)[Emotion_in[,1]]) # parallel coordinate plot
library(SciViews) # pairwised comparison
pairs(Emotion_in[,-1],col=rainbow(2)[Emotion_in[,1]], diag.panel=panel.hist)

###########################################################################################



###############stratified sampling to create training and test data (will not be used to build the model at all)

tot_pts = length(Emotion_in[,1])#full sequence
small_pts = 0.1*tot_pts#10% of the data length
maj_pts = 0.9*tot_pts#90% of the data length

#Emotion_intest = Emotion_in[seq(1,25),]*NA #10% test data
#Emotion_intrain = Emotion_in[seq(1,230),]*NA #90% training data
Emotion_intest = Emotion_in[seq(1,40),]*NA #20% test data
Emotion_intrain = Emotion_in[seq(1,157),]*NA #80% training data
nst = 1
nst_train = 1
for (cl in c("H","S")){
  indx = which(Emotion_in[,1] == cl,arr.ind=TRUE)
  Emotion_incl = Emotion_in[indx,]
  folds<-cut(seq(1,length(indx)),breaks=5,labels=FALSE) # 80% training data, 20% test data
  indxx = which(folds==5, arr.ind=TRUE)
  nd_test = length(indxx)+nst-1
  nd_train = length(indx) - length(indxx)+nst_train-1
  Emotion_intest[seq(nst,nd_test),]=Emotion_incl[indxx,]
  Emotion_intrain[seq(nst_train,nd_train),]=Emotion_incl[-indxx,]
  nst = nd_test+1
  nst_train = nd_train+1
}
Emotion_intrain=na.omit(Emotion_intrain)  ##########this is the data to build classfier 
Emotion_intest=na.omit(Emotion_intest)    ##########this is the data for method comparison 
Emotion_intrain$Emotion = as.factor(Emotion_intrain$Emotion)
Emotion_intest$Emotion = as.factor(Emotion_intest$Emotion)
levels(Emotion_intrain$Emotion)=c("H","S")
levels(Emotion_intest$Emotion)=c("H","S")
#############################################################################


############## KNN Method
Emotion_intrain_new<-Emotion_intrain[sample(nrow(Emotion_intrain)),] # random sampling to avoid clunks of classes
#Create 10 equally sized folds
folds <- cut(seq(1,nrow(Emotion_intrain_new)),breaks=5,labels=FALSE)
#Perform 10 fold cross validation to constrain the # of neighbors for KNN 

#make nrow based on max neighbors being 20% of 192

cverror=matrix(nrow=21,ncol=5)*NA #CV error
terror=matrix(nrow=21,ncol=5)*NA # training error
library(class)
for(j in 1:21){
  for(i in 1:5){
    #Segment your data by ith fold using the which() function
    testindices <- which(folds==i,arr.ind=TRUE)
    testEmo <- Emotion_intrain_new[testindices, ]
    trainEmo <- Emotion_intrain_new[-testindices, ]
    Emo.knn.test=knn(trainEmo[,-1],testEmo[,-1],trainEmo[,1],k=j) #knn: k-nearest neighbor classifier
    Emo.knn.train=knn(trainEmo[,-1],trainEmo[,-1],trainEmo[,1],k=j)
    cverror[j,i]=mean(testEmo[,1]!=Emo.knn.test)
    terror[j,i]=mean(trainEmo[,1]!=Emo.knn.train)
  }
}


#Segment your data by ith fold using the which() function 


x=seq(1,21)
par(new=F)
par(mar=c(4,4,4,4))
plot(x,rowMeans(cverror),type='p',col="red",xlim=c(1,30),ylim=c(0., 0.5),
     main = "Errors For K", ylab="Error", xlab="K")
xtick = seq(0, 30, by = 5)
axis(side=1, at=xtick, cex.axis=1., tck=-0.02)

par(new=T)
plot(x,rowMeans(terror),type='p',col="blue",xlim=c(1,30),ylim=c(0., 0.5), ylab="Error", xlab="K")
legend(10, 0.4, legend=c("CV error", "Training Error"), # make legend
       col=c("red", "blue"), lty=1:2, cex=1.2,
       box.lty=0)




#################SVM  Method 
library(e1071)

terror = terror*NA  # recycling the terror array
cverror = cverror*NA # recycling the cverror array
cost_values = c(0.001,0.01, 0.1, 0.5, 1., 5., 10., 15, 20., 30., 40., 50., 60., 70., 80., 90., 100.)

k = 1

for(j in cost_values){
  
  for(i in 1:5){
    
    #Segement your data by fold using the which() function
    
    testindices <- which(folds==i,arr.ind=TRUE)
    testEmo <- Emotion_intrain_new[testindices, ]
    trainEmo <- Emotion_intrain_new[-testindices, ]
    Emo.svm=svm(Emotion~.,data=trainEmo,kernel="polynomial", cost=j)
    Emo.predict.test=predict(Emo.svm,testEmo[,-1], type="class")
    Emo.predict.train=predict(Emo.svm,trainEmo[,-1], type="class")
    cverror[k,i]=mean(testEmo[,1]!=Emo.predict.test)
    terror[k,i]=mean(trainEmo[,1]!=Emo.predict.train)
  }
  
  k = k + 1
  
}


x=seq(1,21)
par(new=F)
par(mar=c(4, 3, 4, 1), mgp=c(1.5, 0.3, 0), las=0)
cverror = na.omit(cverror)
terror = na.omit(terror)


plot(cost_values,rowMeans(cverror),type='p',col="red",xlim=c(0,100),
     ylim=c(0., 0.7),
     main="SVM Misclassification Probability vs Cost", ylab = "Misclassification   Probability",
     xlab = "Cost Parameter values")
lines(cost_values,rowMeans(terror),type='p',col="blue")
legend(1, 0.6, legend=c("CV Error", "Training Error"),
       col=c("red", "blue"), lty=1:2, cex=1.3,
       box.lty=0)


#############for tree
library('rpart')  # classification tree
# cp is the cost complexity criterion
Emo.tree=rpart(Emotion~.,data=Emotion_intrain_new, control=rpart.control(xval=10, cp=0.0, minbucket=1))
printcp(Emo.tree)
par(mfrow=c(1,1))
plotcp(Emo.tree, minline=TRUE, lty=3, col=1, upper=c("size", "splits", "none"))
tree = rpart(Emotion~.,data=Emotion_intrain,cp=0.028)
par(mfrow=c(1,1))
par(mar=c(0.2,0.2,0.2,0.2))
plot(tree,margin=0.1,branch=0.7)
text(tree,use.n=T,fancy=F,cex=1)
Emo.predict=predict(Emo.tree,Emotion_intest[,-1],type="class")



##############for random forest
require(randomForest)
# ntree: number of trees built by bootstrap sampling; important = T: use OOB sample to evaluate importance
data.rf=randomForest(Emotion~.,data=Emotion_intrain,mtry=2,ntree=1000,importance=T)
vu=varUsed(data.rf)
names(vu)=names(Emotion_intrain[,-1])
vu
importance(data.rf)
par(mfrow=c(1,1))
par(mar=c(0.5,0.5,0.5,0.5))



varImpPlot(data.rf) #plot the importance
### predict the class with test data and constructed random forest classifier --> write your own code 
### find the test error (total and each category) --> write your own code


###KNN error

###############
knn_train = knn(Emotion_intrain[,-1],Emotion_intrain[,-1],Emotion_intrain[,1],k=21)
knn_test = knn(Emotion_intrain[,-1],Emotion_intest[,-1],Emotion_intrain[,1],k=21)  # for method comparison 
############count the misclassification probability for each class
#######derive confusion metrix
table.Emo.knn=table(Emotion_intest[,1], knn_test) # construct confusion matrix
#1.-diag(table.Emo.knn)/rowSums(table.Emo.knn) ## calculate the misclassification probability for each category
#1. - sum(diag(table.Emo.knn))/sum(table.Emo.knn) ###########calculate the total misclassification probability


##Log probs

### find the test error (total and each category) --> write your own code
####construct the cross-validation code to find the optimal limit for total slack --> the cost value
##############logistic regression
library('nnet')
Emo.log=multinom(Emotion~.,data=Emotion_intrain) #multinomial logistic regression (it is actually binary here)
#Emo.predict.log=predict(Emo.log,Emotion_intrain[,-1], type="class")
Emo.predict.log=predict(Emo.log,Emotion_intest[,-1], type="class")
table.Emo_log=table(Emotion_intest[,1], Emo.predict.log)
#1.-diag(table.Emo_log)/rowSums(table.Emo_log)
#1. - sum(diag(table.Emo_log))/sum(table.Emo_log)

##svm probs

##cost is the limit of total slack, could be very small or very large
Emo.svm=svm(Emotion~.,data=Emotion_intrain,kernel="polynomial", cost=10) # support vector machine
#Emo.predict.svm = predict(Emo.svm,Emotion_intest[,-1], type="class")
#Emo.predict.svm=predict(Emo.svm,Emotion_intrain[,-1], type="class")
Emo.predict.svm=predict(Emo.svm,Emotion_intest[,-1], type="class")
table.Emo_svm=table(Emotion_intest[,1], Emo.predict.svm)
#1.-diag(table.Emo_svm)/rowSums(table.Emo_svm)
#1. - sum(diag(table.Emo_svm))/sum(table.Emo_svm)


#tree probs
##cost is the limit of total slack, could be very small or very large
#Emo.tree=rpart(Emotion~.,data=Emotion_intrain_new, control=rpart.control(xval=10, cp=0.0, minbucket=1))
Emo.tree=rpart(Emotion~.,data=Emotion_intrain, control=rpart.control(xval=10, cp=0.0, minbucket=1))
#Emo.predict.tree = predict(Emo.tree,Emotion_intest[,-1], type="class")
#Emo.predict.tree=predict(Emo.tree,Emotion_intrain[,-1], type="class")
Emo.predict.tree=predict(Emo.tree,Emotion_intest[,-1], type="class")
table.Emo_tree=table(Emotion_intest[,1], Emo.predict.tree)
#1.-diag(table.Emo_tree)/rowSums(table.Emo_tree)
#1. - sum(diag(table.Emo_tree))/sum(table.Emo_tree)

#random forest prob
Emo.rf=randomForest(Emotion~.,data=Emotion_intrain,mtry=2,ntree=1000,importance=T)
#Emo.predict.rf = predict(Emo.rf,Emotion_intest[,-1], type="class")
#Emo.predict.rf=predict(Emo.rf,Emotion_intrain[,-1], type="class")
Emo.predict.rf=predict(Emo.rf,Emotion_intest[,-1], type="class")
table.Emo_rf=table(Emotion_intest[,1], Emo.predict.rf)
#1.-diag(table.Emo_rf)/rowSums(table.Emo_rf)
#1. - sum(diag(table.Emo_rf))/sum(table.Emo_rf)


#####Misclassifications for methods

###KNN misclassification
table.Emo.knn=table(Emotion_intest[,1], knn_test) # construct confusion matrix
1.-diag(table.Emo.knn)/rowSums(table.Emo.knn) ## calculate the misclassification probability for each category
1. - sum(diag(table.Emo.knn))/sum(table.Emo.knn) ###########calculate the total misclassification probability

###Log misclassification
1.-diag(table.Emo_log)/rowSums(table.Emo_log)
1. - sum(diag(table.Emo_log))/sum(table.Emo_log)

###SVM misclassification
1.-diag(table.Emo_svm)/rowSums(table.Emo_svm)
1. - sum(diag(table.Emo_svm))/sum(table.Emo_svm)

###Tree misclassification
1.-diag(table.Emo_tree)/rowSums(table.Emo_tree)
1. - sum(diag(table.Emo_tree))/sum(table.Emo_tree)

###Random forest misclassification
1.-diag(table.Emo_rf)/rowSums(table.Emo_rf)
1. - sum(diag(table.Emo_rf))/sum(table.Emo_rf)

######
######
######
######Accuracy Scores
#####


###KNN Accuracy
table.Emo.knn=table(Emotion_intest[,1], knn_test) # construct confusion matrix
diag(table.Emo.knn)/rowSums(table.Emo.knn) ## calculate the misclassification probability for each category
sum(diag(table.Emo.knn))/sum(table.Emo.knn) ###########calculate the total misclassification probability

###Log Accuracy
diag(table.Emo_log)/rowSums(table.Emo_log)
sum(diag(table.Emo_log))/sum(table.Emo_log)

###SVM Accuracy
diag(table.Emo_svm)/rowSums(table.Emo_svm)
sum(diag(table.Emo_svm))/sum(table.Emo_svm)


###Random forest Accuracy
diag(table.Emo_rf)/rowSums(table.Emo_rf)
sum(diag(table.Emo_rf))/sum(table.Emo_rf)

