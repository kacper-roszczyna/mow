#Install the package
#install.packages("klaR")
#Loading the library
library(klaR)

trainSet<-d1
testSet<-d2
validationSet<-d2

#Make factor if sober or drunk
trainSet$ifAlco = "sober"
trainSet[trainSet$Dalc > 1,]$ifAlco = "drunk"
trainSet[trainSet$Walc > 2,]$ifAlco = "drunk"
trainSet$ifAlco = as.factor(trainSet$ifAlco)
trainSet$Walc=NULL
trainSet$Dalc=NULL

validationSet$ifAlco = "sober"
validationSet[validationSet$Dalc > 1,]$ifAlco = "drunk"
validationSet[validationSet$Walc > 2,]$ifAlco = "drunk"
validationSet$ifAlco = as.factor(validationSet$ifAlco)


# Do Naive Bayes classification.
nb = NaiveBayes(ifAlco ~ .,data=trainSet)
nb_predict=predict(nb, testSet)

#Perform TP TN FP FN table
table(nb_predict$class,validationSet$ifAlco)
