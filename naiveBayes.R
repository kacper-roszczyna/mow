#Install the package
#install.packages("klaR")
#Loading the library
library(klaR)
library(magrittr)
library(dplyr)


d3=rbind(d1,d2)
d3<-d3 %>% distinct(school,sex,age,address,famsize,Pstatus,
                             Medu,Fedu,Mjob,Fjob,reason,
                             guardian,traveltime,studytime,failures,
                             schoolsup, famsup,activities,nursery,higher,internet,
                             romantic,famrel,freetime,goout,Dalc,Walc,health,absences, .keep_all = TRUE)



##Dividing set into train and test data
set.seed(123)
smp_size <- floor(0.8 * nrow(d3))
train_ind <- sample(seq_len(nrow(d3)), size = smp_size)

trainSet<-d3[train_ind, ]
testSet<-d3[-train_ind, ]
validationSet<-testSet

#Make factor if sober or abuse
trainSet$ifAlco = "sober"
trainSet[trainSet$Dalc >= 3 & trainSet$Walc>=4,]$ifAlco = "abuse"
trainSet[trainSet$Dalc >= 4, ]$ifAlco = "abuse"
#trainSet[trainSet$Walc < 4& trainSet$Dalc ]$ifAlco = "sober"
trainSet$ifAlco = as.factor(trainSet$ifAlco)
trainSet$Dalc=NULL
trainSet$Walc=NULL

validationSet$ifAlco = "sober"
validationSet[validationSet$Dalc >= 3 & validationSet$Walc >=4, ]$ifAlco = "abuse"
validationSet[validationSet$Dalc >= 4,]$ifAlco = "abuse"
validationSet$ifAlco = as.factor(validationSet$ifAlco)

testSet$Dalc=NULL
testSet$Walc=NULL


# Do Naive Bayes classification.
nb = NaiveBayes(ifAlco ~ .,data=trainSet)

# Do prediction on testset
nb_predict=predict(nb, testSet)

#Perform TP TN FP FN table
result=table(nb_predict$class,validationSet$ifAlco)

precision=result[2,2]/(result[2,2] + result[2,1])
recall=result[2,2]/(result[2,2]+ result[1,2])
Fmeasure=(2*recall*precision)/(recall+precision)
