#Install the package
#install.packages("klaR")
#Loading the library
library(klaR)

#d1 = subset(d1, select = -c(school) )


d1$ifAlco = "sober"
d1[d1$Dalc > 1,]$ifAlco = "drunk"
d1[d1$Walc > 2,]$ifAlco = "drunk"
d1$ifAlco = as.factor(d1$ifAlco)
d1_argsOnly= subset(d1, select = -c(Dalc,Walc))
predictor.col = 1:ncol(d1_argsOnly)-1
# Do Naive Bayes classification.

library(klaR)
nb = NaiveBayes(ifAlco ~ .,data=d1_2)
nb_predict=predict(nb)
