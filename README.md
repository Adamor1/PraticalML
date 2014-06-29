PraticalML
==========
# Project Course Machine Learning.

# Language R.

library(caret); library(gbm)

#By spreading the data for training an cross validation.

pmltrainMin <- subset(pml.training, select=-c(X, user_name))

inTrain <- createDataPartition(y=pmltrainMin$classe, p=0.8, list=FALSE)

train <- pmltrainMin[ inTrain,]
test  <- pmltrainMin[-inTrain,]

# I chose the gbm to be fast and efficient.
# For more accuracy and still possible to increase the number of iterations.

gbm1 <- gbm(classe ~., data=train, distribution="gaussian",
            n.trees=10000, shrinkage=0.1, cv.folds=2, verbose=TRUE)
            
# Choosing the best iteration.

bestperf1 <- gbm.perf(gbm1, plot.it=TRUE, method="cv")

# Predicting using the best iteration.

pmlpred1 <- predict(gbm1, test, bestperf1)

# To build a rule of class.

plot(density(pmlpred1))
grid(ny=12, equilogs=TRUE)

# Rule1, function for class.

rule1 <- function(x) {
pred <- rep(NA, length(x))
pred[( x >= 0.0 & x < 1.5)] <- "A"
pred[( x >= 1.5 & x < 2.5)] <- "B"
pred[( x >= 2.5 & x < 3.5)] <- "C"
pred[( x >= 3.5 & x < 4.5)] <- "D"
pred[( x >= 4.5 & x < 6.0)] <- "E"
return(pred)
}

# Accuracy : 96.92%.
#       IC : 95.43 in 97.26

confusionMatrix((rule1(pmlpred1)), test$classe)
qplot(pmlpred1, colour=classe, data=test)


# Finally we can predict the cases.
pmlpredcases <- predict(gbm1, pml.testing, bestperf1)
rule1(pmlpredcases)










