library(caret)
library(AppliedPredictiveModeling)
library(randomForest)
library(ggplot2)
library (pROC)
library(earth)
library(glmnet)
installed.packages("pamr")
library(pamr)
library(rpart)
installed.packages("partykit")
library(partykit)
library(rpart)
library(lattice)
library(MASS)
library(mosaic)
library(recipes)
library(dplyr)
library(prodlim)

################# Question I #######################

oildata <- data(oil)
table(oilType)

oildatactrl <- trainControl(method = "cv")

nearZV = nearZeroVar(fattyAcids)
dim(nearZV)

############## Removing the Higly Correlated Variables #########

highCorr <- findCorrelation(cor(fattyAcids), cutoff = .75)
HCfatty <- fattyAcids[,-highCorr]

fattyf = HCfatty

############### GLMNET #################
set.seed(10)

glmGrid <-
  expand.grid(.alpha = c(0, .1, .2, .4, .6, .8, 1),
              .lambda = seq(.01, .2, length = 10))
glmModel <-
  train(
    x = fattyf,
    y = oilType,
    method = "glmnet",
    tuneGrid = glmGrid,
    preProc = c("center", "scale"),
    metric = "Accuracy",
    trControl = oildatactrl
  )

glmPred =  predict(glmnetModel, fattyf)
glmCM = confusionMatrix(data = glmnetPred, reference = yOilType)

glmCM$table
glmCM$byClass[, 11]

glms = formatC(glmCM$overall[1], digits = 4, format = "f")

############### Logistic Regression ###############
set.seed(10)

lRModel <-
  train(
    x = fattyf,
    y = oilType,
    method = "multinom",
    metric = "Accuracy",
    preProc = c("center", "scale"),
    trControl = oildatactrl
  )

LRPred = predict(lRModel, fattyf)
LRcm = confusionMatrix(data = LRPred, reference = oilType)

LRcm$table
LRcm$byClass[, 11]

lrs = formatC(LRcm$overall[1], digits = 4, format = "f")

############## Linear Discriminant Analysis #############

set.seed(10)
LDAmodel <-
  train(
    x = fattyf,
    y = oilType,
    method = "lda",
    metric = "Accuracy",
    preProc = c("center", "scale"),
    trControl = oildatactrl
  )

LDAPred = predict(ldaModel, fattyf)

LDAcm = confusionMatrix(data = LDAPred, reference = oilType)
LDAcm$table
LDAcm$byClass[, 11]

ldas = formatC(LDAcm$overall[1], digits = 4, format = "f")

########### Nearest Shrunken Centroids #################
set.seed(10)

NSCgrid <- data.frame(.threshold = seq(0, 4, by = 0.1))

NSCmodel <-
  train(
    x = fattyf,
    y = oilType,
    method = "pam",
    metric = "Accuracy",
    preProc = c("center", "scale"),
    tuneGrid = nscGrid,
    trControl = oildatactrl
  )

NSCPred <- predict(nscModel, xFA)
NSCcm <- confusionMatrix(data = nscPred, reference = yOilType)
NSCcm$table
NSCcm$byClass[, 11]
nscs = formatC(NSCcm$overall[1], digits = 4, format = "f")


############# Summary of the Models ##############
oildata.summary <- list(
  lda = ldas,
  lr = lrs,
  glmnet = glms,
  nsc = nscs
)
oildata.summary



################ Question II ######################

data(permeability)

nearZero <- nearZeroVar(fingerprints)
nonearZero <- fingerprints[, -nearZero]

set.seed(10)
train <- createDataPartition(permeability, p = 0.8, list = FALSE)

trainFP <- nonearZero[train, ]
trainPerm <- permeability[train, ]

testFP <- nonearZero[-train, ]
testPerm <- permeability[-train, ]

#################### SVM #########################

set.seed(10)

svmFit = train(
  x = trainFP,
  y = trainPerm,
  method = "svmRadial",
  preProc = c("center", "scale"),
  tuneLength = 10
)
svmFit
plot(svmFit)

###### Variable Importance ######
varImp(svmFit)

svmFitpred = predict(svmFit, newdata = testFP)
svmPRS = postResample(pred = svmFitpred, obs = testPerm)

svmPRS

RMSEs = c(svmPRS[1])
R2s = c(svmPRS[2])
methods = c("SVM")

############### KNN Model ###############

set.seed(10)
knnFit = train(
  x = trainFP,
  y = trainPerm,
  method = "knn",
  preProc = c("center", "scale"),
  tuneLength = 10
)

knnFit
plot(knnFit)

######V ariable Importance #######

knnFitpred = predict(knnFit, newdata = testFP)
knnPRS = postResample(pred = knnFitpred, obs = testPerm)
knnPRS

RMSEs = c(RMSEs, knnPRS[1])
R2s = c(R2s, knnPRS[2])
methods = c(methods, "KNN")

################ MARS Model ######################

marsgrid = expand.grid(.degree = 1:2, .nprune = 2:38)
set.seed(10)
mars = train(
  x = trainFP,
  y = trainPerm,
  method = "earth",
  preProc = c("center", "scale"),
  tuneGrid = marsgrid
)
mars

###### Variable Importance #######
varImp(mars)

marsPred = predict(mars, newdata = testFP)
marsPRS = postResample(pred = marsPred, obs = testPerm)

marsPRS

RMSEs = c(RMSEs, marsPRS[1])
R2s = c(R2s, marsPRS[2])
methods = c(methods, "MARS")

########### Summary of the Models ##############

summary = data.frame(rmse = RMSEs, r2 = R2s)
rownames(summary) = methods

summary = summary[order(-res$rmse), ]
print(summary)

#Permeability.results <- resamples(list(
# knn = knnFit,
# svm = svmFit,
# mars = mars
#))

#Permeability.results
#summary(Permeability.results)

########################## Question III ######################

data(solubility)

train = data.frame(x = solTrainXtrans$MolWeight, y = solTrainY)

############### Simple Regression Tree ###################

reg = rpart(
  y ~ .,
  data = train,
  method = "anova",
  control = rpart.control(cp = 0.01, maxdepth = 6)
)
reg
plotcp(reg)

regPred = predict(reg, newdata = data.frame(x = solTestXtrans$MolWeight))

regPRS = postResample(pred = regPred, obs = solTestY)
regPRS

plot(solTestY,
     regPred,
     main = "Simple Regression Plot",
     xlab = "Observed",
     ylab = "Predicted")


############## Random Forest #################################

rf = randomForest(y ~ ., data = train, ntree = 500)
plot(rf)

rfPred = predict(rf, newdata = data.frame(x = solTestXtrans$MolWeight))

rfPRS = postResample(pred = rfPred, obs = solTestY)
rfPRS

plot(solTestY,
     rfPred,
     main = "Random Forest Plot",
     xlab = "Observerd",
     ylab = "Predicted")

