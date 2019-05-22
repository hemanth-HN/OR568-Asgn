library(AppliedPredictiveModeling)
library(caret)
library(pls)
library(ggplot2)
library(lattice)

#####6.1 (a)#####
data(tecator)
colnames(absorp) <- paste("x", 1:ncol(absorp))
#?tecator

#####6.1(b)######
dimPCA <- prcomp(absorp, center = TRUE, scale = TRUE)
summary(dimPCA)

#####6.1(c)######

set.seed(620)
Meatds <- createDataPartition(endpoints[, 3], p = 0.75, list=FALSE)

trainAbsorp <- absorp[Meatds,]
trainProtein <- endpoints[Meatds,3]
testAbsorp <- absorp[-Meatds,]
testProtein <- endpoints[-Meatds,3]

#####Linear Model#####
set.seed(560)
Meatlm<-train(x=trainAbsorp, y=trainProtein, method = "lm", trControl = trainControl(method = "repeatedcv", repeats = 5))
Meatlm

#####PLS Model#####
set.seed(560)
MeatPLS <- train(x=trainAbsorp, y=trainProtein, method="pls", 
                 trControl = trainControl(method = "repeatedcv", repeats = 5),
                 preProcess = c("center", "scale"),
                 tuneLength = 20)
MeatPLS

#####PCR Model#####
set.seed(560)
MeatPCR <- train(x=trainAbsorp, y=trainProtein, method="pcr",
                 trControl=trainControl(method = "repeatedcv", repeats = 5),
                 tuneLength = 20)
MeatPCR

#####6.2 (a)#####
data(permeability)
#View(fingerprints)

#####6.2 (b)#####
nearZero <- nearZeroVar(fingerprints)
nonearZero <- fingerprints[,-nearZero]
nonearZero
ncol(nonearZero)

#####6.2 (c)#####
set.seed(10)
training <- createDataPartition(permeability, p = 0.8,list=FALSE)

trainFingerprints <- nonearZero[training,]
trainPermeability <- permeability[training,]
testFingerprints <- nonearZero[-training,]
testPermeability <- permeability[-training,]

PLS <- train( trainFingerprints, trainPermeability, method="pls",
                   tuneLength=12, 
                   preProcess=c("center","scale"),
                   trControl=trainControl(method="repeatedcv",repeats=5) )
PLS

#####6.2 (d)#####

t = predict( PLS, newdata=testFingerprints )
rsquarepls = cor(t,testPermeability,method="pearson")^2
rsquarepls
