install.packages("mlbench")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("caret")
install.packages("lattice")
install.packages("e1071")

library(ggplot2)
library(corrplot)
library(caret)
library(lattice)
library(e1071)
library(mlbench)

data(Glass)
str(Glass)
View(Glass)

#3.1(a)
##### Histograms #####

Ghist <- Glass[,1:9]
par(mfrow = c(3, 3))
for (i in 1:ncol(Ghist))
{
  hist(Ghist[ ,i], xlab = names(Ghist[i]), main = paste(names(Ghist[i]), "histogram"), col="gold")  
}

##### Correlation of Each Predictor #####

corrGlass <- round(cor(Glass[1:9]), 3)
corrplot(corrGlass, method = "color", tl.col = "black", tl.cex = 0.65)

#3.1(b)
##### BoxPlots #####

Gbox <- Glass[,1:9]
par(mfrow = c(3, 3))
for (i in 1:ncol(Gbox)) 
{
  boxplot(Gbox[ ,i], ylab = names(Gbox[i]), horizontal=T,
          main = paste(names(Gbox[i]), "Boxplot"), col="gold")
}

#3.1(c) 
##### Box-Cox transformation #####

boxcox = function(y){
  B = BoxCoxTrans(y)
  u = predict( B, y )
  skewness(u) 
}

apply( Glass[,-10], 2, skewness)

apply( Glass[,-10], 2, boxcox )

##### Adding values to increase the transformation #####

Glass$K=Glass$K+0.000001
Glass$Ba=Glass$Ba+0.000001
Glass$Fe=Glass$Fe+0.000001

apply( Glass[,-10], 2, boxcox )

###### 3.2 ######
data(Soybean)

##### 3.2(a) #####
##### Finding the predictors with zero variance #####
X=nearZeroVar( Soybean )
colnames( Soybean )[ X ]

##### 3.2(b) #####
##### Finding the Missing Data #####
Soybean=Soybean[,-X]
table(Soybean$Class, complete.cases(Soybean))

