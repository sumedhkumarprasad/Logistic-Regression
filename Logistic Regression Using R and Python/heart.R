hd=read.csv("Heart.csv", header = TRUE,stringsAsFactors = TRUE)
# Importing Data Set 
s = sum(is.na(hd))
hd <- na.omit(hd) # Removing NA from whole dataset 
# Renaming the column name of whole data set for adding the id in the dataset 

names(hd) <- c( "id","Age", "Sex", "ChestPain", "RestBP", "MaxHR","Fbs", "RestECG",
                "MaxHR","ExAng", "Oldpeak","Slope", "Ca", "Thal", "AHD")
str(hd)
View(hd)
# Normal Distribution of Age 

l<- hd$Age
hist(l,prob=T,main = "Normal Distribution for Age",xlab = "Age in years ",ylab = "Count",breaks = 30,col = "grey" )
age_norm = rnorm(length(hd$Age),mean = (hd$Age), sd = sd(hd$Age))
lines(density(age_norm, adjust = 2),col="Blue", lwd=0.5)

# Normal Distibution of RestBP

m<- hd$RestBP
hist(m,prob=T,main = "Normal Distribution for RestBP",xlab = "Resting blood pressure on admission to hospital ",ylab = "Count",breaks = 30,col = "grey" )
restbp_norm = rnorm(length(hd$RestBP),mean = (hd$RestBP), sd = sd(hd$RestBP))
lines(density(restbp_norm, adjust = 0.5),col="Blue", lwd=0.5)

# Normal Distribution of MaxHRrestrol 

n<- hd$MaxHR
hist(n,prob=T,main = "Normal Distribution for MaxHR",xlab = "Chorestrol level in the body",ylab = "Count",breaks = 30,col = "grey" )
MaxHR_norm = rnorm(length(hd$MaxHR),mean = (hd$MaxHR), sd = sd(hd$MaxHR))
lines(density(MaxHR_norm, adjust = 0.5),col="Blue", lwd=0.5)

# Normal Distribution of Maximum Heart Rate.

o<- hd$MaxHR
hist(o,prob=T,main = "Normal Distribution for MaxHR",xlab = "Maximum heart Rate",ylab = "Count",breaks = 30,col = "grey" )
MaxHR_norm = rnorm(length(hd$MaxHR),mean = (hd$MaxHR), sd = sd(hd$MaxHR))
lines(density(MaxHR_norm, adjust = 0.5),col="Blue", lwd=0.5)

# Converting Categorial variables into the factors with levels and lables.
hd$Sex=as.factor(hd$Sex)
hd$Sex=factor(hd$Sex,levels = c(0,1),labels = c("Female","Male"))
str(hd)
View(hd)
hd$Fbs=as.factor(hd$Fbs)
hd$Fbs=factor(hd$Fbs,levels = c(0,1),labels =c("Low","High"))
str(hd$Fbs)
View(hd)
hd$RestECG=as.factor(hd$RestECG)
hd$RestECG=factor(hd$RestECG,levels = c(0,1,2),labels =c("normal","abnormal","left ventricular"))
View(hd)
str(hd)
head(hd,2)
hd$ExAng=as.factor(hd$ExAng)
hd$ExAng=factor(hd$ExAng, levels = c(0,1),labels = c("no","yes"))
str(hd)
View(hd)
hd$Slope=factor(hd$Slope)
hd$Slope=factor(hd$Slope,levels = c(1,2,3),labels = c("up","flat","down"))
View(hd)
hd$Ca=as.factor(hd$Ca)
hd$Ca=factor(hd$Ca,levels=c(0,1,2,3))
hd$Thal=as.factor(hd$Thal)

hd$AHD=as.factor(hd$AHD)
mosaicplot(hd$Sex ~ hd$AHD,
           main="Heart Disease classification by Gender", shade=FALSE,color=TRUE,
           xlab="Gender", ylab="Heart disease")
table(hd$Sex,hd$AHD)
boxplot(hd$Age ~ hd$AHD,
        main="Comparing the Heart Disease by Age",
        ylab="Age",xlab="Heart disease")

#Logistic regression---------------------------------------------
library(caret)
set.seed(10)
# Partition of dataset into test and train with 70 :30 ratio 
partdata <- createDataPartition(hd$AHD,p=0.7,list=FALSE)
trainData <- hd[partdata,]
testData <-  hd[-partdata,]
nrow(trainData)/(nrow(testData)+nrow(trainData))
AUC = list()
Accuracy = list()
set.seed(10)
# Logistic Model Building (Model1)
logRegModel <- train(AHD ~ ., data=trainData, method = 'glm', family = 'binomial')
summary(logRegModel)
modlog<- glm(AHD~.,data=trainData,family = "binomial",na.action = na.omit)
summary(modlog)
logRegPrediction <- predict(logRegModel, testData)



logRegPredictionprob <- predict(logRegModel, testData, type='prob')[2]


logRegConfMat <- confusionMatrix(logRegPrediction, testData[,"AHD"])


#ROC Curve
library(pROC)

AUC$logReg <- roc(as.numeric(testData$AHD),as.numeric(as.matrix((logRegPredictionprob))))$auc
Accuracy$logReg <- logRegConfMat$overall['Accuracy']
#  Logistic regression Model Building ( Model 2)

model2 <- train(AHD~Sex+ChestPain+RestBP+MaxHR,data = trainData,method="glm",family = "binomial")
summary(model2)


prediction2 <- predict(model2, testData)
predictionprob2 <- predict(model2, testData, type='prob')[2]
matrix2 <- confusionMatrix(prediction2, testData[,"AHD"])
# By comparing this two models first model is having the greater accuracy.
# Accuracy is coming more in Model1 compare to Model 2


# Confusion Matrix
table(Actualvalues = testData$AHD, Predictedvalues = logRegPredictionprob> 0.63)
# Checking for different probability threshold value optimal value is coming at 0.63

#ROC curve for predicting cut off

library(Deducer)
rocplot(modlog)

# Conditional Probability plot 
cdplot(testData$AHD~logRegPrediction,data = testData, main = "Conditional Probability Density Curve")
