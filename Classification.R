#Preprocessing:

dataset$Gender<-factor (dataset$Gender, levels=c("Female", "Male"), labels=c("F", "M"))
normalize_ds<-function(x)
{
    a=x/100
    return(a)
}
    dataset$Alkaline_Phosphotase=normalize_ds(dataset$Alkaline_Phosphotase)
    dataset$Alamine_Aminotransferase=normalize_ds(dataset$Alamine_Aminotransferase)
    dataset$Aspartate_Aminotransferase=normalize_ds(dataset$Aspartate_Aminotransferase)

##phase 3
dataset$Gender=factor(dataset$Gender, levels=c("F","M"), labels=c("1","0"))
dataset$Age=dataset$Age/10
dataset$Liver_Problem=factor(dataset$Liver_Problem, levels=c("1","2"), labels=c("liver patient","not liver patient"))


View(dataset)
install.packages("e1071")
library(e1071)
install.packages('caret')
library(caret)
install.packages("party")
library(party)

dataset= read.csv("Exportedindian.csv")

#Transformation for Gender and Age attribute:
dataset$Gender=factor(dataset$Gender, levels=c("F","M"), labels=c("1","0"))
dataset$Age=dataset$Age/10
dataset$Liver_Problem=factor(dataset$Liver_Problem, levels=c("1","2"), labels=c("liver patient","not liver patient"))

#Classifcation:
View(dataset)
#1-Split data(80% -20%)
set.seed(1221)
ind <- sample(2, nrow(dataset), replace=TRUE, prob=c(0.8, 0.2))
trainData <- dataset[ind==1,]
testData <- dataset[ind==2,]

myFormula <- (Liver_Problem ~ Age+ Total_Bilirubin + Gender + Direct_Bilirubin + Alkaline_Phosphotase + Alamine_Aminotransferase+ Aspartate_Aminotransferase + Total_Protiens + Albumin + Albumin_and_Globulin_Ratio)
dataset_ctree <- ctree(myFormula, data=trainData)

# check the prediction & Trees:

table(predict(dataset_ctree), trainData$Liver_Problem)
print(dataset_ctree)
plot(dataset_ctree,type="simple")
plot(dataset_ctree)

#Test:
testPred <- predict(dataset_ctree, newdata = testData)

#Evaluate Model:
table(testPred, testData$Liver_Problem)
results <- confusionMatrix(testPred, reference=testData$Liver_Problem)    
acc <- results$overall["Accuracy"]*100
print(results)
print(acc)
as.matrix(results, what = "classes")

####################################


#2-Split data(70% -30%)

ind <- sample(2, nrow(dataset), replace=TRUE, prob=c(0.7, 0.3))
trainData <- dataset[ind==1,]
testData <- dataset[ind==2,]

myFormula <- (Liver_Problem ~ Age+Total_Bilirubin + Gender + Direct_Bilirubin + Alkaline_Phosphotase + Alamine_Aminotransferase+ Aspartate_Aminotransferase + Total_Protiens + Albumin + Albumin_and_Globulin_Ratio)

# check the prediction & Trees:

table(predict(dataset_ctree), trainData$Liver_Problem)
print(dataset_ctree)
plot(dataset_ctree,type="simple")
plot(dataset_ctree)

#Test:
testPred <- predict(dataset_ctree, newdata = testData)

#Evaluate Model:
table(testPred, testData$Liver_Problem)
results <- confusionMatrix(testPred, reference=testData$Liver_Problem)    
acc <- results$overall["Accuracy"]*100
print(results)
print(acc)
as.matrix(results, what = "classes")

####################################


#3-Split data(60% - 40%)
ind <- sample(2, nrow(dataset), replace=TRUE, prob=c(0.6, 0.4))
trainData <- dataset[ind==1,]
testData <- dataset[ind==2,]

myFormula <- (Liver_Problem ~ Age+ Total_Bilirubin + Gender + Direct_Bilirubin + Alkaline_Phosphotase + Alamine_Aminotransferase+ Aspartate_Aminotransferase + Total_Protiens + Albumin + Albumin_and_Globulin_Ratio)
dataset_ctree <- ctree(myFormula, data=trainData)

# check the prediction & Trees:

table(predict(dataset_ctree), trainData$Liver_Problem)
print(dataset_ctree)
plot(dataset_ctree,type="simple")
plot(dataset_ctree)

#Test:
testPred <- predict(dataset_ctree, newdata = testData)

#Evaluate Model:
table(testPred, testData$Liver_Problem)
results <- confusionMatrix(testPred, reference=testData$Liver_Problem)    
acc <- results$overall["Accuracy"]*100
print(results)
print(acc)
as.matrix(results, what = "classes")

