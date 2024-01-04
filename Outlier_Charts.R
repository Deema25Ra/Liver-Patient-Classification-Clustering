getwd()
setwd("Desktop/DatamininigProject")
dataset = read.csv('indian_liver_patient_dataset.csv') 
#2
LV=quantile(dataset$Total_Bilirubin,p=0.25)-1.5*IQR(dataset$Total_Bilirubin)
HV=quantile(dataset$Total_Bilirubin,p=0.75)+1.5*IQR(dataset$Total_Bilirubin)
sum(dataset$Total_Bilirubin>HV)+sum(dataset$Total_Bilirubin<LV)
boxplot(dataset$Total_Bilirubin)


#3
LV=quantile(dataset$Direct_Bilirubin,p=0.25)-1.5*IQR(dataset$Direct_Bilirubin)
HV=quantile(dataset$Direct_Bilirubin,p=0.75)+1.5*IQR(dataset$Direct_Bilirubin)
sum(dataset$Direct_Bilirubin>HV)+sum(dataset$Direct_Bilirubin<LV)
boxplot(dataset$Direct_Bilirubin)

#4
LV=quantile(dataset$Alkaline_Phosphotase,p=0.25)-1.5*IQR(dataset$Alkaline_Phosphotase)
HV=quantile(dataset$Alkaline_Phosphotase,p=0.75)+1.5*IQR(dataset$Alkaline_Phosphotase)
sum(dataset$Alkaline_Phosphotase>HV)+sum(dataset$Alkaline_Phosphotase<LV)
boxplot(dataset$Alkaline_Phosphotase)

#5
LV=quantile(dataset$Alamine_Aminotransferase,p=0.25)-1.5*IQR(dataset$Alamine_Aminotransferase)
HV=quantile(dataset$Alamine_Aminotransferase,p=0.75)+1.5*IQR(dataset$Alamine_Aminotransferase)
sum(dataset$Alamine_Aminotransferase>HV)+sum(dataset$Alamine_Aminotransferase<LV)
boxplot(dataset$Alamine_Aminotransferase)
#6
LV=quantile(dataset$Aspartate_Aminotransferase,p=0.25)-1.5*IQR(dataset$Aspartate_Aminotransferase)
HV=quantile(dataset$Aspartate_Aminotransferase,p=0.75)+1.5*IQR(dataset$Aspartate_Aminotransferase)
sum(dataset$Aspartate_Aminotransferase>HV)+sum(dataset$Aspartate_Aminotransferase<LV)
boxplot(dataset$Aspartate_Aminotransferase)
#7
LV=quantile(dataset$Total_Protiens,p=0.25)-1.5*IQR(dataset$Total_Protiens)
HV=quantile(dataset$Total_Protiens,p=0.75)+1.5*IQR(dataset$Total_Protiens)
sum(dataset$Total_Protiens>HV)+sum(dataset$Total_Protiens<LV)
boxplot(dataset$Total_Protiens)
#8
LV=quantile(dataset$Albumin,p=0.25)-1.5*IQR(dataset$Albumin)
HV=quantile(dataset$Albumin,p=0.75)+1.5*IQR(dataset$Albumin)
sum(dataset$Albumin>HV)+sum(dataset$Albumin<LV)
boxplot(dataset$Albumin)
#9
dataset =na.omit(dataset)#to deal with missing values
LV=quantile(dataset$Albumin_and_Globulin_Ratio,p=0.25 ,na.replace=TRUE)-1.5*IQR(dataset$Albumin_and_Globulin_Ratio)
HV=quantile(dataset$Albumin_and_Globulin_Ratio,p=0.75)+1.5*IQR(dataset$Albumin_and_Globulin_Ratio)
sum(dataset$Albumin_and_Globulin_Ratio>HV)+sum(dataset$Albumin_and_Globulin_Ratio<LV)
boxplot(dataset$Albumin_and_Globulin_Ratio)

#charts
#histagram
hist(dataset$Age)
## pie chart
library(dplyr)
dataset2<-dataset%>%sample_n(50)
dataset2$Gender %>% table()%>% pie()

##add percentage
tab <- dataset2$Gender %>% table()
precentages <- tab %>% prop.table() %>% round (3) * 100
txt <- paste0(names (tab), '\n', precentages, '%')
pie (tab, labels=txt)
