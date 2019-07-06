## Credits By :-
## -------------
## - Chahira Hamza
## - Taghreed Hassan
## - Rana Mostafa
## - Fady Nasser
###############################################################################################
# Libraries Used 
# --------------
library(dplyr)
library("e1071")
library(randomForest)
require(neuralnet)
library(caret)
library(MASS)
#-----------------------------------------------------------------------------------------------------
# Set Directory
#-----------------------------------------------------------------------------------------------------
setwd("C:\\Users\\Owner\\Desktop\\Cairo University\\Senior-2\\Big Data\\Project\\hr-analytics-big-data\\Source Code\\Data")
rm(list=ls())
#-----------------------------------------------------------------------------------------------------
# Preprocessing Data as Data frames: Employee, General, Manager, in_time & out_time.csv 
# -------------------------------------------------------------------------------------
Employee_Data <- as.data.frame(read.table("employee_survey_data.csv",header=TRUE,sep=","))
General_Data <- as.data.frame(read.table("general_data.csv",header=TRUE,sep=","))
Manager_Data <- as.data.frame(read.table("manager_survey_data.csv",header=TRUE,sep=","))
in_time <- as.data.frame(read.table("in_time.csv",header=TRUE,sep=","))
out_time <- as.data.frame(read.table("out_time.csv",header=TRUE,sep=","))
#-----------------------------------------------------------------------------------------------------
# Taking subset of General Data & Performing NA Omit
#-----------------------------------------------------------------------------------------------------
cleaned_data <- General_Data[, !(colnames(General_Data) %in% c("EmployeeCount","EmployeeID","StandardHours","Over18"))]
cleaned_data <- na.omit(cleaned_data)
#-----------------------------------------------------------------------------------------------------
# Removing Outliers
#-----------------------------------------------------------------------------------------------------
outliers_data <- cleaned_data[, (colnames(cleaned_data) %in% c("MonthlyIncome","NumCompaniesWorked",
                                                               "TotalWorkingYears","YearsAtCompany","YearsWithCurrManager",
                                                               "TrainingTimesLastYear"))]

logical_list <- rep(FALSE,dim(outliers_data)[1])

i = 1
while(TRUE){
  third_Q <- unname(quantile(outliers_data[,i], .75))
  intermediate <- (outliers_data[i]>2.5*third_Q )[,1]
  table(intermediate)
  logical_list <- Reduce(`+`, list(logical_list,intermediate)) > 0
  i  = i + 1
  if (i == 8)
    break()
}
table(logical_list)

cleaned_data <- cleaned_data[!logical_list,]

#-----------------------------------------------------------------------------------------------------
# Model Selection
#-----------------------------------------------------------------------------------------------------
# 1) Our Top Classifier (SVM Model)
#-----------------------------------------------------------------------------------------------------

## Split data into partitions
# Preprocessing of Monthly Income [ Normalisation]
cleaned_data$MonthlyIncome = log(cleaned_data$MonthlyIncome)
limit = round(0.8*(dim(cleaned_data)[1]))
total = dim(cleaned_data)[1]
train_data = cleaned_data[1:limit,]
test_data = cleaned_data[(limit+1):total,]


## SVM Model
#  this may take a while !
svmfit = svm(Attrition ~ Age + BusinessTravel + Department + Education + EducationField + 
               JobRole + MaritalStatus + MonthlyIncome + NumCompaniesWorked + 
               StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
               YearsAtCompany + YearsWithCurrManager,data =train_data, 
             kernel = "polynomial", cost = 10, scale = FALSE)
print(summary(svmfit))


# training
y_pred = predict(svmfit,type="response")
matrix <- table(y_pred,train_data$Attrition)
recall <- matrix[2,2] / (matrix[2,2] + matrix[1,2] )
accuracy <- (matrix[1,1]+matrix[2,2]) / length(y_pred)
recall
accuracy

# testing
y_pred_t = predict(svmfit,newdata = test_data,type="response")
matrix <- table(y_pred_t,test_data$Attrition)
recall_t <- matrix[2,2] / (matrix[2,2] + matrix[1,2] )
accuracy_t <- (matrix[1,1]+matrix[2,2]) / length(y_pred_t)
recall_t
accuracy_t

#-----------------------------------------------------------------------------------------------------
# 2) Naive Bayes  
#-----------------------------------------------------------------------------------------------------
# Training Data
Train_Data1<-subset(General_Data[1:2400,], select = c("Attrition","Department","Education","EducationField","JobLevel","JobRole","MaritalStatus"))
Train_Data2<-subset(Employee_Data[1:2400,], select = c("EnvironmentSatisfaction","WorkLifeBalance"))
Train_Data3<-subset(Manager_Data[1:2400,],select=c("JobInvolvement","PerformanceRating"))
TotalTrain_Data <- bind_cols(Train_Data1, Train_Data2,Train_Data3)

# Validation Data
Valid_Data1<-subset(General_Data[2400:3200,], select = c("Attrition","Department","Education","EducationField","JobLevel","JobRole","MaritalStatus"))
Valid_Data2<-subset(Employee_Data[2400:3200,], select = c("EnvironmentSatisfaction","WorkLifeBalance"))
Valid_Data3<-subset(Manager_Data[2400:3200,],select=c("JobInvolvement","PerformanceRating"))
TotalValid_Data <- bind_cols(Valid_Data1, Valid_Data2,Valid_Data3)


# Testing Data
Test_Data1<-subset(General_Data[3200:4000,], select = c("Attrition","Department","Education","EducationField","JobLevel","JobRole","MaritalStatus"))
Test_Data2<-subset(Employee_Data[3200:4000,], select = c("EnvironmentSatisfaction","WorkLifeBalance"))
Test_Data3<-subset(Manager_Data[3200:4000,],select=c("JobInvolvement","PerformanceRating"))
TotalTest_Data <- bind_cols(Test_Data1,Test_Data2,Test_Data3)
TotalTest_Data

ActualResults<-TotalTest_Data$Attrition

# Model
model <- naiveBayes(Attrition~.,TotalTrain_Data)
model

# Validation results
validresults <- predict (model, TotalValid_Data)
validresults

## Validation Accuracy
## 84 %
ValidActualResults<-TotalValid_Data$Attrition
print(paste("Accuracy",sum(ValidActualResults==validresults)/nrow(TotalTest_Data)*100, "%"))

# Predict results 
results <- predict (model, TotalTest_Data)
results

## Accuracy 82.77 % 
print(paste("Accuracy",sum(ActualResults==results)/nrow(TotalTest_Data)*100, "%"))
#-----------------------------------------------------------------------------------------------------
# B) Random Forest  
#-----------------------------------------------------------------------------------------------------
Train_Data1<-subset(General_Data[1:3200,], select = c("Attrition","Department","Education","EducationField","JobLevel","JobRole","MaritalStatus"))
Train_Data2<-subset(Employee_Data[1:3200,], select = c("EnvironmentSatisfaction","WorkLifeBalance"))
Train_Data3<-subset(Manager_Data[1:3200,],select=c("JobInvolvement","PerformanceRating"))
TotalTrain_Data <- bind_cols(Train_Data1, Train_Data2,Train_Data3)


k_FoldsValidation <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
set.seed(1234)
# Run the model with the default values
rf_default <- train(Attrition~.,
                    data = TotalTrain_Data,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = k_FoldsValidation, 
                    na.action = na.omit)

print(rf_default)


# Run model to calculate optimal number of nodes that give best accuracy
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = 24)
for (maxnodes in c(5: 15)) {
  set.seed(1234)
  rf_maxnode <- train(Attrition~.,
                      data = TotalTrain_Data,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl =  k_FoldsValidation,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300,
                      na.action = na.omit)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

# The optimal maxnode equal to 13

## Find optimal ntree

store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(Attrition~.,
                       data = TotalTrain_Data,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = k_FoldsValidation,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 13,
                       ntree = ntree,
                       na.action = na.omit)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)


## Final Model
fit_rf <- train(Attrition~.,
                data = TotalTrain_Data,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = k_FoldsValidation,
                importance = TRUE,
                nodesize = 14,
                ntree = 550,
                maxnodes = 13,
                na.action = na.omit)

print(fit_rf)
print(fit_rf$resample)

## Evaluate Model
TotalTest_Data<- na.omit(TotalTest_Data)
ActualResults<-TotalTest_Data$Attrition

## Predictions Calculation
prediction <-predict(fit_rf, TotalTest_Data)

## Calculation of Confusion Matrix Accuracy : 83.9%
confusionMatrix(prediction, ActualResults)
#-----------------------------------------------------------------------------------------------------
# 3) Shallow Neural Networks 
#-----------------------------------------------------------------------------------------------------

Train_NNData1<-subset(General_Data[1:2400,], select = c("Attrition","PercentSalaryHike","DistanceFromHome"
                                                        ,"Education","JobLevel","NumCompaniesWorked",
                                                        "Age","Gender",
                                                        "MonthlyIncome","YearsAtCompany","TotalWorkingYears"))
Train_NNData1$Gender<-ifelse(Train_NNData1$Gender == 'Male', 1, 0)
Train_NNData2<-subset(Employee_Data[1:2400,], select = c("WorkLifeBalance"))
Train_NNData3<-subset(Manager_Data[1:2400,], select = c("PerformanceRating"))
TotalTrain_NNData <- na.omit(bind_cols(Train_NNData1, Train_NNData2,Train_NNData3)) 


Valid_NNData1<-subset(General_Data[2400:3200,], select = c("Attrition","PercentSalaryHike","DistanceFromHome"
                                                        ,"Education","JobLevel","NumCompaniesWorked",
                                                        "Age","Gender",
                                                        "MonthlyIncome","YearsAtCompany","TotalWorkingYears"))
Valid_NNData1$Gender<-ifelse(Valid_NNData1$Gender == 'Male', 1, 0)
Valid_NNData2<-subset(Employee_Data[2400:3200,], select = c("WorkLifeBalance"))
Valid_NNData3<-subset(Manager_Data[2400:3200,], select = c("PerformanceRating"))
TotalValid_NNData <- na.omit(bind_cols(Valid_NNData1, Valid_NNData2, Valid_NNData3)) 


Test_NNData1<-subset(General_Data[3200:4000,], select = c("Attrition","PercentSalaryHike","DistanceFromHome"
                                                          ,"Education","JobLevel","NumCompaniesWorked",
                                                          "Age","Gender",
                                                          "MonthlyIncome","YearsAtCompany","TotalWorkingYears"))
Test_NNData1$Gender<-ifelse(Test_NNData1$Gender == 'Male', 1, 0)
Test_NNData2<-subset(Employee_Data[3200:4000,], select = c("WorkLifeBalance"))
Test_NNData3<-subset(Manager_Data[3200:4000,], select = c("PerformanceRating"))
TotalTest_NNData <- na.omit(bind_cols(Test_NNData1,Test_NNData2,Test_NNData3))


nn<-neuralnet(Attrition =="Yes"~ ., 
             data=TotalTrain_NNData, stepmax=1e6, 
             hidden=8,act.fct = "logistic",
             learningrate =0.25,rep=10,
             linear.output = FALSE)
#plot(nn)


## Validation
Validation <- ifelse(compute(nn,TotalValid_NNData)$net.result>0.5, "Yes", "No")

# Validation Accuracy 
print(paste("Accuracy",sum(TotalValid_NNData$Attrition==Validation)/nrow(TotalValid_NNData)*100, "%"))

## Testing Data
Prediction <- ifelse(compute(nn,TotalTest_NNData)$net.result>0.5, "Yes", "No")

# Accuracy
print(paste("Accuracy",sum(TotalTest_NNData$Attrition==Prediction)/nrow(TotalTest_NNData)*100, "%"))
#-----------------------------------------------------------------------------------------------------
# Endeavors
#-----------------------------------------------------------------------------------------------------
#  Naive Bayes with K-Folds Validation
#-----------------------------------------------------------------------------------------------------
# Training Data
Train_Data1<-subset(General_Data[1:3200,], select = c("Attrition","Department","Education","EducationField","JobLevel","JobRole","MaritalStatus"))
Train_Data2<-subset(Employee_Data[1:3200,], select = c("EnvironmentSatisfaction","WorkLifeBalance"))
Train_Data3<-subset(Manager_Data[1:3200,],select=c("JobInvolvement","PerformanceRating"))
TotalTrain_Data <- na.omit(bind_cols(Train_Data1, Train_Data2,Train_Data3))

# Testing Data
Test_Data1<-subset(General_Data[3200:4000,], select = c("Attrition","Department","Education","EducationField","JobLevel","JobRole","MaritalStatus"))
Test_Data2<-subset(Employee_Data[3200:4000,], select = c("EnvironmentSatisfaction","WorkLifeBalance"))
Test_Data3<-subset(Manager_Data[3200:4000,],select=c("JobInvolvement","PerformanceRating"))
TotalTest_Data <-na.omit(bind_cols(Test_Data1,Test_Data2,Test_Data3))
TotalTest_Data

ActualResults<-TotalTest_Data$Attrition


# Model
Naive_fit <- train(TotalTrain_Data[, -1], TotalTrain_Data$Attrition, "nb", 
                   trControl = trainControl(method = "cv", number = 10))


## Predictions Calculation
prediction <-predict(Naive_fit, TotalTest_Data)

## Accuracy
print(paste("Accuracy",sum(ActualResults==prediction)/nrow(TotalTest_Data)*100, "%"))

#-----------------------------------------------------------------------------------------------------
# Feature Extraction
#-----------------------------------------------------------------------------------------------------
# 1) PCA
#-----------------------------------------------------------------------------------------------------
# A) PCA on General Data
#-----------------------------------------------------------------------------------------------------
gg<-na.omit(General_Data)
nrow(gg)
tt<-data.frame(gg$DistanceFromHome,gg$MonthlyIncome,gg$YearsAtCompany,gg$NumCompaniesWorked,
               gg$PercentSalaryHike,gg$TotalWorkingYears,gg$YearsWithCurrManager,
               gg$YearsSinceLastPromotion,gg$Age,gg$Education)

tt.pca <- prcomp(tt, center = TRUE,scale. = TRUE)

summary(tt.pca)

str(tt.pca)

head(tt.pca)

head((tt.pca$rotation))
#-----------------------------------------------------------------------------------------------------
# B) Performed PCA on Employee Data
#-----------------------------------------------------------------------------------------------------
tt1.pca <- prcomp(na.omit(Employee_Data), center = TRUE,scale. = TRUE)
summary(tt1.pca)
str(tt1.pca)
head(tt1.pca)
head((tt1.pca$rotation))

#-----------------------------------------------------------------------------------------------------
# 2) Bi-directional Elimination [ Stepwise Regression tp find best features ]
#-----------------------------------------------------------------------------------------------------
col_names = colnames(train_data)[-2]
fit <- glm(as.formula(paste("Attrition ~ ", paste(col_names, collapse="+"))),
           data =train_data, family=binomial(link="logit"),
           na.action=na.pass)
step <- stepAIC(fit, direction="both")
step$anova
#-----------------------------------------------------------------------------------------------------






