# Library checks
library(e1071)
library(ROCR)



setwd("C:\\Users\\Owner\\Desktop\\Cairo University\\Senior-2\\Big Data\\Project\\hr-analytics-big-data\\Source Code\\Data")
rm(list=ls())
General_Data <- as.data.frame(read.table("general_data.csv",header=TRUE,sep=","))

# Data Pre-Processing for the following code
#################################################################################

# Not Useful Columns
# Employee count
# Employee ID
# standard hours
# Over18

# Removing them from the data
cleaned_data <- General_Data[, !(colnames(General_Data) 
                         %in% c("EmployeeCount","EmployeeID","StandardHours","Over18"))]

# Removing all thd NA
cleaned_data <- na.omit(cleaned_data)

# outliers calculation
# marking a datapoint as an outlier if above 1.5 * 3rd Quartile (from summary of each column)

# conitnuous data
# 1. Age
# 2. DistanceFromHome
# 3. Monthly income       ~ has 567 outliers ~ 12.8% table(cleaned_data$MonthlyIncome>1.5*83800)
# 4. NumCompaniesWorked   ~ 525 outliers ~  11.9% table(cleaned_data$NumCompaniesWorked>1.5*4)
# 5. PercentSalaryHike
# 6. TotalWorkingYears    ~ 456 outliers(has NA) ~ 10% table(cleaned_data$TotalWorkingYears>1.5*15)
# 7. YearsAtCompany       ~ 528 outliers ~ 11.9% table(cleaned_data$YearsAtCompany>1.5*9)
# 8. YearsSinceLastPromotion ~ 780 outliers ~ 17.6% table(cleaned_data$YearsSinceLastPromotion>1.5*3)
# 9. YearsWithCurrManager     ~ 219 outliers ~ 4.9% table(cleaned_data$YearsWithCurrManager>1.5*7)
# 10. TrainingTimesLastYear   ~ 552 outliers ~ 12.5%

# cleaning the data from all these outliers will wipe out a lot of data, since they are not
# all common rows. (~ 40% of the data using this definition)
# So we decided to remove the outliers rows of the variables related to our features only.
# and to mark a datapoint as an outlier if > (2.5 * 3rd Quartile) instead of 1.5
# this value can be changed by changing the threshold variable in the following while loop
# Setting threshold to 2.5 results in 2.5% (just a coincidence) outliers.

# Removing Outliers
# OR all the rows to be removed in a logical map
# apply logical map to our data
###################

outliers_data <- cleaned_data[, (colnames(cleaned_data) %in% c("MonthlyIncome","NumCompaniesWorked",
                            "TotalWorkingYears","YearsAtCompany","YearsWithCurrManager",
                            "TrainingTimesLastYear"))]

logical_list <- rep(FALSE,dim(outliers_data)[1])

# threshold to be changed to control the outliers
threshold = 2.5

i = 1
while(TRUE){
  third_Q <- unname(quantile(outliers_data[,i], .75))
  intermediate <- (outliers_data[i]>threshold*third_Q )[,1]
  table(intermediate)
  logical_list <- Reduce(`+`, list(logical_list,intermediate)) > 0
  i  = i + 1
  if (i == length(outliers_data))
    break()
}
row <- table(logical_list)
outlier_percent <- row[2]*100/(row[1]+row[2])
#outlier_percent

cleaned_data <- cleaned_data[!logical_list,]

####################
# End of outlier removal

# Taking the log of monatry value (Monthly Income)
cleaned_data$MonthlyIncome = log(cleaned_data$MonthlyIncome)

# Dividing the data between training and testing (80%-20%)
limit = round(0.8*(dim(cleaned_data)[1]))
total = dim(cleaned_data)[1]
train_data = as.data.frame(cleaned_data[1:limit,])
test_data = cleaned_data[(limit+1):total,]


# End of Data Pre-Processing
#################################################################################

 
# Feature Selection
#################################################################################

# FWD Feature Selection for Logistic Regression

# Removing attrition column
col_names = colnames(train_data)[-2]

# The following while loop adds all the variables available in col_names one by one
# the model is created and its coefficients p-value and p-seudo-R squarred values are saved
# the output of the itertaions from 1 to 7 variables is available in the FWD directory
# where the files are named after the iteration number, 01.csv - 02.csv
# each file has the following headers
# P_Value     Pseudo_R    Col_name
# the selection criteria was based on keeping a p-value > 0.05 along with a higher pseudo_R

# Once a variable has been chosen, we add it manually to the formula, and keep one variable

count = 1
p_list = list()
r_list = list()
newdf <- data.frame(P_Value=double(),Pseudo_R=double(),Col_name = character())

while(count < length(col_names))
{
  var_name = col_names[count]
  
  factors <- c(var_name,"TotalWorkingYears","YearsAtCompany","Age",
               "BusinessTravel","MaritalStatus","NumCompaniesWorked","Department",
               "Education")
  
  mylogit <- glm(as.formula(paste("Attrition ~ ", paste(factors, collapse="+"))),
                 data =train_data, family=binomial(link="logit"),
                 na.action=na.pass) 
  # print(unname(summary(mylogit)$coef[,"Pr(>|z|)"])) 
  # str(summary(mylogit)$coef[,"Pr(>|z|)"])
  p_value = unname(summary(mylogit)$coef[,"Pr(>|z|)"][2])
  p_list <- c(p_list,p_value)
  
  pseudo_R = 1 - (summary(mylogit)$deviance)/(summary(mylogit)$null.deviance)
  r_list <- c(r_list,pseudo_R)
  
  de<-data.frame(p_value,pseudo_R,var_name)
  names(de)<-c("P_Value","Pseudo_R","Col_name")
  
  newdf <- rbind(newdf, de)
  count = count + 1
}
write.csv(newdf, file = "FWD/09.csv")

# Features that made the list from FWD Selection
# "TotalWorkingYears","YearsAtCompany","Age",
# "BusinessTravel","MaritalStatus","NumCompaniesWorked","Department",
# "Education"

# As we will see later these features will not give an accuracy higher than 80%
# let's try bi-directional elimination from an R package.
# This package uses a different comparison criteria which is the AIC (Akaike’s Information Criteria)


# Stepwise Regression
library(MASS)
fit <- glm(as.formula(paste("Attrition ~ ", paste(col_names, collapse="+"))),
           data =train_data, family=binomial(link="logit"),
           na.action=na.pass)
step <- stepAIC(fit, direction="both")
step$anova # display results

# The package gave us the Final Model:
#   Attrition ~ Age + BusinessTravel + Department + Education + JobRole + 
#   MaritalStatus + NumCompaniesWorked + PercentSalaryHike + 
#   StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
#   YearsSinceLastPromotion + YearsWithCurrManager

# These features will be proven to be better than the FWD selection features
# later on when deciding between classifiers.

# End of Feature Selection
#####################################################################################

# Logistic Regression

factors <- c("TotalWorkingYears","YearsAtCompany","Age",
             "BusinessTravel","MaritalStatus","NumCompaniesWorked","Department",
             "Education")

mylogit <- glm(as.formula(paste("Attrition ~ ", paste(factors, collapse="+"))),
               data =train_data, family=binomial(link="logit"),
               na.action=na.pass)

mylogit <- glm(Attrition ~ Age + BusinessTravel + Department + Education + EducationField +
                 JobRole + MaritalStatus + MonthlyIncome + NumCompaniesWorked +
                 StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear +
                 YearsAtCompany + YearsWithCurrManager,
               data =train_data, family=binomial(link="logit"),
               na.action=na.pass)
print((summary(mylogit)))


pred = predict(mylogit, type="response") 
#predObj = prediction(pred, train_data$Attrition)


#rocObj= performance(predObj,  measure="rec")  # creates ROC curve obj
#plot(rocObj,col="blue",ylab = "",ylim=range(0:1))
#rocObj = performance(predObj,  measure="acc")  # creates ROC curve obj
#par(new=TRUE)
#plot(rocObj,col="red", axes = FALSE,ylab = "",ylim=range(0:1))
#rocObj = performance(predObj,  measure="f")  # creates ROC curve obj
#par(new=TRUE)
#plot(rocObj,col="green", axes = FALSE,ylab = "",ylim=range(0:1))
#par(new=FALSE)

# press esc to close the next command once you run it (on the plot)
# locator()

# auc
#rocObj = performance(predObj,  measure="tpr", x.measure="fpr")  # creates ROC curve obj
#aucObj = performance(predObj, measure="auc")  # auc object
#auc = aucObj@y.values[[1]]
#plot(rocObj, main = c("AUC= ",auc))


# train accuracy
#rocObj= performance(predObj,  measure="f")  # creates ROC curve obj
#max_ = which.max(na.omit(rocObj@y.values[[1]]))
#f = unname((rocObj@x.values)[[1]])[max_]

# taking the threshold to be the argmax(max f-value)
# f-measure takes into account recall and precision
train_results <- ifelse(pred >= 0.5,"Yes","No")
train_accuracy <- ifelse(train_results == train_data$Attrition,1,0)
train_accuracy <- sum(train_accuracy)*100/length(train_results)
matrix <- table(train_results,train_data$Attrition)
recall <- matrix[2,2] / (matrix[2,2] + matrix[1,2] )


# test accuracy
pred = predict(mylogit,newdata = test_data, type="response")
#predObj = prediction(pred, test_data$Attrition) # prediction object needed by ROCR
#rocObj = performance(predObj,  measure="acc")  # creates ROC curve obj
#plot(rocObj, main = "Accuracy")

test_results <- ifelse(pred >= 0.5,"Yes","No")
test_accuracy <- ifelse(test_results == test_data$Attrition,1,0)
test_accuracy <- sum(test_accuracy)*100/length(test_results)
matrix <- table(test_results,test_data$Attrition)

recall <- matrix[2,2] / (matrix[2,2] + matrix[1,2] )

#rocObj = performance(predObj,  measure="tpr", x.measure="fpr")  # creates ROC curve obj
#rocObj = performance(predObj,  measure="rec")  # creates ROC curve obj
#plot(rocObj, main = paste("Area under the curve:", auc))

# End of Logistic Regression
# Train Accuracy = 80%
# Test Accuracy = 80%
###################################################################################

library(rpart) 
library(rpart.plot)# importing the library
decisionTree_regressor = rpart(as.formula(paste("Attrition ~ ", 
                               paste(col_names, collapse="+"))),data =train_data, 
                               control = rpart.control(minbucket = 10 ),
                               method="class",
                               parms=list(split='information'))
rpart.plot(decisionTree_regressor, type = 4, extra = 1)

y_pred = predict(decisionTree_regressor, newdata = train_data)

#predObj = prediction(y_pred[,1], train_data$Attrition) # prediction object needed by ROCR
#rocObj = performance(predObj,  measure="tpr", x.measure="fpr")  # creates ROC curve obj
#aucObj = performance(predObj, measure="auc")  # auc object
#auc = aucObj@y.values[[1]]
#plot(rocObj, main = c("AUC= ",auc))

# Terrible AUC 0.3
# No need to enhance this clssifier, the best will be AUC 0.5 with only one root node

train_results <- ifelse(y_pred[,1] >= 0.5,"Yes","No")
train_accuracy <- ifelse(train_results == train_data$Attrition,1,0)
train_accuracy <- sum(train_accuracy)*100/length(train_results)
matrix <- table(train_results,train_data$Attrition)
recall <- matrix[2,2] / (matrix[2,2] + matrix[1,2] )

# End of Decision Tree
# Train Accuracy = 13.8%
####################################################################################

