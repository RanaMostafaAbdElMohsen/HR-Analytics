# Libraries Used :-
# -----------------
#install.packages("")
library(plyr)
library("e1071")
library(rattle)
library(NbClust)
library(cluster)
library(HSAUR)
library(BBmisc)
library(polycor)
library(ggplot2)
library(auto.pca)
library(arules)
library(arulesViz)
library(RColorBrewer)
#################################################################################################
# Set Directory :-
# ----------------
#setwd("")
#setwd("D:/Cufe Courses/Big Data/Project/hr-analytics-big-data")
#setwd("F:\\Faculty of Engineering\\Senior-2\\CMPN451 - Big Data\\BigData Project\\Data")
setwd("C:\\Users\\Owner\\Desktop\\Cairo University\\Senior-2\\Big Data\\Project\\hr-analytics-big-data\\Source Code\\Data")
rm(list=ls())
###################################################################################################
# Preprocessing Data: Employee, General & Manager :-
# --------------------------------------------------
Employee_Data <- as.data.frame(read.table("employee_survey_data.csv",header=TRUE,sep=","))
General_Data <- as.data.frame(read.table("general_data.csv",header=TRUE,sep=","))
Manager_Data <- as.data.frame(read.table("manager_survey_data.csv",header=TRUE,sep=","))
Attrition <- subset(General_Data,Attrition == "Yes") # 711 Attritied out of 4410 (16%)
Managers_Attrition <- merge(x = Attrition, y = Manager_Data, by = "EmployeeID", all.x = TRUE)
Empolyee_Attrition<-merge(x = Attrition, y = Employee_Data, by = "EmployeeID", all.x = TRUE)
General_Data$Over18 <- NULL
General_Data$StandardHours <- NULL
str(General_Data)
##############################################################################################################
##################################################################################################################################
######################################################## Assosication Rules ######################################################
##################################################################################################################################
summary(Attrition)
Age<-discretizeDF(Attrition$Age)
YearsAtCompany<-discretizeDF(Attrition$YearsAtCompany)
summary(YearsAtCompany)
Experience<-discretizeDF(Attrition$TotalWorkingYears)
rating<-Managers_Attrition$JobInvolvement

ratingByManager<-mapvalues(Managers_Attrition$JobInvolvement,from = c(1,2,3,4),to = c("Low", "Medium", "High", "Very High"))

summary(Attrition$YearsWithCurrManager)

Attrition$YearsWithCurrManagerCateogry[Attrition$YearsWithCurrManager== 0 | Attrition$YearsWithCurrManager ==1 ] <- "0->1"
Attrition$YearsWithCurrManagerCateogry[Attrition$YearsWithCurrManager > 1 & Attrition$YearsWithCurrManager <=4 ]<-"2->4"
Attrition$YearsWithCurrManagerCateogry[Attrition$YearsWithCurrManager > 4  & Attrition$YearsWithCurrManager <=7 ]<-"5->7"
Attrition$YearsWithCurrManagerCateogry[Attrition$YearsWithCurrManager> 7 ]<-"7->14"

dfm_association<-data.frame(Attrition$Attrition,Attrition$Gender,Attrition$BusinessTravel,
                            Attrition$JobRole,Attrition$MaritalStatus,Age,YearsAtCompany,
                            Experience,ratingByManager,Attrition$YearsWithCurrManagerCateogry)
str(dfm_association)
dfm1 <- as(dfm_association, "transactions")

inspect(head(dfm1))
freq <- itemFrequency(dfm1)
freq <- sort(freq,decreasing = TRUE)
freq[0:2]
itemFrequencyPlot(dfm1,topN = 5,col=brewer.pal(8,'Pastel2'),main="Top Frequent Items")

rule <- apriori(dfm1,parameter = list(supp = 0.1, conf = 0.5,minlen=2))
# 1365 rules from apriori Algoritm
rule_supp <- sort(rule)
inspect(head(rule_supp),n=10)
rule_conf <- sort(rule,by = "confidence")
inspect(head(rule_conf,n=6))

rule_lift <- sort(rule,by = "lift")
inspect(head(rule_lift,n=20))

plot(rule,measure=c("support", "confidence"),shading = "lift")
plot(rule)

