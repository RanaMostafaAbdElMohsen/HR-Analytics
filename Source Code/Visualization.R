##################################################################################################################################
## Credits By :-
## -------------
## - Taghreed Hassan
## - Fady Nasser
##################################################################################################################################
# Libraries Used :-
# -----------------
#install.packages("")
library(dplyr)
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
##################################################################################################################################
# Set Directory :-
# ----------------
#setwd("")
setwd("C:\\Users\\Owner\\Desktop\\Cairo University\\Senior-2\\Big Data\\Project\\hr-analytics-big-data\\Source Code\\Data")
rm(list=ls())
##################################################################################################################################
# Preprocessing Data: Employee, General & Manager :-
# --------------------------------------------------
Employee_Data <- as.data.frame(read.table("employee_survey_data.csv",header=TRUE,sep=","))
General_Data <- as.data.frame(read.table("general_data.csv",header=TRUE,sep=","))
Manager_Data <- as.data.frame(read.table("manager_survey_data.csv",header=TRUE,sep=","))
Attrition <- subset(General_Data,Attrition == "Yes") # 711 Attritied out of 4410 (16%)
Managers_Attrition <- merge(x = Attrition, y = Manager_Data, by = "EmployeeID", all.x = TRUE)
Empolyee_Attrition<-merge(x = Attrition, y = Employee_Data, by = "EmployeeID", all.x = TRUE)
in_time <- as.data.frame(read.table("in_time.csv",header=TRUE,sep=","))
out_time <- as.data.frame(read.table("out_time.csv",header=TRUE,sep=","))
LogIn_Attrition <- merge(x = Attrition, y = in_time , by = "EmployeeID", all.x = TRUE)
General_Data$Over18 <- NULL
General_Data$StandardHours <- NULL
str(General_Data)
##################################################################################################################################
############################################# Attration == Yes With Employees Variables ##########################################
##################################################################################################################################
# 1) Age :- (Most Between 26 to 35) -> Signficant
# --------- 
Age <- table(Attrition$Age)
Age # 348 Employees (50%)
barplot(Age, col = "darkred", main = "Age of Employees Whose Attrition = Yes",xlab="Age",ylab="Count of Employees")
##################################################################################################################################
# 2) Department :- (Most from R&D Derpartment) -> Signficant 
# ---------------- 
names<-c("HR","R&D","Sales")
Departments <- table(Attrition$Department)
Departments # R&D = 453 (63.7%) => Sales = 201 (28.3%) => HR = 57 (8%)
barplot(Departments,main="Departments of Employees Whose Attrition = Yes", xlab = "Departments", ylab="Count of Employees",names.arg=c("HR","R&D","Sales"), col=c("blue","green","red"))
##################################################################################################################################
# 3) Education Level :- Not Signficant
# ---------------------
Education_Level <- table(Attrition$Education)
Education_Level # Bachelor (37.5%) => Master (26.2%) => College (22.3%) => Below College (11%) => Doctor (3%)
barplot(Education_Level, col = "darkred",main="Education Level of Employees Whose Attrition = Yes" ,xlab = "Education Level", ylab = "Count of Employees",names.arg=c("Below College","College","Bacelor","Master","Doctor"))
##################################################################################################################################
# 4) Education Field :- (Most are life science and medical) -> Significant
# --------------------- 
Education_Field <- table(Attrition$EducationField)
Education_Field # 42.7% from LifeScience and 31.7% from Medical 
barplot(Education_Field, col = "darkred",main="Education Field of Employees Whose Attrition = Yes" ,xlab = "Education Field", ylab = "Count of Employees",names.arg=c("Human Resources","Life Sciences","Marketing","Medical","Other","Techincal Degree"))
##################################################################################################################################
# 5) Job Involvement level :- (75% of low involvement) -> Significant
# ---------------------------
Job_Level <- table(Attrition$JobLevel)
Job_Level # 35.5% from level 1 and 40% from level 2
barplot(Job_Level, col = "darkred",main="Involvement Level of Employees Whose Attrition = Yes" ,xlab = "Empolyee Involvement Level", ylab = "Count of Employees",names.arg=c("Very Low","Low","Intermediate","High","Very High"))
##################################################################################################################################
# 6) Job role :-  Significant
# --------------
Job_Role <- table(Attrition$JobRole)
Job_Role # Sale (23.2%) => Research Scientist (22.3%) => Lab Tech (17.7%)
Job_Attrition<-data.frame(Attrition$Attrition, Attrition$JobRole)
Role <- Attrition$JobRole
ggplot(Job_Attrition,aes(x=Job_Attrition$Attrition.JobRole,fill=Role) )+geom_bar()+ggtitle("Job Role of Employees Whose Attrition = Yes") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Job Role")+ylab("Count of Employees ")
##################################################################################################################################
# 7) Percentage salary increase :- (Small salary hike tends to leave) -> Significant
# --------------------------------
Salary_Hike <- table(Attrition$PercentSalaryHike)
Salary_Hike # 11% -> 15% = 432 Employees (61%)
barplot(Salary_Hike,main="Increase in Salaray Percentage of Employees Whose Attrition = Yes" ,xlab = "Percentage of increase", ylab = "Count of Employees",col=cm.colors(10) )
##################################################################################################################################
# 8) Stock Option level :- (Low stock level tends to leave) -> Significant
# ------------------------
Stock <- table(Attrition$StockOptionLevel)
Stock # 0 - 1 = 588 Employees (83%)
barplot(Stock,col = "darkred",main="Stock Level of Employees Whose Attrition = Yes" ,xlab = "Stock Level", ylab = "Count of Employees")
##################################################################################################################################
# 9) Total working year :- (Less than or equal 10 years tends to leave more than who stayed longer) -> Not Significant
# ------------------------  
Total_Years <- table(Attrition$TotalWorkingYears)
Total_Years # 544 Employees (76.5 %)
Experience<-data.frame(Attrition$TotalWorkingYears)
Experience<-na.omit(Experience)
summary(Experience$Attrition.TotalWorkingYears)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   3.000   7.000   8.255  10.000  40.000 
ggplot(Experience,aes(x=Experience$Attrition.TotalWorkingYears) )+
  geom_histogram(alpha = 0.8,bins =39,fill="blue")+
  ggtitle("Total Working Years of Employees Whose Attrition = Yes") +theme(plot.title = element_text(hjust = 0.5)) + xlab("Number of Years") + ylab("Count of Employees ")
##################################################################################################################################
# 10) Training time last year :- (Middle timed trained employees tend to attrite) -> Not Significant
# ------------------------------
Training <- table(Attrition$TrainingTimesLastYear)
Training # 2-3 time => 540 Employees (76%)
barplot(Training,col = "darkred",main="Number of Training Times During last year of Employees Whose Attrition = Yes " ,xlab = "Training Times", ylab = "Count of Employees")
##################################################################################################################################
# 11) Check in and out time :- Not Significant
# ----------------------------
Relation_Att <- as.data.frame(read.table("Time_Difference.csv",header=TRUE,sep=","))
Merge_Attrition <- merge(x = Attrition, y = Relation_Att, by = "EmployeeID", all.x = TRUE)
vacations <- sum(colSums(is.na(Merge_Attrition)) == 711,na.rm = TRUE) #12 days was vacation by getting which days all the employes where absent
Absent <- rowSums(is.na(Merge_Attrition)) - 12 
# Working days = 365 - weekends = 261 - 12 vacation day = 249 day
max(Absent) # Max absent 23 days from 249 days
min(Absent) # Min Absent is 1 day from 249 days
##################################################################################################################################
# 12) Working average :- Not Significant
# ----------------------
Employee_RM <- as.data.frame(read.table("Working_Average.csv",header=TRUE,sep=","))
Merge_Attrition <- merge(x = Attrition, y = Employee_RM, by = "EmployeeID", all.x = TRUE)
mean(Merge_Attrition$Working_Average) #7.55 Yes and 6.86 for No
summary(Employee_RM$Working_Average)
ggplot(Merge_Attrition,aes(x=Merge_Attrition$Working_Average)) + ggtitle("Working Average of Employees Whose Attrition = Yes") +geom_histogram(bins=55)+ xlab("Average Working Time in Hours") + ylab("Count of Employees ")
##################################################################################################################################
# 13) Job involvement rated by managers :- (Employees Whose involvement rated as high instead of very high tends to leave) -> Significant
# ---------------------------------------- 
Rated_Involvement <- table(Managers_Attrition$JobInvolvement)
Rated_Involvement 
Rating_Inv<-Managers_Attrition$JobInvolvement
ggplot(Managers_Attrition,aes(x=Managers_Attrition$JobInvolvement )) + geom_bar(fill=c("red","blue","green","yellow"))+ggtitle("Involvement rating of Employees Whose Attrition = Yes") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Involvement Rating")+ ylab("Count of Employees")
##################################################################################################################################
# 14) Performance rating rated by managers :- (Employees Whose Performance rated as excellent instead of outstanding tends to leave) -> Not Significant
# ------------------------------------------- 
Performance_Rating <- table(Managers_Attrition$PerformanceRating)
Performance_Rating # 588 Employees (82.7%)
barplot(Performance_Rating,col = "#009E73",xlab = "Rating by Manager", ylab="Count of Employees" ,main="Rating vs Attrition" ,names.arg=c("Excellent","Outstanding"))
##################################################################################################################################
# 15) Business Travel :- Significant
# ----------------------
newdfm<-Attrition
summary(newdfm$BusinessTravel) # Non_travel = 36 (5%) , Travel_Freq = 207 (29%) , Travel_rarely = 468 (66%)
newdfm<-subset(newdfm, BusinessTravel!="Non-Travel")
newdfm$BusinessTravel<-droplevels(newdfm$BusinessTravel)
levels(newdfm$BusinessTravel)
summary(newdfm$BusinessTravel)  #Travel_Freq = 207 (30%), Travel_rarely = 468 (70%)
ggplot(newdfm, aes(x = BusinessTravel) ) + geom_bar(fill ="Blue") + ggtitle("Business Travel Number of Employees Whose Attrition = Yes")+theme(plot.title = element_text(hjust = 0.5))
##################################################################################################################################
# 16) Gender :- (62% Male , 38% Female) -> Significant
# -------------
newdfm<-Attrition
Gender_Attrition<-data.frame(newdfm$Attrition, newdfm$Gender)
z<-newdfm[newdfm$Attrition=="Yes", c('Gender')]
result<-summary(z)
alabels <- round((result/sum(result)) * 100)
alabels <- paste(alabels, "%", sep="")
pie(result,main = "Precentage of Males and Females of Employees Whose Attrition = Yes",labels=alabels,col=c("red","blue"))
legend(1, 1.0, c("Males", "Females"), fill=c("blue","red"))
##################################################################################################################################
# 17) Marital Status :- Significant
# --------------------
Marital_Attrition<-data.frame(newdfm$Attrition, newdfm$MaritalStatus)
table(Marital_Attrition) # Divorced = 99 (14%) , Married = 252 (35%) , Single = 360 (50%)
ggplot(Marital_Attrition, aes( newdfm$MaritalStatus ,fill=newdfm$MaritalStatus) ) + geom_bar()+xlab("Marital Status") +scale_fill_discrete(name = "Marital Status")+ ggtitle("Marital Status of Employees Whose Attrition = Yes") +theme(plot.title = element_text(hjust = 0.5))
##################################################################################################################################
# 18) Years At Company :- (Employees who spent less than 15 years at company tends more to leave the company ) -> Signficant
# ----------------------
summary(newdfm$YearsAtCompany)
attr_yearsAtCompany<-data.frame(newdfm$Attrition,newdfm$YearsAtCompany)
ggplot(attr_yearsAtCompany, aes(x = newdfm$YearsAtCompany) ) + geom_density(position ="identity", alpha = 1, fill="orange") +  xlab("Years")+ggtitle("Distribution of The Years Spent by Employees Whose Attrition = Yes") +theme(plot.title = element_text(hjust = 0.5))
##################################################################################################################################
# 19)YearsWithCurrManager :- (Employees who spent less than 7 years with their current managers leave the country) -> Significant
# -------------------------
summary(newdfm$YearsWithCurrManager)
YearsWithManager<-subset(newdfm,newdfm$YearsWithCurrManager > 0)
summary(YearsWithManager$YearsWithCurrManager) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   2.000   3.000   4.447   7.000  14.000 
table(YearsWithManager$Attrition,YearsWithManager$YearsWithCurrManager)
ggplot(YearsWithManager, aes(x =YearsWithManager$YearsWithCurrManager) ) + geom_histogram(alpha = 0.8,bins =45,fill="#D55E00")+ xlab("years")+ ggtitle("Years Spent With The Current Manager by Employees Whose Attrition = Yes ") +theme(plot.title = element_text(hjust = 0.5))
##################################################################################################################################
# 20)Job Satisfication :- Not Significant
# -----------------------
Empolyee_Attrition$JobSatisfaction
Employee_Satisfaction<-na.omit(Empolyee_Attrition$JobSatisfaction)
table(Empolyee_Attrition$JobSatisfaction,Empolyee_Attrition$Attrition)
barplot(table(Empolyee_Attrition$Attrition,Empolyee_Attrition$JobSatisfaction))
##################################################################################################################################
############################################# Relation Between Top Correlated Variables ##########################################
##################################################################################################################################
# Relation Between Travel and Department :-
----------------------------------------------
table(Attrition$BusinessTravel,Attrition$Department)
newdfm<-Attrition
summary(newdfm$BusinessTravel)
newdfm<-subset(newdfm, BusinessTravel!="Non-Travel")
newdfm$BusinessTravel<-droplevels(newdfm$BusinessTravel)
levels(newdfm$BusinessTravel)
ggplot(newdfm, aes( x= newdfm$Department ,fill=newdfm$BusinessTravel) ) + geom_bar()+ xlab("Department") +scale_fill_discrete(name = "Business Travel")+ylab("Count of employees") + ggtitle("Business Travel vs Department") +theme(plot.title = element_text(hjust = 0.5))
############################################################################################################################################
###########################################unsuccessful trials###########################################################################

############################################################# Clustering #########################################################
##################################################################################################################################
plot(Attrition$MonthlyIncome)

#"scale " ->Divide by standard deviation
income<-normalize(Attrition$MonthlyIncom, method = "scale", range = c(0, 1))
summary(income)

YearsCompany<-normalize(Attrition$YearsAtCompany, method = "scale", range = c(0, 1))
summary(YearsCompany)
plot(x=income,y=YearsCompany,col=c("red", "blue"))

tt<-data.frame(Attrition$YearsAtCompany,Attrition$YearsSinceLastPromotion)

#corr o.7
cor(Attrition$YearsAtCompany,Attrition$YearsSinceLastPromotion)

ggplot(tt, aes(y=Attrition$YearsAtCompany,x =Attrition$YearsSinceLastPromotion) ) +
  geom_point()+geom_smooth(method = "lm") 

# another way to examine the best number of clusters 
nc <- NbClust(Attrition$MonthlyIncome, min.nc=2, max.nc=15, method="kmeans")
# see the voting results 
table(nc$Best.n[1,])
cl<-kmeans(Attrition$MonthlyIncome, 6, iter.max=15)
print(cl$centers)
plot(Attrition$MonthlyIncome, col = cl$cluster)
points(cl$centers, col = "black",pch=17)
cl$size

