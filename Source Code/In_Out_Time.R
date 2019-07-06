##################################################################################################################################
# Set Directory
# --------------
#setwd("F:\\Faculty of Engineering\\Senior-2\\CMPN451 - Big Data\\BigData Project\\Data")
setwd("C:\\Users\\Owner\\Desktop\\Cairo University\\Senior-2\\Big Data\\Project\\hr-analytics-big-data\\Source Code\\Data")
rm(list=ls())
##################################################################################################################################
# Preprocessing Data: Employee, General & Manager 
# ------------------------------------------------
Employee_Data <- as.data.frame(read.table("employee_survey_data.csv",header=TRUE,sep=","))
General_Data <- as.data.frame(read.table("general_data.csv",header=TRUE,sep=","))
Manager_Data <- as.data.frame(read.table("manager_survey_data.csv",header=TRUE,sep=","))
in_time <- as.data.frame(read.table("in_time.csv",header=TRUE,sep=","))
out_time <- as.data.frame(read.table("out_time.csv",header=TRUE,sep=","))
# Attrition <- subset(General_Data,Attrition == "Yes") # 711 Attritied out of 4410 (16%)
##################################################################################################################################
# a) Get Working Hours For Each Employee :-
# ---------------------------------------
Att <- subset(General_Data, select = c("EmployeeID"))
intime_Attrition <- merge(x = Att, y = in_time, by.x = "EmployeeID", by.y = "X", all.x = TRUE)
outtime_Attrition <- merge(x = Att, y = out_time, by.x = "EmployeeID", by.y = "X", all.x = TRUE)
Relation_Att <- intime_Attrition
i <- 2
while (i <= 262) {
  t1 <- as.POSIXct(intime_Attrition[[i]])
  t2 <- as.POSIXct(outtime_Attrition[[i]])
  d <- data.frame(t1= t1, t2= t2)
  Relation_Att[[i]] = round(d$t2-d$t1,digits=2)
  i = i+1
}
write.csv(Relation_Att, "Time_Difference.csv",row.names = FALSE)
##################################################################################################################################
# b) Get Average Working Time For Employees
# -------------------------------------------------------------------
Relation_Att <- as.data.frame(read.table("Time_Difference.csv",header=TRUE,sep=","))
Relation_Att[is.na(Relation_Att)] = 0
Working_Average <- rowMeans(Relation_Att[,2:262], na.rm=FALSE)
Employee_ID <- subset(General_Data,select = c("EmployeeID"))
Employee_RM <- cbind(Employee_ID,Working_Average)
write.csv(Employee_RM, "Working_Average.csv",row.names = FALSE)
##################################################################################################################################
# c) Get Average Work for The Attrited and non Attriated Employees :- 
# -------------------------------------------------------------------
Employee_RM <- as.data.frame(read.table("Working_Average.csv",header=TRUE,sep=","))
Attrition_Yes <- subset(General_Data, Attrition == "Yes", select = c("EmployeeID"))
Attrition_No <- subset(General_Data, Attrition == "No", select = c("EmployeeID"))
Merge_Yes_Attrition <- merge(x = Attrition_Yes, y = Employee_RM, by = "EmployeeID", all.x = TRUE)
Merge_No_Attrition <- merge(x = Attrition_No, y = Employee_RM, by = "EmployeeID", all.x = TRUE)
mean(Merge_Yes_Attrition$Working_Average) #7.55
mean(Merge_No_Attrition$Working_Average) #6.86
##################################################################################################################################

