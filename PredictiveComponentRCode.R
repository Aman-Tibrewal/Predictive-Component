#If p-value is less than 0.05 then accept alt and reject null.
#If p-value is more than 0.05 then accept null and reject alt.

#To save the data file in the current working directory ####################
save(EmployeeData, file="EmployeeData.RData")

#Packages and Data File Activation ---------------------------------------

#Package Activation
library(DescTools) #For Mode
library(dplyr) #For Pipe Operator
library(ggplot2) #For QQ Plot
library(moments) #For Skewness & Kurtosis
library(rstatix) #For Welch ANOVA Test
library(Hmisc) #For rcorr Function
library(QuantPsyc) #For lm.beta function
library(ggpubr) #For advanced QQ Plots

#Attaching Data Files
attach(EmployeeData)

#Normality Test ----------------------------------------------------------

DataSkewness <- EmployeeData %>% 
  summarise(skewness(satisfaction_level),
            skewness(last_evaluation),
            skewness(number_project),
            skewness(average_montly_hours),
            skewness(time_spend_company),
            skewness(Log_TimeSpent),
            skewness(Inv_TimeSpent),
            skewness(Imp_timeSpend))
DataKurtosis <- EmployeeData %>% 
  summarise(kurtosis(satisfaction_level),
            kurtosis(last_evaluation),
            kurtosis(number_project),
            kurtosis(average_montly_hours),
            kurtosis(time_spend_company),
            kurtosis(Log_TimeSpent),
            kurtosis(Inv_TimeSpent),
            kurtosis(Imp_timeSpend))
as.data.frame(DataSkewness)            
as.data.frame(DataKurtosis)

write.csv(as.data.frame(DataKurtosis),
          file = "DataKurtosis.csv")
write.csv(as.data.frame(DataSkewness),
          file = "DataSkewness.csv")

##Outlier Test -------------------------------------------------------------

boxplot(satisfaction_level, main="satisfaction_level")
boxplot(last_evaluation, main="last_evaluation")
boxplot(number_project, main="number_project")
boxplot(average_montly_hours, main="average_montly_hours")
boxplot(time_spend_company, main="time_spend_company")

boxplot(average_montly_hours~ Work_accident, main="Work_accident")
boxplot(average_montly_hours~ left, main="left")
boxplot(average_montly_hours~ promotion_last_5years, main="promotion_last_5years")
boxplot(average_montly_hours~ Dept_Convert, main="Dept_Convert")
boxplot(average_montly_hours~ Salary_Convert, main="Salary_Convert")

#Outliers and High Skewness detected in Time Spent Variable
#Transformation (Log) attempted
boxplot(Log_TimeSpent, main="Log_TimeSpent")
skewness(Log_TimeSpent)
kurtosis(Log_TimeSpent)
#outliers were still there
#Transformation (Inverse) attempted
Inv_TimeSpent <- 1/(time_spend_company)
boxplot(Inv_TimeSpent, main="inv_TimeSpent")
skewness(Inv_TimeSpent)
kurtosis(Inv_TimeSpent)
#Even after Transformation, although the skewness and kurtosis values were bought down significantly
#The skewness was less in inverse compared to log
#But the outliers were still there thus, we have created a new Time Spent Variable
#With Imputed data (Median Values) for the Time Spent Variable

boxplot.stats(time_spend_company)
mean(time_spend_company)
median(time_spend_company)
Mode(time_spend_company)
Imp_timeSpend <- time_spend_company
Imp_timeSpend[Imp_timeSpend >= 6] <- 3
boxplot(Imp_timeSpend)
boxplot.stats(Imp_timeSpend)
skewness(Imp_timeSpend)
kurtosis(Imp_timeSpend)
