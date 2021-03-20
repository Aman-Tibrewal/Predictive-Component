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
            skewness(Work_accident),
            skewness(left),
            skewness(promotion_last_5years),
            skewness(Dept_Convert),
            skewness(Salary_Convert),
            skewness(Log_TimeSpent))
DataKurtosis <- EmployeeData %>% 
  summarise(kurtosis(satisfaction_level),
            kurtosis(last_evaluation),
            kurtosis(number_project),
            kurtosis(average_montly_hours),
            kurtosis(time_spend_company),
            kurtosis(Work_accident),
            kurtosis(left),
            kurtosis(promotion_last_5years),
            kurtosis(Dept_Convert),
            kurtosis(Salary_Convert),
            kurtosis(Log_TimeSpent))
as.data.frame(DataSkewness)            
as.data.frame(DataKurtosis)

write.csv(as.data.frame(DataKurtosis),
          file = "DataKurtosis.csv")
write.csv(as.data.frame(DataSkewness),
          file = "DataSkewness.csv")

boxplot(satisfaction_level)
boxplot(last_evaluation)
boxplot(number_project)
boxplot(average_montly_hours)
boxplot(time_spend_company)
boxplot(Work_accident)
boxplot(left)
boxplot(promotion_last_5years)
boxplot(dept)
boxplot(salary)
boxplot(Dept_Convert)
boxplot(Salary_Convert)
boxplot(Log_TimeSpent)