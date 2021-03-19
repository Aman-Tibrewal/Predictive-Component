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
