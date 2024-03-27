####################################################################
####################################################################
###                   Case Econometrics                          ###
### Bachelor of Science in Economics and Business Engineering    ###
###         Academic year 2023-24 Ghent University               ###
####################################################################
####################################################################


rm(list = ls())   # Clear workspace 

####################################################################
## Set input file directory: change the path to your own pc directory
## !! Note: EconometricsUGent package and dataset are saved in this 
## input file directory
####################################################################

## Rstudio installed on your pc
## For Windows
setwd("C:/Users/Brent Bogemans/OneDrive/Desktop")

####################################################################
## Set output file directory: change the path to your own pc directory
## The output tables are created in this directory
####################################################################

## Rstudio installed on your pc
## For Windows
output="C:/Users/Brent Bogemans/OutputEcoRcode"

####################################################################
## Install required packages: this only needs to be done the first time
## before you use these packages
####################################################################
if(!require(pastecs)){install.packages("pastecs")}
if(!require(psych)){install.packages("psych")}
if(!require(moments)){install.packages("moments")}
if(!require(lmtest)){install.packages("lmtest")}
if(!require(sandwich)){install.packages("sandwich")}
if(!require(AER)){install.packages("AER")}
if(!require(stargazer)){install.packages("stargazer")}
if(!require(nlme)){install.packages("nlme")}
if(!require(orcutt)){install.packages("orcutt")}
if(!require(openxlsx)){install.packages("openxlsx")}
if(!require(fastDummies)){install.packages("fastDummies")}
if(!require(datawizard)){install.packages("datawizard")}

####################################################################
## To install the EconometricsUGent-package, execute the following steps:
## 1. Copy-paste the EconometricsUGent_1.0.tar.gz file to your desktop
## 2. Change the part "C:\\users\\ymeersch\\Desktop" in the code below
##    so that it corresponds with your pc directory to the desktop
####################################################################

## Rstudio installed on your pc
## For Windows
install.packages("C:/Users/Brent Bogemans/OneDrive/Desktop/RcodeEconometrics/EconometricsUGent_1.0.tar.gz", source = TRUE, repos = NULL)


####################################################################
## Load required packages: this needs to be done every time you
## close an R-session
####################################################################
library(pastecs)     ## Descriptive Statistics
library(psych)       ## Correlation plot
library(moments)     ## Test for Normality
library(lmtest)      ## Test for heteroscedasticity
library(sandwich)    ## Newey-West HAC estimators
library(AER)         ## 2SLS
library(stargazer)   ## Stargazer
library(orcutt)      ## Cochrane-Orcutt EGLS
library(nlme)        ## Linear models with autocorrelated error terms
library(openxlsx)    ## To read an excel xlsx data file
library(fastDummies) ## Create dummies based on different categories in a variable
library(datawizard)  ## Data transformations (e.g. demeaning by group)
library(EconometricsUGent)  ## Additional functions


############################################
## Load data from csv file
############################################
myDataRaw <- read.table("C:/Users/Brent Bogemans/OneDrive/Desktop/RcodeEconometrics/Data.csv", header = TRUE, sep = ",")
head(myDataRaw)

installed.packages()["EconometricsUGent", ]
detach("package:EconometricsUGent", unload = TRUE)
install.packages("C:/Users/Brent Bogemans/OneDrive/Desktop/RcodeEconometrics/EconometricsUGent_1.0.tar.gz", source = TRUE, repos = NULL)










### Step 0
## Load data from csv file
GrowthCurrentYear = myDataRaw[,3]
Democracy = myDataRaw[,8]
GrowthPreviousYear = myDataRaw[,4]

## OLS estimation using lm package
# Estimation
RegBaselineModel = lm(GrowthCurrentYear ~ Democracy + GrowthPreviousYear)
# Make table with results
stargazer(RegBaselineModel, type="text", digits = 4, style="all")
# Show variance-covariance matrix
vcov(RegBaselineModel) 










### Step 1
## Summary of the dataset
summary(myDataRaw)
## Structure of the dataset
str(myDataRaw)
## First Values of dataset
head(myDataRaw)
## Variance of all columns in the data frame
VariancesOfAllColumns = var(myDataRaw)
## Variance matrix for all columns in the data frame
CovarianceMatrixOffAllColumns = cov(myDataRaw)
## Correlation matrix for all columns in the data frame
CorrelationMatrixOffAllColumns = cor(myDataRaw)
##Scatter plot: Democracy vs. GDP Growth
plot(Democracy, GrowthCurrentYear, xlab = "Democracy", ylab = "GDP Growth", main = "Scatter plot: Democracy vs. GDP Growth")
## Scatter plot: Democracy vs. GDP Growth
plot(GrowthPreviousYear, GrowthCurrentYear, xlab = "GDP Growth Previous Year", ylab = "GDP Growth", main = "Scatter plot: GDP Growth Previous Year vs. GDP Growth")
## Histogram: GDP Growth
hist(GrowthCurrentYear, breaks = 20, main = "Histogram of GDP Growth", xlab = "GDP Growth")
# Heatmap: Correlation matrix
heatmap(cor(myDataRaw), symm = TRUE, main = "Heatmap: Correlation matrix")

 








# Means of variables
means <- sapply(myDataRaw, mean)
means

# Variances of variables
variances <- sapply(myDataRaw, var)

# Covariance matrix of variables
covariance_matrix <- cov(myDataRaw)



# Degrees of freedom
df <- 171

# Significance level
alpha <- 0.05

# Compute the critical value for the lower tail (one-sided)
critical_value <- qt(1 - alpha, df)

critical_value
