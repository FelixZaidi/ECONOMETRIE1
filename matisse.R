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
setwd("C:/Users/dirkr/OneDrive/Documenten/SCHOOL MATISSe/JAAR 3 SEM 2/Econometrie/Input")

## For Mac
## setwd("/users/ymeersch/Desktop")

## RStudio through Athena, path corresponds to the desktop or other folder on your H-drive
## For Windows
## setwd("\\Desktop")

## For Mac
## setwd("/Desktop")

####################################################################
## Set output file directory: change the path to your own pc directory
## The output tables are created in this directory
####################################################################

## Rstudio installed on your pc
## For Windows
output="C:/Users/dirkr/OneDrive/Documenten/SCHOOL MATISSe/JAAR 3 SEM 2/Econometrie/Output"

## For Mac
## output="/users/ymeersch/Desktop"

## RStudio through Athena, path corresponds to the desktop or other folder on your H-drive 
## For Windows
## output = "\\Desktop"

## For Mac
## output = "/Desktop"

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
install.packages("C:/Users/dirkr/OneDrive/Documenten/SCHOOL MATISSe/JAAR 3 SEM 2/Econometrie/Input/EconometricsUGent_1.0.tar.gz", source = TRUE, repos = NULL)

## For Mac
## install.packages("/users/ymeersch/Desktop/EconometricsUGent_1.0.tar.gz", type="source", repos = NULL)

## RStudio through Athena
## For Windows
## install.packages("H://Desktop//EconometricsUGent_1.0.tar.gz", type="source", repos = NULL, lib = output) 

## For Mac
## install.packages("H:/Desktop/EconometricsUGent_1.0.tar.gz", type="source",repos = NULL, lib = output) 


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
myDataRaw <- read.table("C:/Users/dirkr/OneDrive/Documenten/SCHOOL MATISSe/JAAR 3 SEM 2/Econometrie/Input/Data.csv", header = TRUE, sep = ",")
head(myDataRaw)

## Load data from csv file
Growth=myDataRaw[,3]
Democraty=myDataRaw[,8]
Growth1=myDataRaw[,4]

## OLS estimation using lm package
reg1=lm(Growth~Democraty+Growth1)                        # Estimation
stargazer(reg1,type="text",digits = 4,style="all") # Make table with results
vcov(reg1)                                         # Show variance-covariance matrix

summary(myDataRaw)
plot(Growth1, Growth, xlab = "Growth1", ylab = "GDP Growth", main = "Scatter plot: Democracy vs. GDP Growth")


ols <- lm(Growth ~ Growth1+ Democraty, data = myDataRaw)
summary (ols)
t-test <- t.test(myDataRaw, alternative = "less")

var(myDataRaw)
mean(myDataRaw)