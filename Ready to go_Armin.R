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



## For Mac
setwd("/Users/arminabbaspourtehrani/Documents/Universiteit/Academiejaar\ 2023-2024/Econometrie/Input\ Eco")

####################################################################
## Set output file directory: change the path to your own pc directory
## The output tables are created in this directory
####################################################################


## For Mac
output="/Users/arminabbaspourtehrani/Documents/Universiteit/Academiejaar\ 2023-2024/Econometrie/Output\ Eco "



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



## For Mac
install.packages("/Users/arminabbaspourtehrani/Documents/Universiteit/Academiejaar 2023-2024/Econometrie/EconometricsUGent_1.0.tar", type="source", repos = NULL)

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
myDataRaw <- read.table("/Users/arminabbaspourtehrani/Documents/Universiteit/Academiejaar\ 2023-2024/Econometrie/Input\ Eco/Data.csv", header = TRUE, sep = ",")
head(myDataRaw)




