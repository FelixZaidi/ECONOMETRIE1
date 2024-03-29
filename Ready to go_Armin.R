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
VariancesOfAllColumns
## Variance matrix for all columns in the data frame
CovarianceMatrixOffAllColumns = cov(myDataRaw)
CovarianceMatrixOffAllColumns
## Correlation matrix for all columns in the data frame
CorrelationMatrixOffAllColumns = cor(myDataRaw)
CorrelationMatrixOffAllColumns
##Scatter plot: Democracy vs. GrowthCurrentYear
plot(Democracy, GrowthCurrentYear, 
     xlab = "Democracy", ylab = "GrowthCurrentYear", 
     main = "Scatter plot: Democracy vs. GrowthCurrentYear", 
     ylim = c(-20, 20))
##Scatter plot: Democracy vs. GrowthCurrentYear with trendline
plot(Democracy, GrowthCurrentYear, 
     xlab = "Democracy", ylab = "GrowthCurrentYear", 
     main = "Scatter plot: Democracy vs. GrowthCurrentYear", 
     ylim = c(-20, 20))
# Voeg een trendlijn toe
lm_model <- lm(GrowthCurrentYear ~ Democracy)
abline(lm_model, col = "red")
## Scatter plot: GrowthPreviousYear vs. GrowthCurrentYear with trendline
plot(GrowthPreviousYear, GrowthCurrentYear, 
     xlab = "GrowthPreviousYear", ylab = "GrowthCurrentYear", 
     main = "Scatter plot: GrowthPreviousYear vs. GrowthCurrentYear")
# Pas een lineair regressiemodel toe
lm_model <- lm(GrowthPreviousYear ~ Democracy)
# Teken een rode trendlijn
abline(lm_model, col = "red")

## Scatter plot: GrowthPreviousYear vs. Democracy
plot(Democracy, GrowthPreviousYear, 
     xlab = "Democracy", ylab = "GrowthPreviousYear", 
     main = "Scatter plot: Democracy vs. GrowthPreviousYear",
     xlim = c(-1, 1), ylim = c(-20, 20))

# Pas een lineair regressiemodel toe
lm_model <- lm(GrowthPreviousYear ~ Democracy)
# Teken een rode trendlijn
abline(lm_model, col = "red")

## Histogram: GrowthCurrentYear
hist(GrowthCurrentYear, breaks = 100, 
     main = "Histogram of GrowthCurrentYear", 
     xlab = "GrowthCurrentYear", 
     xlim = c(-20, 20))

## Heatmap: Correlation matrix
heatmap(cor(myDataRaw), symm = TRUE, main = "Heatmap: Correlation matrix")



### Step 3 NIET GEBRUIKEN MOMENTEEL
## Bereken de error term
## Bereken model estimation voor elke meting
OLSestimationsBaseLineModel = GrowthPreviousYear*0.3147 + Democracy*1.0747 - 0.0496
OLSestimationsBaseLineModel
Mu_i = GrowthCurrentYear - OLSestimationsBaseLineModel
Mu_i
## exspected error term 1ste orde conditie 1
mean(Mu_i)
## 1ste orde conditie 2
SecondorderConditDemocracy = sum(Mu_i*Democracy)/6150
SecondorderConditDemocracy
SecondorderConditGrowthPreviousYear = sum(Mu_i*GrowthPreviousYear)/6150
SecondorderConditGrowthPreviousYear
## Correlatie tussen variabelen
correlationMu_i_Democracy = cor(Mu_i,Democracy)
correlationMu_i_Democracy
correlationMu_i_GrowthPreviousYear = cor(Mu_i,GrowthPreviousYear)
correlationMu_i_GrowthPreviousYear
## Exspected Mu_i x Xi
ExspectedMu_ixDemocracy = mean(Democracy*Mu_i)
ExspectedMu_ixDemocracy
ExspectedMu_ixGrowthpreviousyear = mean(GrowthPreviousYear*Mu_i)
ExspectedMu_ixGrowthpreviousyear


##### NIET GEBRUIKEN
sample_indices <- sample(nrow(myDataRaw), size = 100, replace = FALSE)
# Selecteer de rijen van het dataframe met behulp van de willekeurig gekozen indices
sample_data <- myDataRaw[sample_indices, ]
sample_data
# Stel dat 'myDataRaw' de naam van je dataframe is
# en 'n' het aantal waarnemingen dat je wilt nemen in elke steekproef
n <- 100
num_samples <- 100

# Maak een lege lijst om de steekproeven op te slaan
sample_list <- list()

# Herhaal het proces van het nemen van een steekproef '5' keer
for (i in 1:5) {
  # Genereer willekeurige steekproefindices
  sample_indices <- sample(nrow(myDataRaw), size = 100, replace = FALSE)
  # Selecteer de rijen van het dataframe met behulp van de willekeurig gekozen indices
  sample_data <- myDataRaw[sample_indices, ]
  # Voeg de steekproef toe aan de lijst
  sample_list[[i]] <- sample_data
}
sample_list[[1]]
sample_list[[2]]
sample_list[[3]]

## Histogram: GrowthCurrentYear
hist(Mu_i, breaks = 100, 
     main = "Histogram of Mu_i", 
     xlab = "Mu_i", 
     xlim = c(-20, 20))

jarque.test(Mu_i)
var(Mu_i)

s = skew(Mu_i)
k = kurtosi(Mu_i)
JB = n*(((s^2)/6) + (((k-3)^2)/24))
n <- length(Mu_i)



