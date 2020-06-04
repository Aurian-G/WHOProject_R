library(tidyverse)
library(car)
library(naniar)
library(corrplot)

#Set WD
setwd("C:/Users/Aurian/Documents/SMU_Git/MSDS/STATS6372/Project1")

#Read in csv file
df <- read.csv(file = "Life_Expectancy_Data.csv", header=TRUE)

#Subset df so just Year = 2014 data is shown
df2014 <- df[df['Year'] == 2014,]

#Check out summary statistics
head(df2014)
summary(df2014)
str(df2014)

#Check NA's
#Population has 41 missing and GDP with 28
gg_miss_var(df)

##Corr plots - Initial EDA
#Filter NA's
df2014 <- df2014[complete.cases(df2014),]

#Reset Status Factor to integers. 1=developed, 2=developing
df2014$Status = as.integer(df2014$Status)

#Remove Year and Country as they don't relate
df2014 <- subset(df2014,select = -c(1,2))

#corrplot
#Life expectancy is corr with 0.5+ Adult Mortality, Alcohol, BMI, HIV/AIDS
#Income.composition, Schooling
SP_matrix = cor(df2014[,sapply(df2014, is.numeric)])
corrplot(SP_matrix, type="upper", method = "number",
         sig.level = 0.05, tl.cex = 0.55, number.cex=0.53)

#Check scatterplot distribution of applicable variables
pairs(~Life.expectancy + Adult.Mortality + Alcohol + BMI + HIV.AIDS
      + Income.composition.of.resources + Schooling,
      data = df2014,
      col = "blue",
      main = "Scatterplot Distribution of Explanatory and Response Variables")

##Assertions made from both plots above
#We see Schooling is correlated with income.composition
#Adult Mortality is corr with HIV.AIDS, Income.comp, and Schooling
#Alcohol is corr with Income.Comp and Schooling
#BMI Corr with Income.comp and Schooling
#Based of this analysis, we would want to fit a model without Income.comp because
#it is highly correlated with a lot of the other predictors

##Variable Transformations
#Alcohol, and HIV/Aids could benefit from a log transformation - will transform
#variables and see how the correlation plots change
df2014$Log.Alcohol = log(df2014$Alcohol)
df2014$Log.HIV.AIDS = log(df2014$HIV.AIDS)

#Check Correlation matrix
SP_matrix2 = cor(df2014[c(2,21,22)])
corrplot(SP_matrix2, type="upper", method = "number",
         sig.level = 0.05, tl.cex = 0.75, number.cex=0.75)

#Check Pair plot of new log vars
pairs(~Life.expectancy + Adult.Mortality + Log.Alcohol + BMI + Log.HIV.AIDS
      + Income.composition.of.resources + Schooling,
      data = df2014,
      col = "blue",
      main = "Scatterplot Distribution of Explanatory and Response Variables")

#Based on the above anaysis, both correlation coefficients -0.62, 0.53 increased 
#for HIV/AIDS and Alcohol to -0.78,0.56 respectively
#Will move forward with these log transformation for our model