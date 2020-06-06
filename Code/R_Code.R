library(tidyverse)
library(car)
library(naniar)
library(corrplot)
library(xlsx)
library(olsrr)

#Set WD
setwd("C:/Users/Aurian/Documents/SMU_Git/MSDS/STATS6372/Project1")

#Read in csv file
df <- read.csv(file = "Life_Expectancy_Data.csv", header=TRUE)

#Subset df so just Year = 2014 data is shown
df2014 <- df[df['Year'] == 2014,]

#Export DF to .xlsx for reference
#write.xlsx(df2014, "C:/Users/Aurian/Documents/SMU_Git/MSDS/STATS6372/Project1/Data.xlsx")

#Check out summary statistics
head(df2014)
summary(df2014)
str(df2014)

#Check NA's
#Population has 41 missing and GDP with 28
gg_miss_var(df)

##Corr plots - Initial EDA
#Filter NA's just to see general distributions
df2014 <- df2014[complete.cases(df2014),]

#Remove Year and Country as they don't relate. Status remove for now for corr plots
#Then we will come back and see if we can use it as an indicator variable if plausible
df2014 <- subset(df2014,select = -c(1,2,3))

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
#Would also be smart to fit a model without Schooling and keep Income.comp instead
#to assess the difference of fits

##Variable Transformations
#Alcohol, and HIV/Aids could benefit from a log transformation - will transform
#variables and see how the correlation plots change
df2014$Log.Alcohol = log(df2014$Alcohol)
df2014$Log.HIV.AIDS = log(df2014$HIV.AIDS)

#Check Correlation matrix
SP_matrix2 = cor(df2014[c(1,20,21)])
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
#Remove non-log variables from our df
df2014 <- subset(df2014, select = -c(4,13))

#Rerun correlation matrix and check out the new relationships
SP_matrix3 = cor(df2014[,sapply(df2014, is.numeric)])
corrplot(SP_matrix3, type="upper", method = "number",
         sig.level = 0.05, tl.cex = 0.55, number.cex=0.53)

#LOGHIV is HIGHLY correlated with Adult Mortality, Income.comp, and moderate schooling
#Income.Comp is HIGHLY corr Adult Mortality, LogAlcohol, BMI, Schooling, LogHIV
#Let's move forward with a model include all predictors
#Then run a model with out Income.comp and then run a model without Log.HIV



###Model 1 Full Model after analysis
fit1 <- lm(Life.expectancy~Adult.Mortality + BMI + Income.composition.of.resources
          + Schooling + Log.Alcohol + Log.HIV.AIDS, data = df2014)

#Check QQPLOT for normality Assumption. Seems good
ols_plot_resid_qq(fit1)

#Check Residual Histogram. Perfect
ols_plot_resid_hist(fit1)

#Check Residual vs Fitted Plots for constant variance and normality. Perfect
ols_plot_resid_fit(fit1)

#Studentized Residuals. All seem good except observation 4.
ols_plot_resid_stud(fit1)

#Studentized Residual vs Leverage plot. Observation 39 is most egregious
ols_plot_resid_lev(fit1)

#Cook's D. Observation 39 and 99 seem most suspicious. May need to fit without
#those points to see if there is collinearity being explained by said values
ols_plot_cooksd_chart(fit1)

###Overall Summary
summary(fit1)

#ASE
ase = mean(fit1$residuals^2)
ase

#AIC
AIC(fit1)

#BIC
BIC(fit1)

#Adj R-Squared = 0.8671
#ASE = 9.315
#AIC = 680.105
#BIC = 703.106
#Adult.Mort, Income.comp, Log.HIV Only significant terms
#Next model we will fit without observation 39 and see what we get.



###Model 2 w/o influential point
#39 = Equitorial Guinea. We will fit this model without this point and see how
#our summaries change
df2014_influential <- df2014[-39,]
fit2 <- lm(Life.expectancy~Adult.Mortality + BMI + Income.composition.of.resources
           + Schooling + Log.Alcohol + Log.HIV.AIDS, data = df2014_influential)

#Summary 
summary(fit2)

#ASE
ase = mean(fit2$residuals^2)
ase

#AIC
AIC(fit2)

#BIC
BIC(fit2)

#Adj R-Squared = 0.8725
#ASE = 8.849
#AIC = 668.3597
#BIC = 691.3
#Adult.Mort, Income.comp, Log.HIV Only significant terms
#Same predictors were signficant, nothing changed from that aspect



### Model 3. Without Income.Comp Predictor
fit3 <- lm(Life.expectancy~Adult.Mortality + BMI + Schooling 
           + Log.Alcohol + Log.HIV.AIDS, data = df2014)

#ASE
ase = mean(fit3$residuals^2)
ase

#AIC
AIC(fit3)

#BIC
BIC(fit3)

#Summary
summary(fit3)

#ASE = 11.358
#AIC = 704.081
#BIC = 724.208
#Adj R^2 = 0.9383
#Adult.Mortal, Schooling, Log.Alc, Log.HIV are significant. BMI is not.
#Might want to consider not including BMI in overall model
#Model3 is weaker than the Model1 and 2; however, more terms are significant



### Model 4 without Log.HIV
fit4 <- lm(Life.expectancy~Adult.Mortality + BMI + Schooling 
           + Log.Alcohol + Income.composition.of.resources, data = df2014)

#ASE
ase = mean(fit4$residuals^2)
ase

#AIC
AIC(fit4)

#BIC
BIC(fit4)

#Summary
summary(fit4)

#ASE = 10.70131
#AIC = 696.280
#BIC = 716.406
#Adj R^2 = 0.8485
#Adult.Mortal, Income.composition are ONLY significant terms
#Model4 has least amount of significant terms, but is stronger than Model3
#but than the Model1 and Model2.



### Model 5 without Schooling, include all other predictors
fit5 <- lm(Life.expectancy~Adult.Mortality + BMI + Income.composition.of.resources
           + Log.Alcohol + Log.HIV.AIDS, data = df2014)

#ASE
ase = mean(fit5$residuals^2)
ase

#AIC
AIC(fit5)

#BIC
BIC(fit5)

#Summary
summary(fit5)

#ASE = 9.321
#AIC = 678.189
#BIC = 698.316
#Adj R^2 = 0.8681
#Adult Mort, Income.comp, and Log HIV.AIDS is most significant in this model

### Model 6 without BMI and Schooling
##Schooling is highly correlated with many other predictors
##BMI seems to not have much effect on response or predictors so we will leave out
fit6 <- lm(Life.expectancy~Adult.Mortality + Income.composition.of.resources
           + Log.HIV.AIDS + Log.Alcohol, data = df2014new)

#ASE
ase = mean(fit6$residuals^2)
ase

#AIC
AIC(fit6)

#BIC
BIC(fit6)

#Summary
summary(fit6)

#ASE = 9.348
#AIC = 676.566
#BIC = 693.817
#Adj R^2 = 0.8688
#All terms are significant except Log.Alc at 0.068. We are confident in moving
#forward with Model 6, so we want to re-run our residual diagnostics/assumptions

#Check QQPLOT for normality Assumption. Seems good
ols_plot_resid_qq(fit6)

#Check Residual Histogram. Perfect
ols_plot_resid_hist(fit6)

#Check Residual vs Fitted Plots for constant variance and normality. Perfect
ols_plot_resid_fit(fit6)

#Studentized Residuals. All seem good except observation 4.
ols_plot_resid_stud(fit6)

#Studentized Residual vs Leverage plot. Observation 108, but 39 is most egregious
ols_plot_resid_lev(fit6)

#Cook's D. Observation 4 and 108 seem most suspicious, but 39 is most impactful
#We will re-run our Model 6 without the 39 influential point. Cook'D of 0.27
ols_plot_cooksd_chart(fit6)



### Model 7 without influential point 39
fit7 <- lm(Life.expectancy~Adult.Mortality + Income.composition.of.resources
           + Log.HIV.AIDS + Log.Alcohol, data = df2014_influential)

#ASE
ase = mean(fit7$residuals^2)
ase

#AIC
AIC(fit7)

#BIC
BIC(fit7)

#Summary
summary(fit7)
car::vif(fit7)

#ASE = 8.877
#AIC = 664.777
#BIC = 681.982
#Adj R^2 = 0.8742
#VIF numbers look good. Observation 4 and 107 display Cooks D of about 0.13 which
#is not too egregious for our analysis

#Check QQPLOT for normality Assumption. Seems good
ols_plot_resid_qq(fit7)

#Check Residual Histogram. Perfect
ols_plot_resid_hist(fit7)

#Check Residual vs Fitted Plots for constant variance and normality. Perfect
ols_plot_resid_fit(fit7)

#Studentized Residuals. All seem good.
ols_plot_resid_stud(fit7)

#Studentized Residual vs Leverage plot. Observation 107, hard to tell
ols_plot_resid_lev(fit7)

#Cook's D. Observation 4 and 107 seem most suspicious, but after removal of
#point 39, Cook's D values were cut by nearly 50%. No other further analysis
#needed for this model
ols_plot_cooksd_chart(fit7)


#### MODEL REVISION
#Now let's go back and revise our old data set and only remove NA's from the
#predictors that we want to include in our model
df <- df %>% filter(!is.na(Alcohol))
df <- df %>% filter(!is.na(Income.composition.of.resources))
df <- df %>% filter(!is.na(Schooling))
df <- df %>% filter(!is.na(Adult.Mortality))

#subset to show only 2014
df2014new <- df[df['Year'] == 2014,]

#Check NA's and make sure you included the correct ones
gg_miss_var(df2014new)

#Filter out Country, Year, and Status from DF
df2014new <- subset(df2014new, select = -c(1,2,3))

#Create Log Variables of Alcohol and HIV
df2014new$Log.Alcohol <- log(df2014new$Alcohol)
df2014new$Log.HIV.AIDS <- log(df2014new$HIV.AIDS)

#Filter our normal Alcohol and HIV values
df2014new <- subset(df2014new, select = -c(4,13))


###IMPORTANT NOTE: our new Dataframe has 171 observations as opposed to the
#initial 130 observations we had for our models that we ran above.
#The reason we have 41 more observations is because I only removed specific
#NA values from the 4 predictors we ended up using after doing model checks
#I believe the df2014new is a better represenation of our models vs the ones above
#Will run the SAME analysis and fit for df2014new

#Rerun correlation matrix and check out the new relationships
SP_matrix4 = cor(df2014new[c(1,2,16,18,19)])
corrplot(SP_matrix4, type="upper", method = "number",
         sig.level = 0.05, tl.cex = 0.75, number.cex=0.75)

#Refit our chosen model with new df2014new data frame
fit8 <- lm(Life.expectancy~Adult.Mortality + Income.composition.of.resources
           + Log.HIV.AIDS + Log.Alcohol, data = df2014new)

#ASE
ase = mean(fit8$residuals^2)
ase

#AIC
AIC(fit8)

#BIC
BIC(fit8)

###Summary
summary(fit8)
car::vif(fit8)

##ASE = 9.438
#AIC = 886.205
#BIC = 905.090
#Adj R^2 = 0.8652
#VIF numbers look good.
#ALL terms are significant
#When we included some of the NA countries that were excluded from our initial model
#selection, our AIC and BIC increased by 200 each roughly. ASE rose by 0.2
#I believe we need to go with fit8 because it incorporates most of the 2014 data

###ASSUMPTION checks of Fit8 (including previous influential point)

#Check QQPLOT for normality Assumption. Seems good
ols_plot_resid_qq(fit8)

#Check Residual Histogram. Perfect
ols_plot_resid_hist(fit8)

#Check Residual vs Fitted Plots for constant variance and normality. Perfect
ols_plot_resid_fit(fit8)

#Studentized Residuals. All seem good.
ols_plot_resid_stud(fit8)

#Studentized Residual vs Leverage plot. Observation 49
ols_plot_resid_lev(fit8)

#Cook's D. Observation 49 is a still a very egregious point. This is probably the
#same 2014 point be filtered out in previous analysis
ols_plot_cooksd_chart(fit8)

##Re-run model without this point
df2014new_influential <- df2014new[-49,]
fit9 <- lm(Life.expectancy~Adult.Mortality + Income.composition.of.resources
           + Log.HIV.AIDS + Log.Alcohol, data = df2014new_influential)

#ASE
ase = mean(fit9$residuals^2)
ase

#AIC
AIC(fit9)

#BIC
BIC(fit9)

###Summary
summary(fit9)
car::vif(fit9)

##ASE = 9.075
#AIC = 874.412
#BIC = 893.262
#Adj R^2 = 0.8652
#VIF numbers look good.
#ALL terms are significant
#When we included some of the NA countries that were excluded from our initial model
#selection, our AIC and BIC increased by 200 each roughly. ASE rose by 0.2
#I believe we need to go with fit8 because it incorporates most of the 2014 data

###ASSUMPTION checks of Fit9 (Final Model without influential point)

#Cook's D. Observation 4 and 137 are < 0.125 for Cook's D. Angola and Sierra Leone
#Will remove those 2 points just for visual purposes, but most likely will keep them
#in model due to not egregious enough
ols_plot_cooksd_chart(fit9)



### Model 10 removing observations 4 and 137 now
df2014new_influential2 <- df2014new_influential[-c(4,137),]
fit10 <- lm(Life.expectancy~Adult.Mortality + Income.composition.of.resources
           + Log.HIV.AIDS + Log.Alcohol, data = df2014new_influential2)

#ASE
ase = mean(fit10$residuals^2)
ase

#AIC
AIC(fit10)

#BIC
BIC(fit10)

#Summary
summary(fit10)
car::vif(fit10)

##ASE = 8.232
#AIC = 847.864
#BIC = 866.644
#Adj R^2 = 0.8682
#VIF numbers look good.
#ALL terms are significant
#Let's just view the Cook's D plot now and see. All observations of Cook'D is
#below 0.05 .... May want to consider either Fit9 or Fit10 as our final model
ols_plot_cooksd_chart(fit10)

##### MODEL9 or MODEL10?