proc import out = work.who2014
			datafile = "C:\Users\Aurian\Documents\SMU_Git\WHO_Project1_STATS6372\WHOProject1\Data\WHO_2014_Transformed_Data.csv"
			dbms = csv replace;
		getnames = yes;
		datarow = 2;
	run;
/*I filtered out the original 182 data observations down to 131, we may need to try new techniques like taking the average of a column and inserting that mean value into all the NULL spaces */
proc print data = who2014; run;

/* Prediction using Forward Selection */
/*CV PRESS 1299.7884*/
/*Income Comp, Adult Mortality, HIV_AIDS, Total Expenditure*/
proc glmselect data = who2014;
class Status;
model Life_expectancy = Status Adult_Mortality infant_deaths Alcohol percentage_expenditure Hepatitis_B Measles BMI under_five_deaths Polio Total_expenditure Diphtheria HIV_AIDS GDP Population thinness__1_19_years thinness_5_9_years Income_composition_of_resources Schooling / selection=Forward(stop=cv) cvmethod=random(20) stats=adjrsq;
run;

/* Prediction using Backward Selection */
/*CV PRESS 1421*/
/*Over 10 predictors*/
proc glmselect data = who2014;
class Status;
model Life_expectancy = Status Adult_Mortality infant_deaths Alcohol percentage_expenditure Hepatitis_B Measles BMI under_five_deaths Polio Total_expenditure Diphtheria HIV_AIDS GDP Population thinness__1_19_years thinness_5_9_years Income_composition_of_resources Schooling / selection=Backward(stop=cv) cvmethod=random(20) stats=adjrsq;
run;

/* Prediction using Stepwise Selection */
/* Predictors are Income Comp, Adult Mortality, HIV_AIDS, Total Expenditure*/
/*CV PRESS 1393.1828*/
proc glmselect data = who2014;
class Status;
model Life_expectancy = Status Adult_Mortality infant_deaths Alcohol percentage_expenditure Hepatitis_B Measles BMI under_five_deaths Polio Total_expenditure Diphtheria HIV_AIDS GDP Population thinness__1_19_years thinness_5_9_years Income_composition_of_resources Schooling / selection=Stepwise(stop=cv) cvmethod=random(20) stats=adjrsq;
run;

/* Prediction using LASSO */
/* Income_Comp, Adult_Mort, HIV_AIDS, Total Expenditure */
/* CV PRESS 1310.4323 */
proc glmselect data = who2014;
class Status;
model Life_expectancy = Status Adult_Mortality infant_deaths Alcohol percentage_expenditure Hepatitis_B Measles BMI under_five_deaths Polio Total_expenditure Diphtheria HIV_AIDS GDP Population thinness__1_19_years thinness_5_9_years Income_composition_of_resources Schooling / selection=LASSO(choose=cv stop=cv) CVdetails;
run;

/*Log Transform Alcohol and AIDS to see if that helps with out Stepwise selection */
data who2014log;
set who2014;
LogAlcohol = log(Alcohol);
LogHIVAIDS = log(HIV_AIDS);
run;

/*Forward selection with log variables */
/*CV PRESS improved to 1281*/
/*Income Comp, Adult_Mort, LogHIVAIDS, LogAlc, Total_Expend*/
proc glmselect data = who2014log;
class Status;
model Life_expectancy = Status Adult_Mortality infant_deaths Alcohol percentage_expenditure Hepatitis_B Measles BMI under_five_deaths Polio Total_expenditure Diphtheria HIV_AIDS GDP Population thinness__1_19_years thinness_5_9_years Income_composition_of_resources Schooling LogAlcohol LogHIVAIDS / selection=Forward(stop=cv) cvmethod=random(20) stats=adjrsq;
run;

/*Backward selection with log variables */
/*CV PRESS improved to 1288.4796*/
/*Over 14 predictors */
proc glmselect data = who2014log;
class Status;
model Life_expectancy = Status Adult_Mortality infant_deaths Alcohol percentage_expenditure Hepatitis_B Measles BMI under_five_deaths Polio Total_expenditure Diphtheria HIV_AIDS GDP Population thinness__1_19_years thinness_5_9_years Income_composition_of_resources Schooling LogAlcohol LogHIVAIDS / selection=Backward(stop=cv) cvmethod=random(20) stats=adjrsq;
run;

/*Stepwise with log transformation */
/*Income_comp, Adult_Mort, LogHIVAIDS, Total_Expend*/
/*CV Press improved to 1323.9686*/
proc glmselect data = who2014log;
class Status;
model Life_expectancy = Status Adult_Mortality infant_deaths Alcohol percentage_expenditure Hepatitis_B Measles BMI under_five_deaths Polio Total_expenditure Diphtheria HIV_AIDS GDP Population thinness__1_19_years thinness_5_9_years Income_composition_of_resources Schooling LogAlcohol LogHIVAIDS / selection=Stepwise(stop=cv) cvmethod=random(20) stats=adjrsq;
run;

/* Prediction using LASSO with log */
/* Income_Comp, Adult_Mort, HIV_AIDS, Total Expenditure */
/* CV PRESS increased to 1420 */
proc glmselect data = who2014log;
class Status;
model Life_expectancy = Status Adult_Mortality infant_deaths Alcohol percentage_expenditure Hepatitis_B Measles BMI under_five_deaths Polio Total_expenditure Diphtheria HIV_AIDS GDP Population thinness__1_19_years thinness_5_9_years Income_composition_of_resources Schooling / selection=LASSO(choose=cv stop=cv) CVdetails;
run;
