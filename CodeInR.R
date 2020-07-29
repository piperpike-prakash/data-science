######################## Preparation ##########################

#' *Clearing the global environment and setting the working directory*
rm(list = ls())
setwd("F:/RStudio/Project1")

#' *Loading all the required libraries*
x = c("ggplot2", "corrgram", "DMwR", "caret", "e1071", "tseries",
      "psych", "rpart", "gbm", "sampling", "DataCombine", "readxl")
lapply(x, require, character.only = TRUE)
rm(x)


######################## Data Acquisition and Exploration #################

#' *Loading the provided data file*
library(readxl)
data_record = read_excel("Absenteeism_at_work_Project.xls")

#' *Checking the metadata*
dim(data_record)
str(data_record)
colnames(data_record)
class(data_record)

#' *Converting the data into a proper dataframe format*
data_record = as.data.frame(data_record)

#' *Modifying Column names for ease of typing*
names(data_record)[2] <- "ReasonForAbsence"
names(data_record)[3] <- "MonthOfAbsence"
names(data_record)[4] <- "DayOfWeek"
names(data_record)[6] <- "TransportationExpense"
names(data_record)[7] <- "DistanceFromResidence"
names(data_record)[8] <- "ServiceTime"
names(data_record)[10] <- "WorkloadAverage"
names(data_record)[11] <- "HitTarget"
names(data_record)[12] <- "DisciplinaryFailure"
names(data_record)[15] <- "SocialDrinker"
names(data_record)[16] <- "SocialSmoker"
names(data_record)[20] <- "BodyMassIndex"
names(data_record)[21] <- "AbsenteeismTimeInHours"

#' *Classifying the variables as 'continuous' or 'categorical'*
contin_var = c("TransportationExpense", "DistanceFromResidence", "ServiceTime", "Age", "WorkloadAverage", 
               "HitTarget", "Weight", "Height", "BodyMassIndex", "AbsenteeismTimeInHours")
categor_var = c("ID", "ReasonForAbsence", "MonthOfAbsence", "DayOfWeek", "Seasons", "DisciplinaryFailure", 
                "Education", "Son", "SocialDrinker", "SocialSmoker", "Pet")


####################### Missing Value Analysis #########################

#' *Checking for missing values in each variable*
sapply(data_record, function(x) sum(is.na(x)))

#' *Creating a dataframe with the missing values in each variable*
missingvalues = data.frame(apply(data_record, 2, function(x) sum(is.na(x))))
missingvalues$Columns = row.names(missingvalues)
rownames(missingvalues) <- c()

#' *Working to get the missing percentage values for each variable*
names(missingvalues)[1] = "Missing_Percentage"
missingvalues$Missing_Percentage = (missingvalues$Missing_Percentage/nrow(data_record)) * 100

#' *Reordering the columns accordingly*
missingvalues = missingvalues[,c(2,1)]

#' *Saving the missing precentage data on the disk as a Comma-Separated Values file*
write.csv(missingvalues, "Missing Percentage.csv")

#' *Creating a copy dataset to check for the proper imputation method*
data_copy = as.data.frame(data_record)

#' *Creating a known missing value to configure the proper imputation method*
#' *Choosing the 8th observation in the column 'TransportationExpense' to be the missing value*
data_copy[9,6]

#' *Original value of the observation = 155.0*
#' *Changing the value to NaN*
data_copy[9,6] = NaN

#' *Imputing with mean*
data_copy[9,6] = mean(data_record$TransportationExpense, na.rm = T)

#' *Mean imputation gives 221.04*

#' *Changing the value back to NaN*
data_copy[9,6] = NaN

#' *Imputing with median*
data_copy[9,6] = median(data_record$TransportationExpense, na.rm = T)

#' *Median imputation gives 225*

#' *Changing the value to NaN again*
data_copy[9,6] = NaN

#' *Imputing with k-nearest neighbour*
library(DMwR)
data_copy = knnImputation(data_copy, k = 3)

#' *KNN Imputation gives 155*
#' *Therefore, among Mean, Median and KNN, KNN is the superior method to impute the missing data*

#' *Using the k-Nearest Neighbour Method to impute the missing values*
data_record = knnImputation(data_record, k = 3)


####################### Data Manipulation #########################

#' *Removing the observations where Absenteeism Time In Hours is zero*
#' *As there is no point in considering cases where the employee wasn't absent*
data_record = subset(data_record,  data_record$AbsenteeismTimeInHours != 0)


####################### Outlier Analysis ##############################

#' *Creating a boxplot for outlier check in continuous variables*
for(i in 1:length(contin_var))
{
  assign(paste0("gn",i),ggplot(aes_string(y = (contin_var[i]), x = "AbsenteeismTimeInHours"), data = subset(data_record)) +
                  stat_boxplot(geom = "errorbar", width = 0.5) + 
                  geom_boxplot(outlier.colour = "red", fill = "grey", outlier.shape = 18,
                               outlier.size = 1, notch = FALSE) +
                  theme(legend.position = "bottom") +
                  labs(y = contin_var[i], x = "AbsenteeismTimeInHours") + 
                  ggtitle(paste("Boxplot - Absenteeism for",contin_var[i])))
}

#' *Creating the plots together (except for the target variable)*
gridExtra::grid.arrange(gn1,gn2,gn3, ncol = 3)
gridExtra::grid.arrange(gn4,gn5,gn6, ncol = 3)
gridExtra::grid.arrange(gn7,gn8,gn9, ncol = 3)

#' *Removing outliers from all variables by replacing them with NA*
for(i in contin_var)
{
  value = data_record[,i][data_record[,i] %in% boxplot.stats(data_record[,i])$out]
  data_record[,i][data_record[,i] %in% value] = NA
}

#' *Checking for missing values*
sapply(data_record, function(x) sum(is.na(x)))

#' *Imputing the missing data with kNN*
data_record = knnImputation(data_record, k=3)

#' *Rechecking for outliers*
for(i in 1:length(contin_var))
{
  assign(paste0("gn",i),ggplot(aes_string(y = (contin_var[i]), x = "AbsenteeismTimeInHours"), data = subset(data_record)) +
           stat_boxplot(geom = "errorbar", width = 0.5) + 
           geom_boxplot(outlier.colour = "red", fill = "grey", outlier.shape = 18,
                        outlier.size = 1, notch = FALSE) +
           theme(legend.position = "bottom") +
           labs(y = contin_var[i], x = "AbsenteeismTimeInHours") + 
           ggtitle(paste("Boxplot - Absenteeism for",contin_var[i])))
}
gridExtra::grid.arrange(gn1,gn2,gn3, ncol = 3)          
gridExtra::grid.arrange(gn4,gn5,gn6, ncol = 3)          
gridExtra::grid.arrange(gn7,gn8,gn9, ncol = 3)     
#' *No outliers were discovered, so we can proceed ahead*


############################################### Data Analysis ###############################################

#' *Loading the requisite library*
library(psych)

#' *Rounding off the dataset to maintain regularity among the imputed values of categorical variables*
data_record = round(data_record,0)

#' *Creating a duplicate dataset at this point (to be used for Time Series operations)*
absentData = as.data.frame(data_record)

#' *Creating bar graphs for the categorical variables*
barplot(table(data_record$ID),xlab = "ID", ylab = "Absenteeism Time In Hours", col = "cornflowerblue")
barplot(table(data_record$ReasonForAbsence),xlab = "ReasonForAbsence", ylab = "Absenteeism Time In Hours", col = "cornflowerblue")
barplot(table(data_record$MonthOfAbsence),xlab = "MonthOfAbsence", ylab = "Absenteeism Time In Hours", col = "cornflowerblue")
barplot(table(data_record$DayOfWeek),xlab = "DayOfWeek", ylab = "Absenteeism Time In Hours", col = "cornflowerblue")
barplot(table(data_record$Seasons),xlab = "Seasons", ylab = "Absenteeism Time In Hours", col = "cornflowerblue")
barplot(table(data_record$DisciplinaryFailure),xlab = "DisciplinaryFailure", ylab = "Absenteeism Time In Hours", col = "cornflowerblue")
barplot(table(data_record$Education),xlab = "Education", ylab = "Absenteeism Time In Hours", col = "cornflowerblue")
barplot(table(data_record$Son),xlab = "Son", ylab = "Absenteeism Time In Hours", col = "cornflowerblue")
barplot(table(data_record$SocialDrinker),xlab = "SocialDrinker", ylab = "Absenteeism Time In Hours", col = "cornflowerblue")
barplot(table(data_record$SocialSmoker),xlab = "SocialSmoker", ylab = "Absenteeism Time In Hours", col = "cornflowerblue")
barplot(table(data_record$Pet),xlab = "Pet", ylab = "Absenteeism Time In Hours", col = "cornflowerblue")

#'*------------------------------------------- Primitive Conclusions -----------------------------------------*#


#' *Employee with ID = 3 has the most entries for absenteeism.*
#' *The top 5 reasons for absenteeism (in number of entries) are 23 (Medical Consultation), 28 (Dental Consultation),*
#' *27 (Physiotherapy), 13 (Musculoskeletal Diseases), and 19 (Injury, poisoning or other external causes).*
#' *Most entries for absenteeism is in month number 3 (March).*
#' *Most entries for absenteeism are for the second day of the week (Monday).*
#' *Disciplinary Failures are rare.*
#' *A sizeable number of employees don't have kids and/or pets.*
#' *There is a moderate number of social drinkers and a miniscule number of social smokers.*

#' *Creating plots to see the relation between the predictor variables and target variable.*
scatter.hist(data_record$ID,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "ID",ylab = "Absentee Hours",title = "Absentee Hours by ID", col = 'blue')
scatter.hist(data_record$ReasonForAbsence,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Reason For Absence",ylab = "Absentee Hours",title = "Absentee Hours by Reason", col = 'blue')
scatter.hist(data_record$MonthOfAbsence,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Month",ylab = "Absentee Hours",title = "Absentee Hours by Month", col = 'blue')
scatter.hist(data_record$DayOfWeek,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Day Of Week",ylab = "Absentee Hours",title = "Absentee Hours by Day Of Week", col = 'blue')
scatter.hist(data_record$Seasons,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Seasons",ylab = "Absentee Hours",title = "Absentee Hours by Season", col = 'blue')
scatter.hist(data_record$DisciplinaryFailure,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Disciplinary Failure",ylab = "Absentee Hours",title = "Absentee Hours by Disciplinary Failure",
             col = 'blue')
scatter.hist(data_record$Education,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Education",ylab = "Absentee Hours",title = "Absentee Hours by Education", col = 'blue')
scatter.hist(data_record$Son,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Sons",ylab = "Absentee Hours",title = "Absentee Hours by Number of Sons", col = 'blue')
scatter.hist(data_record$Pet,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Pets",ylab = "Absentee Hours",title = "Absentee Hours by Number of Pets", col = 'blue')
scatter.hist(data_record$SocialDrinker,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Social Drinker",ylab = "Absentee Hours",title = "Absentee Hours by Measure of Social Drinking",
             col = 'blue')
scatter.hist(data_record$SocialSmoker,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Social Smoker",ylab = "Absentee Hours",title = "Absentee Hours by Measure of Social Smoking",
             col = 'blue')
scatter.hist(data_record$TransportationExpense,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Transportation Expense",ylab = "Absentee Hours",title = "Absentee Hours by Transportation Expense",
             col = 'blue')
scatter.hist(data_record$DistanceFromResidence,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Distance From Residence",ylab = "Absentee Hours",title = "Absentee Hours by Distance From Residence (kms)",
             col = 'blue')
scatter.hist(data_record$ServiceTime,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Service Time",ylab = "Absentee Hours",title = "Absentee Hours by Amount of Service Time (yrs)", col = 'blue')
scatter.hist(data_record$Age,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Age",ylab = "Absentee Hours",title = "Absentee Hours by Age (yrs", col = 'blue')
scatter.hist(data_record$WorkloadAverage,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Work Load Average Per Day",ylab = "Absentee Hours",title = "Absentee Hours by Work Load Average Per Day",
             col = 'blue')
scatter.hist(data_record$HitTarget,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Hit Target",ylab = "Absentee Hours",title = "Absentee Hours by Hit Target (%)", col = 'blue')
scatter.hist(data_record$Weight,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Weight",ylab = "Absentee Hours",title = "Absentee Hours by Weight (kg)", col = 'blue')
scatter.hist(data_record$Height,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Height",ylab = "Absentee Hours",title = "Absentee Hours by Height (cm)", col = 'blue')
scatter.hist(data_record$BodyMassIndex,data_record$AbsenteeismTimeInHours, ellipse = FALSE, freq = TRUE,
             xlab = "Body Mass Index",ylab = "Absentee Hours",title = "Absentee Hours by Body Mass Index (kg/cm^2)",
             col = 'blue')

#'*------------------------------------------- Informed Conclusions --------------------------------------------*#

#'* 1. The company lost most hours to absenteeism due to employees having Physiotherapy and Medical Consultations.*

#'* 2. The company lost most hours to absenteeism in the months of February, July and December.*

#'* 3. The company lost significant hours to absenteeism from employees who have residences farthest from work.*

#'* 4. The company lost significant hours to absenteeism from employees who are nearing 10 or 20 years of service.*

#'* 5. The company lost most hours to absenteeism from employees in the 36-42 age group,*
#'*    and lost significant hours to absenteeism from employees in the 28-29 age group.*

#'* 6. The company lost most hours to absenteeism from employees with average daily workload of 270000 units,*
#'*    and lost significant hours to absenteeism from employees with average daily workload of 240000 units.*

#'* 7. The company lost most hours to absenteeism from employees who hit a target of 93%,*
#'*    and lost significant hours to absenteeism from employees who hit the target range of 96-99%.*


############################################## Feature Selection ###########################################

#' *Creating a Correlation plot to investigate dependencies*
corrgram(data_record[,contin_var], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#' *There is highly positive correlation present between the variables BodyMassIndex and Weight*
#' *Since Weight is the base variable, we can get rid of BodyMassIndex*
data_record = subset(data_record, select = -c(BodyMassIndex))

#' *Performing the ANOVA test for the categorical variables*
summary(aov(formula = AbsenteeismTimeInHours~ID + DayOfWeek + Education + SocialSmoker +
               SocialDrinker + ReasonForAbsence + Seasons + MonthOfAbsence + DisciplinaryFailure,data = data_record))

#' *Performing dimension reduction (categorical variables) on the duplicate data for ease*
#' *(After Investigating The Summary of The ANOVA Test)*
data_record = subset(data_record, select = -c(DayOfWeek,Education,SocialSmoker,SocialDrinker,
                                                        Seasons,MonthOfAbsence))


######################################## Feature Scaling ############################################

#' *Clearing the work environment*
rmExcept("data_record", envir = globalenv())

#' *Creating a duplicate dataframe for further operations*
duplicate_record = as.data.frame(data_record)

#' *Updating the lists of continuous and categorical variables*
contin_var = c("TransportationExpense", "DistanceFromResidence", "ServiceTime", "Age", "WorkloadAverage", 
               "HitTarget", "Weight", "Height", "AbsenteeismTimeInHours")
categor_var = c("ID", "ReasonForAbsence", "DisciplinaryFailure", "Son", "Pet")

#' *Normalising the numerical features (continuous variables) of the record to be used*
for (i in contin_var) {
  print(i)
  duplicate_record[,i] = ((duplicate_record[,i] - min(duplicate_record[,i])) / 
                            (max(duplicate_record[,i]) - min(duplicate_record[,i])))
}


############################################### Forecasting Data ###############################################

#' *To work on forecasting, we will utilise the duplicate dataset created after performing Outlier Analysis*
View(absentData)

#' *Converting the field MontOfAbsence to factor*
absentData$MonthOfAbsence = as.factor(absentData$MonthOfAbsence)

#' *Creating a time series aggregating AbsenteeismTimeInHours by MonthOfAbsence*
monthlyAbsence = aggregate(absentData$AbsenteeismTimeInHours, by=list(Month=absentData$MonthOfAbsence), FUN=sum)


#' *For making predictions of absenteeism for the next year (2011),*
#' *data of more than one year is required to get more refined predictions.*

#' *Therefore, it will be in our interest to assume that the existing data*
#' *is of the last two years (since the timeframe of the existing data is unclear,*
#' *and the bigger timeframe we have, the better the predictions will be)*


#' *Calculating the absenteeism hours for a single year*
monthlyAbsence$x = monthlyAbsence$x/2

#' *Renaming the column for absenteeism hours*
colnames(monthlyAbsence)[2] <- "AbsenceHours"
#' *Loading the library for time series*
library(tseries)

#' *MOdelling a time series for the absence hours*
timeSeries = ts(monthlyAbsence$AbsenceHours)

#' *Plotting the time series*
plot(timeSeries, ylab = 'Absence Hours')

#' *Performing the Augmented Dickey-Fuller Test to chec stationarity of the time series*
adf.test(timeSeries, alternative = "stationary", k=0)

#' *Observations made from the test are as follows :*
#' *Dickey-Fuller = -3.8769,*
#' *Lag order = 0,*
#' *p-value = 0.03022*

#' *Since the p-value here is lower than 0.05, we can reject the null hypothesis,*
#' *and classify this time series as stationary*

#' *Plotting the Autocorrelation Function and the Partial Autocorrelation Function to check for p and q*
par(mfrow=c(1,2))
acf(timeSeries)
pacf(timeSeries)

#' *Initial models chosen for fitting :*
#' *(0,0,0)*
#' *(1,0,0)*
#' *(0,0,1)*
#' *(2,0,0)*
#' *(0,0,2)*

#' *Loading the requisite library*
library(forecast)

#' *Fitting the model and getting the Residual Sum of Squares*

#' *(0,0,0)*
seriesModel = arima(timeSeries,c(0,0,0))
fitValue = fitted(seriesModel)
ResSumSqa = timeSeries - fitValue
sum(ResSumSqa**2)

#' *(1,0,0)*
seriesModel = arima(timeSeries,c(1,0,0))
fitValue = fitted(seriesModel)
ResSumSqa = timeSeries - fitValue
sum(ResSumSqa**2)

#' *(0,0,1)*
seriesModel = arima(timeSeries,c(0,0,1))
fitValue = fitted(seriesModel)
ResSumSqa = timeSeries - fitValue
sum(ResSumSqa**2)

#' *(2,0,0)*
seriesModel = arima(timeSeries,c(2,0,0))
fitValue = fitted(seriesModel)
ResSumSqa = timeSeries - fitValue
sum(ResSumSqa**2)

#' *(0,0,2)*
seriesModel = arima(timeSeries,c(0,0,2))
fitValue = fitted(seriesModel)
ResSumSqa = timeSeries - fitValue
sum(ResSumSqa**2)

#' *Fitting models with higher p and q values*

#' *(1,0,1)*
seriesModel = arima(timeSeries,c(1,0,1))
fitValue = fitted(seriesModel)
ResSumSqa = timeSeries - fitValue
sum(ResSumSqa**2)

#' *(2,0,1)*
seriesModel = arima(timeSeries,c(2,0,1))
fitValue = fitted(seriesModel)
ResSumSqa = timeSeries - fitValue
sum(ResSumSqa**2)

#' *(2,0,2)*
seriesModel = arima(timeSeries,c(2,0,2))
fitValue = fitted(seriesModel)
ResSumSqa = timeSeries - fitValue
sum(ResSumSqa**2)

#' *(3,0,0)*
seriesModel = arima(timeSeries,c(3,0,0))
fitValue = fitted(seriesModel)
ResSumSqa = timeSeries - fitValue
sum(ResSumSqa**2)

#' *(3,0,2)*
seriesModel = arima(timeSeries,c(3,0,2))
fitValue = fitted(seriesModel)
ResSumSqa = timeSeries - fitValue
sum(ResSumSqa**2)

#' *(3,0,3)*
seriesModel = arima(timeSeries,c(3,0,3))
fitValue = fitted(seriesModel)
ResSumSqa = timeSeries - fitValue
sum(ResSumSqa**2)

#' *(4,0,0)*
seriesModel = arima(timeSeries,c(4,0,0))
fitValue = fitted(seriesModel)
ResSumSqa = timeSeries - fitValue
sum(ResSumSqa**2)

#' *(4,0,2)*
seriesModel = arima(timeSeries,c(4,0,2))
fitValue = fitted(seriesModel)
ResSumSqa = timeSeries - fitValue
sum(ResSumSqa**2)

#' *(4,0,3)*
seriesModel = arima(timeSeries,c(4,0,3))
fitValue = fitted(seriesModel)
ResSumSqa = timeSeries - fitValue
sum(ResSumSqa**2)

#' *(4,0,4)*
seriesModel = arima(timeSeries,c(4,0,4))
fitValue = fitted(seriesModel)
ResSumSqa = timeSeries - fitValue
sum(ResSumSqa**2)


#' *____________________________________________ Observations ______________________________________________*
 
#' *RSS value for model (0,0,0) = 17662.56*
#' *RSS value for model (1,0,0) = 17404.66*
#' *RSS value for model (0,0,1) = 15874.85*
#' *RSS value for model (2,0,0) = 12290.29*
#' *RSS value for model (0,0,2) = 11263.41*
#' *RSS value for model (1,0,1) = 12598.55*
#' *RSS value for model (2,0,1) = 12277.26*
#' *RSS value for model (2,0,2) = 11005.04*
#' *RSS value for model (3,0,0) = 12251.34*
#' *RSS value for model (3,0,2) = 6058.74*
#' *RSS value for model (3,0,3) = 5919.37*
#' *RSS value for model (4,0,0) = 9448.39*
#' *RSS value for model (4,0,2) = 5654.00*
#' *RSS value for model (4,0,3) = 5599.87*
#' *RSS value for model (4,0,4) = 3874.39*

#' *ARIMA(4,0,4) gives the lowest RSS value of 3874.39, hence this model is chosen for forecasting*
chosenModel = arima(timeSeries,c(4,0,4))
chosenFit = fitted(chosenModel)

#' *Predicting the absenteeism hours for 2011*
absenceHours_2011 = predict(chosenModel, n.ahead = 12)

#' *Converting the predicted data to a data frame and getting rid of unusable attributes*
absenceHours_2011 = as.data.frame(absenceHours_2011)
absenceHours_2011 = subset(absenceHours_2011, select = -c(se))

#' *Changing the column names and row names for the corresponding months in future*
colnames(absenceHours_2011)[1] <- "AbsenceHours"
row.names(absenceHours_2011) <- 13:24

#' *Rounding off the predicted data for ease*
absenceHours_2011 = round(absenceHours_2011,1)
absenceHours_2011

#' *Converting the data into a time series for ease of visualisation*
timeSeries_2011 = ts(absenceHours_2011, start = 13)

#' *Plotting the original absenteeism hours and the forecasted absenteeism hours*
plot(timeSeries,xlim=c(1,24))
lines(timeSeries_2011)


#' *The predictions appear to be lacking the irregularity of the existing data,*
#' *but that's because of the assumption that the existing data is of the past two years*
#' *(2009 and 2010). Had there been a longer timeframe of the data, the irregularity*
#' *would have been much lower. Not to forget that the data was simply halved instead*
#' *of having separate observations for the two years in question.*