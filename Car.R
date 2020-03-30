# Installing Packages important for this " Advanced regression Techniques "
install.packages("corrplot")
install.packages("boot")
install.packages("vars")
install.packages("QuantPsyc")
install.packages("lmtest")
install.packages("nortest")
install.packages("car")
install.packages("sandwich")
install.packages("MASS")
install.packages("caTools")
install.packages("dplyr")

# Loading essential libraries
library(corrplot)                    # For visualizing correlation matrices and confidence intervals
library(boot)                        # Provides extensive facilities for bootstrapping and related resampling methods
library(vars)                        # Modelling Estimation, lag selection, diagnostic testing, forecasting etc
library(QuantPsyc)                   # Quantitative Psychology Tools which Contains functions useful for data screening, testing moderation, mediation and estimating power
library(lmtest)                      # Testing Linear Regression Models
library(nortest)                     # Tests for Normality
library(car)                         # Companion to Applied Regression
library(sandwich)                    # Covariance Matrix Estimators
library(MASS)                        # Support Functions and Datasets for Venables
library(caTools)                     # Moving window statistics, GIF, Base64, ROC AUC, etc. Contains several basic utility functions including: moving (rolling, running) window statistic functions, read/write for GIF and ENVI binary files, fast calculation of AUC, LogitBoost classifier etc.
library(dplyr)                       # For data manipulation
library(dummies)

## Setting up the working Directory
setwd("E:/industry_cases/Rproject")
getwd()
cars_data<-read.csv("CarPrice_Assignment.csv", header = TRUE)
Data<-cars_data
str(Data)
View(Data)

##########################################################################################
##########################################################################################

                       #Linear Regression is used here#

##########################################################################################
##########################################################################################

# Checking N/A values in the given dataset

data.frame(colSums(is.na(Data)))                 # No N/A values present 
View(Data)
str(Data)

#####################################################################################################
                          # PROBLEM STATEMENT #
#Model question: Frame a predictive-modelling using R, where you have to predict the Price of the car.
######################################################################################################

boxplot(Data$price, main = "Outliers present in a Price Variable", ylab = "price", col = "orange", border = "black")
quantile(Data$price, c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.95,0.99,1.0))
Data1 <- Data[Data$price < 40000,]
nrow(Data)
nrow(Data1)                             # 3 rows are remove only.

boxplot(Data1$price, main = "Outliers present in a Price Variable", ylab = "price", col = "orange", border = "black")
quantile(Data1$price, c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.95,1.0))
Data2 <- Data1[Data1$price < 31000,]
nrow(Data2)
nrow(Data1) - nrow(Data2)               # 11 rows are remove.

boxplot(Data2$price, main = "Outliers present in a Price Variable", ylab = "price", col = "orange", border = "black")
quantile(Data2$price, c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.95,1.0))
Data3 <- Data2[Data2$price < 20000,]
nrow(Data3)
nrow(Data2) - nrow(Data3)             # 12 rows are remove here.

boxplot(Data3$price, main = "No Outliers present in a Price Variable", ylab = "price", col = "orange", border = "black")
quantile(Data3$price,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.95,1.0))
nrow(Data3)

dim(Data3)

####################################################################################################################
                               # Spliting Data into Train And Test Part #
####################################################################################################################
set.seed(123)
split_Data3<-sample.split(Data3$price , 0.7) 
Train_Data3 <- subset(Data3 , split_Data3 == TRUE)
Test_Data3 <- subset(Data3 , split_Data3 == FALSE)

dim(Train_Data3)
dim(Test_Data3)
str(Train_Data3)

## This will categorise all the factor variables under DROP & NODROP category. Those falling under DROP, we do not have to take those variables in our model.


(l <- sapply(Train_Data3, function(x) is.factor(x)))
m <- Train_Data3[,l]
ifelse(n <- sapply(m, function(x) length(levels(x))) <= 2, "DROP", "NODROP" )


## Fitted Models

fit_1<-lm(formula = price ~ car_ID + symboling + CarName + carbody + drivewheel + wheelbase + carlength + carwidth + carheight + curbweight + enginetype + cylindernumber + enginesize + fuelsystem + boreratio + stroke + compressionratio + horsepower + peakrpm + citympg + highwaympg, data = Train_Data3)
summary(fit_1)

fit_2<-lm(price ~ car_ID + symboling + carbody + drivewheel + wheelbase + carlength + carwidth + carheight + curbweight + enginetype + cylindernumber + enginesize + fuelsystem + boreratio + stroke + compressionratio + horsepower + peakrpm + citympg + highwaympg, data = Train_Data3)
summary(fit_2)

fit_3<-lm(price ~ car_ID + symboling + carbody + drivewheel + wheelbase + carlength + carwidth + carheight + curbweight + enginetype + cylindernumber + enginesize + fuelsystem + boreratio + stroke + compressionratio + horsepower + peakrpm + citympg, data = Train_Data3)
summary(fit_3)

fit_4<-lm(price ~ car_ID + symboling + carbody + drivewheel + wheelbase + carlength + carwidth + carheight + curbweight + enginetype + cylindernumber + enginesize + fuelsystem + boreratio + stroke + compressionratio + horsepower + peakrpm, data = Train_Data3)
summary(fit_4)

fit_5<-lm(price ~ car_ID + symboling + carbody + drivewheel + wheelbase + carlength + carwidth + carheight + curbweight + enginetype + cylindernumber + enginesize + fuelsystem + boreratio + stroke + compressionratio + horsepower, data = Train_Data3)
summary(fit_5)

fit_6<-lm(price ~ car_ID + symboling + carbody + drivewheel + wheelbase + carlength + carwidth + carheight + curbweight + enginetype + cylindernumber + enginesize + fuelsystem + boreratio + stroke + horsepower, data = Train_Data3)
summary(fit_6)

fit_7<-lm(price ~ car_ID + symboling + carbody + drivewheel + wheelbase + carlength + carwidth + carheight + curbweight + enginetype + cylindernumber + enginesize + fuelsystem + boreratio + horsepower, data = Train_Data3)
summary(fit_7)

fit_8<-lm(price ~ car_ID + symboling + carbody + drivewheel + wheelbase + carlength + carwidth + carheight + curbweight + enginetype + cylindernumber + enginesize + fuelsystem + horsepower, data = Train_Data3)
summary(fit_8)

fit_9<-lm(price ~ car_ID + symboling + carbody + drivewheel + wheelbase + carlength + carwidth + carheight + curbweight + enginetype + cylindernumber + enginesize + horsepower, data = Train_Data3)
summary(fit_9)

fit_10<-lm(price ~ car_ID + symboling + carbody + drivewheel + wheelbase + carlength + carwidth + curbweight + enginetype + cylindernumber + enginesize + horsepower, data = Train_Data3)
summary(fit_10)


fit_11<-lm(price ~ car_ID + symboling + carbody + drivewheel + wheelbase + carwidth + curbweight + enginetype + cylindernumber + enginesize + horsepower, data = Train_Data3)
summary(fit_11)

fit_12<-lm(price ~ car_ID + symboling + carbody  + wheelbase + carwidth + curbweight + enginetype + cylindernumber + enginesize + horsepower, data = Train_Data3)
summary(fit_12)

fit_13<-lm(price ~ car_ID + carbody  + wheelbase + carwidth + curbweight + enginetype + cylindernumber + enginesize + horsepower, data = Train_Data3)
summary(fit_13)

fit_14<-lm(price ~ car_ID + carbody  + wheelbase  + curbweight + enginetype + cylindernumber + enginesize + horsepower, data = Train_Data3)
summary(fit_14)

fit_15<-lm(price ~ car_ID + carbody  + wheelbase  + curbweight + I(enginetype=="l") + cylindernumber + enginesize + horsepower, data = Train_Data3)
summary(fit_15)

fit_16<-lm(price ~ car_ID + I(carbody=="hardtop") + I(carbody=="hatchback") + I(carbody=="wagon")  + wheelbase  + curbweight + I(enginetype=="l") + cylindernumber + enginesize + horsepower, data = Train_Data3)
summary(fit_16)

fit_17<-lm(price ~ car_ID + I(carbody=="hatchback") + I(carbody=="wagon")  + wheelbase  + curbweight + I(enginetype=="l") + cylindernumber + enginesize + horsepower, data = Train_Data3)
summary(fit_17)

fit_18<-lm(price ~ car_ID + I(carbody=="hatchback") + I(carbody=="wagon")  + wheelbase  + curbweight + I(enginetype=="l") + cylindernumber +  horsepower, data = Train_Data3)
summary(fit_18)

fit_19<-lm(price ~ car_ID + I(carbody=="wagon")  + wheelbase  + curbweight + I(enginetype=="l") + cylindernumber +  horsepower, data = Train_Data3)
summary(fit_19)

fit_20<-lm(price ~ I(carbody=="wagon")  + wheelbase  + curbweight + I(enginetype=="l") + cylindernumber +  horsepower, data = Train_Data3)
summary(fit_20)

fit_21<-lm(price ~ I(carbody=="wagon")  + wheelbase  + curbweight + I(enginetype=="l") + I(cylindernumber=="four") + I(cylindernumber=="six") +  horsepower, data = Train_Data3)
summary(fit_21)

fit_22<-lm(price ~ I(carbody=="wagon")  + wheelbase  +  I(enginetype=="l") + I(cylindernumber=="four") + I(cylindernumber=="six") +  horsepower, data = Train_Data3)
summary(fit_22)

fit_23<-lm(price ~ I(carbody=="wagon")  + wheelbase  +  I(cylindernumber=="four") + I(cylindernumber=="six") +  horsepower, data = Train_Data3)
summary(fit_23)
vif(fit_23)

fit_24<-lm(price ~ I(carbody=="wagon")  + wheelbase  +  I(cylindernumber=="four")  +  horsepower, data = Train_Data3)
summary(fit_24)
vif(fit_24)



## Get the predicted or fitted values
fitted(fit_24)
par(mfrow=c(2,2))
plot(fit_24)


#Calculating Coefficient of Determination
attach(Train_Data3)
pred<-predict(fit_24)
sse<-sum((Train_Data3$price-pred)^2)
sst<-sum((Train_Data3$price-mean(Train_Data3$price))^2)
1-sse/sst


## Calculated MAPE VALUE
MAPE<-(sum((abs(price-pred))/price)/nrow(Train_Data3))
MAPE

############ Residual Analysis ############################################################################

res <- Train_Data3

res$stu_res <- studres(fit_24) ##Studentized residuals
res$stud.del.resids <- rstudent(fit_24) ##studentized deleted residuals
res$leverage <- hatvalues(fit_24) ## leverage values (hi)
res$cooks_dis <- cooks.distance(fit_24) ## Cook's distance
res$dffits <- dffits(fit_24) ## Dffit
res$dfbetas <- dfbetas(fit_24) ## Dfbetas
res$cov_ratio <- covratio(fit_24) ## Covariance Ratio

write.csv(res,"res.csv")

##################################### Checking of Assumption ############################################

# residuals should be uncorrelated ##Autocorrelation
# Null H0: residuals from a linear regression are uncorrelated. Value should be close to 2. 
#Less than 1 and greater than 3 -> concern
## Should get a high p value

durbinWatsonTest(fit_24)
#Since, the p-value is >0.05, we fail to reject H0: (No Autocorrelation)

# Checking multicollinearity
vif(fit_24) # should be within 2. If it is greater than 10 then serious problem

################ Constant error variance ##########Heteroscedasticity


# Breusch-Pagan test
bptest(fit_24)  # Null hypothesis -> error is non-homogenious (p value should be more than 0.05)

resids<-fit_24$residuals
ad.test(resids) #get Anderson-Darling test for normality 
# Hence we can conclude that the Train data do not follow the Normal Distribution.

########################################################################################################
                       # Testing the final model on Test Data 
##########################################################################################################
fit_test<-lm(price ~ I(carbody=="wagon") +  wheelbase  +  I(cylindernumber=="four")  +  horsepower, data = Test_Data3) 
summary(fit_test)


#Check Vif, vif>2 means presence of multicollinearity
vif(fit_test)

## Get the predicted or fitted values

fitted(fit_test)
par(mfrow=c(2,2))
plot(fit_test)


#Calculating Coefficient of Determination 
attach(Test_Data3)
pred_test<-predict(fit_test)
sse<-sum((Test_Data3$price-pred_test)^2)
sst<-sum((Test_Data3$price-mean(Test_Data3$price))^2)
1-sse/sst


## Calculated Test MAPE VALUE
MAPE_Test<-(sum((abs(price-pred_test))/price)/nrow(Test_Data3))
MAPE_Test

## Residual Analysis
res1 <- Test_Data3

res1$stu_res <- studres(fit_test) ##Studentized residuals
res1$stud.del.resids <- rstudent(fit_test) ##studentized deleted residuals
res1$leverage <- hatvalues(fit_test) ## leverage values (hi)
res1$cooks_dis <- cooks.distance(fit_test) ## Cook's distance
res1$dffits <- dffits(fit_test) ## Dffit
res1$dfbetas <- dfbetas(fit_test) ## Dfbetas
res1$cov_ratio <- covratio(fit_test) ## Covariance Ratio

write.csv(res1,"res1.csv")
durbinWatsonTest(fit_test)
dwt(fit_test)
#Since, the p-value is < 0.05, Autocorrelation

# Checking multicollinearity
vif(fit_test) # should be within 2. If it is greater than 10 then serious problem

################ Constant error variance ##########Heteroscedasticity


# Breusch-Pagan test
#Interpretation of Breusch-Pagan test in R
#A p-Value > 0.05 indicates that the null hypothesis(the variance is unchanging in the residual) can be rejected and therefore heterscedasticity exists.
bptest(fit_test) 


#If the p value is low (e.g., <=0.05), you conclude that the data do not follow the normal distribution.
resids_test<-fit_test$residuals
ad.test(resids_test) #get Anderson-Darling test for normality 
# Hence we can conclude that the test data do not follow the Normal Distribution.

#For test data and train data, R-squrared values are 0.77 and 0.73 respectively.

#Adjusted R-squared value for train data & test data is 0.77 and 0.73 respectively.

#p-values for all the features has less than 0.05 and it is statistically significant.

#Hence, 85% of variance explained.Model fit is significant.
