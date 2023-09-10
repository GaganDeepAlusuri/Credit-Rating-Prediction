rm(list=ls())
library(rio)

credit.df = import('CreditRating.xlsx')

View(credit.df)

#Creating a utilization ratio using Balance and Limit
credit.df$utilization = round((credit.df$Balance / credit.df$Limit) * 100,1)

#shape of y
plot(density(credit.df$Rating), main = 'Rating distribution') #right skewed

#plotting Rating vs. Utilization ratio
plot(credit.df$Rating , credit.df$utilization)

#Plotting Rating vs. Income
plot(credit.df$Rating, credit.df$Income)

#correlation with y, correlations within x
library(dplyr)
numCols = select_if(credit.df, is.numeric)
library(corrplot)
cor(numCols)
corrplot(cor(numCols), method = 'number')

#Rating has high correlation with Limit, Income,Balance and Utilization.

#Higher limit indicates high-credit scores because people with proper payment history and credit util have lead to better scores.

#Income has no impact on credit score.
plot(credit.df$Balance, credit.df$Rating)
#model a
m1 = lm(Rating ~Income + Limit + Cards+ Balance, data = credit.df)
summary(m1)
#model b
m2 = lm(Rating ~ Income + Cards, data = credit.df)
summary(m2)
#model c
m3 = lm(Rating ~ Income  + utilization , data=credit.df)
summary(m3)

#Linearity
plot(credit.df$Rating,m3$fitted.values, pch=19,main="Ratings Actual v. Fitted Values")
abline(0,1,col="red",lwd=3)

#Normality
qqnorm(m3$residuals,pch=19,main="Rating Normality Plot")
qqline(m3$residuals,col="red",lwd=3)
hist(m3$residuals,col="red", main="Histogram of Residuals")
hist(rstandard(m3),xlim=c(-4,4),col="red", main="Histogram of Standardized Residuals")

#Equality of Variances
#Here we should not observe any pattern. If pattern, the residuals are not normal.
plot(m3$fitted.values,rstandard(m3),pch=19,main="Rating Standardized Residuals") #Scaling the residuals. Take the pennies and convert them into z values
abline(0,0,col="red",lwd=3)

#Comparing models
library(stargazer)
stargazer(m1,m2, m3, title="Results", align=TRUE,out="creditRatingModels.html")



credit.df$Gender = as.factor(credit.df$Gender)
#credit.df$Gender = relevel(credit.df$Gender, 'Female')
credit.df$Ethnicity = as.factor(credit.df$Ethnicity)
str(credit.df)
m4 = lm(Rating ~ Gender + Ethnicity, data=credit.df)
summary(m4)

x = lm(Rating ~ ., data = credit.df)
summary(x)
