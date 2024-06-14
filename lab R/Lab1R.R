## "Laboratorio con `R` - 1"
## Metodi e Modelli per l'Inferenza Statistica - Ing. Matematica - a.a. 2023-24

## Topics:  
## - Introduction to linear regression  
## - Analysis of linear regression components  
## - Parameters estimation  
## - Analysis of residuals

## 0. Required packages

library( car )
library( ellipse )
library( faraway )
library( leaps )
library(MASS)
library( GGally)
library(rgl)
# library( qpcR )

## 1. Linear regression and tests for coefficients significance.

#__1.a__ Upload `faraway` library and the dataset `savings`, an economic dataset on 50 different countries.
#These data are averages over 1960-1970 ( to remove business cycle or other short-term fluctuations ).
#The recorded variables are:

#* __sr__ is aggregate personal saving divided by disposable income ( risparmio personale diviso per il reddito disponibile ).
#* __pop15__ is the percentage population under 15.
#* __pop75__ is the percentage population over 75.
#* __dpi__ is per-capita disposable income in U.S. dollars ( reddito pro-capite in dollari, al netto delle tasse ).
#* __ddpi__ is the rate [percentage] of change in per capita disposable income ( potere d'acquisto - indice economico aggregato, espresso in % ).

# Create a summary of the data. How many variables have missing data? Which are quantitative and which are qualitative?

# __Solution__

# import data
data(savings)
View(savings)

# Dimensions
dim(savings)

#We have 50 observations (50 countries) with 5 attributes each. To visualize the first 5:
  
# Overview of the first rows
head(savings)

#Look at the main statistics for each variable:
summary(savings)

#If missing values were present, 'summary' function would have informed us. To check it directly:
# observe that there are no missing values
sum(is.na(savings))  # na.omit(savings)
print(sapply(savings,function(x) any(is.na(x))))

#Finally we get the data type of each column:
# check the type of each column (integer, double, character, ...)
print(sapply(savings, typeof))

# or
str(savings)


#__1.b__ Visualize the data and try to fit a complete linear model, in which `sr` is the outcome of interest. 
#        Explore the output of the model. 

#__solution__
#For visualizing the data, we can plot the pairs. It is useful also for making an idea about the relationship between the variables.

pairs(savings, pch=16)
pairs(savings[ , c('sr', 'pop15', 'pop75', 'dpi', 'ddpi')], pch = 16) # to plot a subset of columns (variables)

#For a nicer visualization of these scatterplots, we can use the package 'GGally'. We can easily visualize the relationship of couple of variables, 
# their sample correlation and their approximated density function.

ggpairs(data = savings, title ="Relationships between predictors & response",
        lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))

#Secondly, we can fit the complete linear model and look at a summary of the estimated coefficients. 

g = lm( sr ~ pop15 + pop75 + dpi + ddpi, data = savings )
#g = lm( sr ~ ., savings )
summary( g )

gs = summary( g )

#In order to measure the goodness of fit of the model, we have to look at $R^2$ and $R^2_{adj}$. 
#They assume values between 0 and 1 and represent the percentage of explained variability by regressors, 
#thus the more they are near to 1 the more the model explains well the dependent variable. Both of them are low in this case.

#Third and last columns of the summary represent univariate statistics and p-values related to each estimated coefficient. 
#They make us know the results of the test of estimated coefficients being equal from 0. 
#In other words, they communicate us if the ICs of $\hat{\beta_i}$ contain or not the 0. 
#Only 'pop15' and 'ddpi' seem to be significant in this model.

#Through the F-statistic, we can investigate whether there is at least one covariate's parameter among $\beta_1$, $\beta_2$, $\beta_3$ and
#$\beta_4$ which is different from 0.
#Since the p-value of F-statistic is so small (0.0007904), the null hypothesis is rejected and there is at least one 
#covariate's parameter that is different from 0.

names(g) # this gives you the attributes of the linear model object

#We can look through the model's attributes.

g$call # linear model forumla
g$coefficients #beta_hat
g$fitted.values # estimated 'sr' for each observation


#We could also compute directly the fitted values of the dependent variable:
X = model.matrix(g)
y_hat_man = X %*% g$coefficients #beta_hat

g$residuals # residuals

g$rank # the numeric rank of the fitted linear model (number of covariates + 1)


#Calculate Variance-Covariance Matrix for a Fitted Model Object
vcov( g ) 

#__1.c__ Try to compute F-test, manually. 
#Recall that the F-test says us if there is at least one estimated coefficient significantly different from 0.

SS_tot = sum( ( savings$sr-mean( savings$sr ) )^2 )
SS_res = sum( g$res^2 )

p = g$rank # p = 5 
n = dim(savings)[1] # n = 50

f_test = ( ( SS_tot - SS_res )/(p-1) )/( SS_res/(n-p) )

## p-value (right-hand area of f_test)
1 - pf( f_test, p - 1, n - p )


#__1.d__ Test the significance of the parameter $\beta_1$ (the parameter related to pop_15), manually.

#We want to test:
#H_0: \beta_1 = 0 \qquad vs \qquad H_1: \beta_1 \neq 0

# __t-test__

#We compute the test, whose output is shown in the R summary. 

X = model.matrix( g )
sigma2 = (summary( g )$sigma)^2
#manually
sigma2 = sum( ( savings$sr - g$fitted.values )^2 ) / ( n -p )
se_beta_1 = summary( g )$coef[ 2, 2 ]

#manually
se_beta_1 = sqrt( sigma2 * diag( solve( t( X ) %*% X ) )[2] )
T.0 = abs( ( g$coefficients[ 2 ] - 0 )/ se_beta_1 ) 
2*( 1-pt( T.0, n-p ) )


## Homework
#__1.e__  Test the significance of all the regression parameters, separately. 

#__1.f__ Test the regression parameter $\beta_4$ ( the one related to 'ddpi' ) for this test:
#H_0: \beta_4 = 0.35 \qquad vs \qquad  H_1: \beta_4 > 0.35


## 2. Confidence Intervals and Regions
### Confidence Intervals

#__2.a__ 
#Compute the $95\%$ confidence intervals for the regression parameter related to 'pop75'.

alpha = 0.05
t_alpha2 = qt( 1-alpha/2, n-p )
beta_hat_pop75 = g$coefficients[3]
se_beta_hat_pop75 = summary( g )[[4]][3,2]

IC_pop75 = c( beta_hat_pop75 - t_alpha2 * se_beta_hat_pop75, 
beta_hat_pop75 + t_alpha2 * se_beta_hat_pop75 )
IC_pop75

#We observe that the IC includes $0$, so there is no evidence for rejecting $H_0: \beta_2 = 0$, at the $5% level. 
# Indeed, this parameter was not significant even in the previous section (p-value $12.5\%$).
summary(g)$coef[3,4]

#__2.b__
#Compute the $95\%$ confidence intervals for the regression parameter related to 'ddpi'.

alpha = 0.05
n=50
p=5
t_alpha2 = qt( 1-alpha/2, n-p )
beta_hat_ddpi = g$coefficients[5]
se_beta_hat_ddpi = summary( g )[[4]][5,2]

IC_ddpi = c( beta_hat_ddpi - t_alpha2 * se_beta_hat_ddpi,
beta_hat_ddpi + t_alpha2 * se_beta_hat_ddpi )
IC_ddpi

#In this case, we observe that the IC does NOT include $0$, so there is evidence for rejecting $H_0: \beta_4 = 0$, at the $5\%$ level. 
#However, the lower bound of the $IC_{(1-\alpha)}(\beta_4)$ is really close to $0$. 
#We can see from the output above that the p-value is $4.2\%$ - lower than $5\%$ - confirming this point.

summary(g)$coef[5,4]

#Notice that this confidence interval is pretty wide in the sense that the upper limit is about 80 times larger than the lower limit.
#This means that we are not really that confident about what the exact effect of growth on savings really is.

#__REMARK__ Confidence intervals often have a duality with two-sided hypothesis tests.
#A 95% confidence interval contains all the null hypotheses that would not be rejected at the 5% level.

### Confidence Regions

#__2.c__
#Build the joint $95\%$ confidence region for parameters 'pop15' e 'pop75'. 
#And add the value of $(\beta_1,\beta_2)$ according to the null hypothesis.

#help( ellipse )
plot( ellipse( g, c( 2, 3 ) ), type = "l", xlim = c( -1, 0 ) )

# add the origin (we test that the coeff are (0,0)) and the point of the estimates:
points( 0, 0 )

# add also the center of the ellipse, that is, the estimated couple of coefficients
points( g$coef[ 2 ] , g$coef[ 3 ] , pch = 18 )


#The filled dot is the center of the ellipse and represents the estimates of the 2 parameters ($\hat{\beta_1}$, $\hat{\beta_2}$).
#Now, we are interested in this test:
  
#  H_0: (\beta_1,\beta_2) = (0,0) \qquad vs \qquad  H_1: (\beta_1,\beta_2) \neq (0,0)

#We observe that the empty dot ($0$,$0$) is not included in the Confidence Region (which is now an ellipse), 
#so we reject $H_0$ at $5\%$ level. In other words, we are saying that there is at least one parameter 
#between $\beta_1$ and $\beta_2$ which is not equal to $0$. 

#__REMARK__ It is important to stress that this Confidence Region is different from the one obtained by the cartesian product of 
#the two Confidence Intervals, $IC_{(1-\alpha)}(\beta_1)$ X $IC_{(1-\alpha)}(\beta_2)$. 
#The cartesian product of the two Confidence Intervals is represented by the four dashed lines.
beta_hat_pop15 = g$coefficients[2]
se_beta_hat_pop15 = summary( g )[[4]][2,2]

IC_pop15 = c( beta_hat_pop15 - t_alpha2 * se_beta_hat_pop15, 
              beta_hat_pop15 + t_alpha2 * se_beta_hat_pop15 )
IC_pop15

plot( ellipse( g, c( 2, 3 ) ), type = "l", xlim = c( -1, 0 ) )

points( 0, 0 )
points( g$coef[ 2 ] , g$coef[ 3 ] , pch = 18 )

#new part
abline( v = c( IC_pop15[1], IC_pop15[2] ), lty = 2 )
abline( h = c( IC_pop75[1], IC_pop75[2] ), lty = 2 )

#__REMARK__ The origin $(0,0)$ is included in the $IC_{(1-\alpha)}(\beta_2)$ and is NOT included in the $IC_{(1-\alpha)}(\beta_1)$, 
#as expected from the previous point (we are expecting that $\beta_1$ is significantly different from 0, while $\beta_2$ is not.)

#__REMARK__ It can happen that you could reject according to one Confidence Region and accept according to the other Confidence Region. 
#So which region should we choose?  

#* blue point: inside the the cartesian product of marginal ICs, outside the joint Confidence Region
#* red point: outside the the cartesian product of marginal ICs, inside the joint Confidence Region

plot( ellipse( g, c( 2, 3 ) ), type = "l", xlim = c( -1, 0 ) )

points( 0, 0 )
points( g$coef[ 2 ] , g$coef[ 3 ] , pch = 18 )

abline( v = c( IC_pop15[1], IC_pop15[2] ), lty = 2 )
abline( h = c( IC_pop75[1], IC_pop75[2] ), lty = 2 )

#new part
points( -0.22, 0.7, col = "red", lwd = 2 )
points( -0.71, 0, col = "blue", lwd = 2 )

#We should always refer the joint Confidence Region (the elliptic one), because it is taking into account the correlation 
#between the parameters. So we will accept the hypothesis represented by the red point and reject the hypothesis represented by the blue one.

#In this case, correlation is near to -1. This means that the variables share a lot of variability and, consequently, of information

cor( savings$pop15, savings$pop75 )


## 3. Hypotheses of the model

### Homoscedasticity 

#__3.a__ Plot residuals ( $\hat{\varepsilon}$ ) vs fitted  values ( $\hat{y}$ ).

#A sequence of random variables is homoscedastic if all its random variables have the same finite variance. 
#Homoscedasticity is a fundamental assumptions in OLS.

plot( g$fit, g$res, xlab = "Fitted", ylab = "Residuals", 
      main = "Residuals vs Fitted Values", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )  # variance seems uniform across the fitted values

#The residual vs fitted value plot does not show any particular features that challenge the normality and 
#constant variance assumptions of the residuals.

#Alternatively, we can plot
plot(g, which=1 ) 

### Normality

#__3.b__ Plot QQ plot and test the normality of the residuals with Shapiro-Wilks test.

#Normality of data is another strong assumption of the OLS model. 

# QQ plot
qqnorm( g$res, ylab = "Raw Residuals", pch = 16 )
qqline( g$res )
# linear trend, the hypothesis is verified 

# Shapiro-Wilk normality test
shapiro.test( g$res )

#Since p-value is very high, I have no evidence to reject $H_0$, which is the gaussianity of the data.

#Histogram of residuals and boxplot are useful tools to look at the shape of the residual distribution and 
#to see if the tails of the distributions are souspiciously heavy (i.e., there are many outliers) respectively.

# other useful tools...
hist( g$res, 10, probability = TRUE, col = 'lavender', main = 'residuals'  )
boxplot( g$res, main = "Boxplot of savings residuals", pch = 16, col = 'lavender' )

## Treatment of not informative covariates

summary(g)

g2 = lm(sr ~ pop15 + pop75 + ddpi, data = savings)
summary(g2)

g3 = lm(sr ~ pop15 +  ddpi, data = savings)
summary(g3)


plot3d(savings$pop15, savings$ddpi, savings$sr, xlab='pop15', ylab='ddpi', zlab='sr')
planes3d( -0.21638, 0.44283,-1, 15.59958, col = 'blue', alpha = 0.6) # ax + by + cz +d = 0


rm(list=ls())
graphics.off()

