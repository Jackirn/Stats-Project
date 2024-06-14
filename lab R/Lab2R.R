
## "Laboratorio con `R` - 2"
## Metodi e Modelli per l'Inferenza Statistica - Ing. Matematica - a.a. 2023-24

## Topics:  
## - Analisi dei punti influenti  
## - Collinearità e non linearità  
## - Trasformazione di variabili  


## 0. Required packages

library( car )
library( ellipse )
library( faraway )
library( leaps )
library(MASS)
library( GGally)
library(BAS)
library(rgl)
library(corrplot)


## 1. Linear regression (refresh).

##__1.a__  Let's start from the linear model we built in the Laboratory 1. Upload `faraway` library and the dataset  
# `savings`, an economic dataset on 50 different countries.
##These data are averages over 1960-1970 ( to remove business cycle or other short-term fluctuations ).

# import data
savings = read.table(file='savings.txt', header =T)

# Dimensions
dim(savings)
str(savings)
#We have 50 observations (50 countries) with 5 attributes each. 

#Look at the main statistics for each covariate:

summary(savings)

str(savings)



##__1.b__ Visualize the data and fit the complete linear model, in which `sr` is the outcome of interest.

# For visualizing the data, we can plot the pairs or ggpairs. 
# It is useful also for making an idea about the relationship between the covariates.

ggpairs(data = savings, title ="Relationships between predictors & response", 
lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))

# Secondly, we fit the complete linear model (Lab 1) and look at the summary of the estimated coefficients. 
g = lm( sr ~ pop15 + pop75 + dpi + ddpi, data = savings )
#g = lm( sr ~ ., savings )
summary( g )

gs = summary( g )


## 2. Diagnostics: detecting influential points

#The goal of diagnostics consists in detecting possible influential points in a sample. 
#In general, an influential point is one whose removal from the dataset would cause a large change in the fit.
#Influential points are outliers and leverages (in italiano, punti leva). The definitions of outliers and 
#leverages can overlap. A possible definition of outlier is 'a point that does not fit the chosen model'. 
#On the other hand, a leverage is 'a point that significantly affects the estimates of the model'. 
#It is immediate to see that often an outlier is also a leverage point.

#By 'influential', we mean that :

#1. estimated coefficients with or without an observation significantly changes:  hat{beta} - hat{beta}_{-i}
#2. fitted values with or without an observation significantly changes:  x^T(hat{beta} - hat{beta}_{-i}) = hat{y} - hat{y}_{-i}
#Anyway, these are hard measures to judge in the sense that the scale varies between datasets.

#There are several approaches for identifying influential points in a sample, such as:

#* __a. Leverages (projection matrix)__

#* __b. Standardized Residuals__

#* __c. Studentized Residuals__  

#* __d. Cook's Distance__


### a. Leverages

# Investigate possible leverages among data.
# We can compute diagonal elements of H matrix with two different functions:

X = model.matrix( g )
X

lev = hat( X )
lev
# or 
lev = hatvalues( g )
lev

# Punto che fa cambiare tanto il problema (outlier)

#Alternatively, we can compute H manually and then extract its diagonal elements:
#manually
H = X %*% solve( t( X ) %*% X ) %*% t( X ) 
lev = diag( H )

sum(lev) # verifica: sum_i hat( x )_i = p = r+1


# __Rule of thumb:__ Given a point h_ii diagonal element of H, the i-th observation is a leverage if:
#  h_ii > 2*(p)/n

p = g$rank  
n = dim(savings)[1] # n = 50


plot( g$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages", 
      pch = 16, col = 'black' )

abline( h = 2 * p/n, lty = 2, col = 'red' )

watchout_points_lev = lev[ which( lev > 2 * p/n  ) ]
watchout_points_lev
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ] ## identify the rows relative to leverage points
watchout_ids_lev

points( g$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16 )

lev [ lev >  2 * 5 / 50 ]
sum( lev [ lev >  2 * 5 / 50 ] )

#Fit the model without leverages.

gl = lm( sr ~ pop15 + pop75 + dpi + ddpi, savings, subset = ( lev < 0.2 ) )
summary( gl )
#summary( g )

# Moreover, investigate the relative variation of hat(beta) due to these influential points.

abs( ( g$coefficients - gl$coefficients ) / g$coefficients )

# The leverages affect the estimates heavily (there is a variation of 22\% at least).

# We can also visualize the position of leverages for each covariate couple.

colors = rep( 'black', nrow( savings ) )
colors[ watchout_ids_lev ] = c('red', 'blue', 'green', 'orange')

pairs( savings[ , c( 'sr', 'pop15', 'pop75', 'dpi', 'ddpi' ) ], 
       pch = 16, col = colors, cex = 1 + 0.5 * as.numeric( colors != 'black' ))


### b. Standardized Residuals

#Plot the residuals of the complete model.

# Residui non standardizzati (e non studentizzati)

plot( g$res, ylab = "Residuals", main = "Plot of residuals" )

sort( g$res )
sort( g$res ) [ c( 1, 50 ) ]  ## per vedere il primo e l'ultimo residuo

countries = row.names( savings )

identify( 1:50, g$res, countries ) 
# click 2 times on the points you want to make a label appear
# it works only by console and plots window

#Plot the __Standardized Residuals__ of the complete model.

#__Rule of thumb__ A point i is influential if |r_i^{std}|>2

# It is easy to see that influential points according to standardized residuals and to leverages are different.

gs = summary(g)
res_std = g$res/gs$sigma
watchout_ids_rstd = which( abs( res_std ) > 2 )
watchout_rstd = res_std[ watchout_ids_rstd ]
watchout_rstd

# Plot Residui standardizzati 

plot( g$fitted.values, res_std, ylab = "Standardized Residuals", main = "Standardized Residuals" )
abline( h = c(-2,2), lty = 2, col = 'orange' )
points( g$fitted.values[watchout_ids_rstd], 
        res_std[watchout_ids_rstd], col = 'red', pch = 16 )
points( g$fitted.values[watchout_ids_lev], 
        res_std[watchout_ids_lev], col = 'orange', pch = 16 )
legend('topright', col = c('red','orange'), 
       c('Standardized Residuals', 'Leverages'), pch = rep( 16, 2 ), bty = 'n' )


### c. Studentized Residuals
# #__Rule of thumb__ A point i is influential if |r_i^{stud}|>2

#Compute the Studentized Residuals, highlighting the influential points.

gs = summary( g )

gs$sigma

# manually
stud = g$residuals / ( gs$sigma * sqrt( 1 - lev ) )

# 'rstandard' gives studentized residuals automatically
stud = rstandard( g )

watchout_ids_stud = which( abs( stud ) > 2 )
watchout_stud = stud[ watchout_ids_stud ]
watchout_stud


plot( g$fitted.values, stud, ylab = "Studentized Residuals", main = "Studentized Residuals", pch = 16 )
points( g$fitted.values[watchout_ids_stud], 
        stud[watchout_ids_stud], col = 'pink', pch = 16 )
points( g$fitted.values[watchout_ids_lev], 
        stud[watchout_ids_lev], col = 'orange', pch = 16 )
abline( h = c(-2,2), lty = 2, col = 'orange' )
legend('topright', col = c('pink','orange'), 
       c('Studentized Residual', 'Leverages'), pch = rep( 16, 3 ), bty = 'n' )

# Studentized residuals and Standardized residuals identify the same influential points in this case.

### d. Cook's distance

# __Rule of thumb__ C_i > {4}/{n-p}

Cdist = cooks.distance( g )

watchout_ids_Cdist = which( Cdist > 4/(n-p) ) 
watchout_Cdist = Cdist[ watchout_ids_Cdist ]
watchout_Cdist

#Three suspect points are detected. 

par( mfrow = c( 1, 3 ) )
plot( g$fitted.values, Cdist, pch = 16, xlab = 'Fitted values', 
ylab = 'Cooks Distance', main = 'Cooks Distance' )
points( g$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ], 
col = 'green', pch = 16 )
plot( g$fitted.values, stud, pch = 16, xlab = 'Fitted values', 
ylab = 'Studentized Residuals', main = 'Studentized Residuals' )
points( g$fitted.values[ watchout_ids_stud ], stud[ watchout_ids_stud ], 
col = 'pink', pch = 16 )
plot( g$fitted.values, lev, pch = 16, xlab = 'Fitted values', 
ylab = 'Leverages', main = 'Leverages' )
points( g$fitted.values[ watchout_ids_lev ], lev[ watchout_ids_lev ],
col = 'orange', pch = 16 )

par( mfrow = c( 1, 1 ) )

#Fit the model without influential points wrt Cook's distance and compare the outcome to the 
#former model (on the complete dataset).

#id_to_keep = (1:n)[ - watchout_ids_Cdist ]
id_to_keep = !( 1:n %in% watchout_ids_Cdist )

gl = lm( sr ~ pop15 + pop75 + dpi + ddpi, savings[ id_to_keep, ]  ) 

summary( gl )

#Observe that the fitting in terms of $R^2$ slightly improved wrt to the complete model.

abs( ( gl$coef - g$coef )/g$coef )

#The coefficient for dpi changed by about 64%, the coefficient of pop75 by 82%.

# __All together: Influential Plot__

# The influential plot represents the studentized residuals vs leverages, 
# and highlights them with a circle which is proportional to Cook's distance.
x11()
influencePlot( g, id.method = "identify", main = "influential Plot",
sub = "Circle size is proportial to Cook's Distance" )

#There is another easy way to visually detect the influential points by Cook's distance.

plot(g, which = 5)

#__All together: Influential measures__

#`influential.measures` produces a class "infl" object tabular display showing several 
#diagnostics measures (such as h_{ii} and Cook's distance).
#Those cases which are influential with respect to any of these measures are marked with an asterisk.

influence.measures( g )
# DFBETA measures the difference in each parameter estimate with and without the influential point


## 3. Hypotheses of the model

# In Laboratory 1, we analysed the normality and homoschedasticity of the residuals. 
# Let's now look at the collinearity and nonlinearity.

### Nonlinearity/Collinearity

#__Partial regression plots__

#Partial Regression or Added Variable plots can help isolate the effect of x_i on y.
# 1. Regress y on all x except x_i, get residuals hat{delta}. This represents y with the other X-effect taken out.
# 
# 2. Regress x_i on all x except x_i, get residuals hat{gamma}.This represents x_i with the other X-effect taken out.
# 
# 3. Plot hat{delta} against hat{gamma}
#   
# The slope of a line fitted to the plot adds some insight into the meaning of regression coefficients. 
# Look for non-linearity and outliers and/or influential points.


#We construct a partial regression (added variable) plot for pop15:

d <- lm(sr ~ pop75 + dpi + ddpi,savings)$res
m <- lm(pop15 ~ pop75 + dpi + ddpi,savings)$res
x11()
plot(m,d,xlab="pop15 residuals",ylab="Saving residuals", main="Partial Regression")
abline(0,g$coef['pop15'])

#Compare the slope on the plot to the original regression and show the line on the plot.
lm(d ~ m)$coef

g$coef

#Notice how the slope in the plot and the slope for pop15 in the regression fit are the same. 

# __Partial Residual plots__

# They are a competitor to added variable plots. These plot epsilon_i + beta_i x_i against x_i. 
# The slope on the plot will have the same interpretation of Partial regression plots. 
# Partial residual plots are reckoned to be better for non-linearity detection while added variable plots 
# are better for outlier/influential detection.
#A partial residual plot is easier to do:

prplot(g,1) # 1 stands for the position of the independent variable  --> library(faraway)


##__VIF__ Variance Inflation Factor is an index of collinearity. 

#Collinearity means that two covariate share a lot of variability and thus are likely to carry the same information.

vif( g )

#In this case, __pop75__ and __pop15__ show the highest collinearity. (Rule of thumb: VIF > 5 or 10)


## 4. Transformation: Box-Cox

#In this section we would like to answer the following question: what should we do when there is
#a clear violation of hypotheses?
#The answer consists in investigating variable transformations (transformation of the outcome).

#__Warning__ Transforming a variable can lead to a more difficult interpretation of the model.

#An algorithm that helps us in variable transformation is the *Box-Cox* algorithm.
#It detects the best lambda among a family of transformations ({y^lambda-1}/{lambda}, if lambda !=0, 
#otherwise log(y) ) in order to gain the Normality/homoscedasticity for *positive data*. 

#Here we report an example (linear regression with one predictor). 
#For a group of 252 male subjects, various body measurements were obtained. An accurate measurement of the 
#percentage of body fat is recorded for each. The goal is to use the feature 'Abdomen' 
#(indicating abdomen circumference (cm)) for predicting the weight. 


library(BAS)
data(bodyfat) # help(bodyfat) 
# summary(bodyfat)
mod = lm(Weight ~ Abdomen, data = bodyfat)
summary(mod)


mod_res = mod$residuals/summary(mod)$sigma

x11()
plot( mod$fitted, mod_res, xlab = 'Fitted values',  ylab = 'Standarzized residuals'  )

qqnorm( mod$residuals )
qqline( mod$residuals, col = 'blue' )
# abline( 0, 1, col = 'red' )

shapiro.test( mod_res )

#Very good fit of the model: R^2 = 78.9% and the predictor is significant with a p-value < 2e-16. 
#Nonetheless, the normality assumption cannot be accepted with a lot of confidence: QQ plots show heavy tails 
#(especially the right one) and Shapiro-Wilks test return a p-value of 0.02412. 

# So, we apply the Box-Cox transformation.
# *Remark* We can apply the Box-Cox transformation, because variable is positive.

#The best lambda that is chosen is the one maximizing the likelihood of the transformed data of being 
b = boxcox(Weight ~ Abdomen, data = bodyfat)
names(b)
#y likelihood evaluation
#x lambda evaluated
best_lambda_ind = which.max( b$y )
best_lambda = b$x[ best_lambda_ind ]
best_lambda


#We can see that the best transformation is the one related to the *maximum* of the curve.
#According to this method, the best $\lambda$ is $ 0.2626263$.

#Finally, we test the new model and we investigate the standardized residuals.
mod1 = lm( (Weight ^ best_lambda - 1)/best_lambda ~ Abdomen, data = bodyfat )
summary(mod1)

mod1_res = mod1$residuals/summary( mod1 )$sigma

plot( mod1$fitted, mod1_res, xlab = 'Fitted values',  ylab = 'Standarzized residuals'  ) 


qqnorm( mod1_res )
abline( 0, 1, col = 'red' )


shapiro.test( residuals( mod1 ) ) 

#The normality of residuals improved after Box-Cox Transformation.


