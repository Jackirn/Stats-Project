library( car )
library( ellipse )
library( faraway )
library( leaps )
library(MASS)
library( GGally)
library(BAS)
library( Matrix )
library(rgl)
library(corrplot)
library(RColorBrewer)
library(readr)
library(RColorBrewer)
library(stats)
library(boot)

dataset_anova=dataset #mi serve per dopo
dataset_anova_2=dataset #mi serve per dopo
dataset_poli=subset(dataset,dataset$X2025.Rank==111) #levo il poli
dataset=subset(dataset,dataset$X2025.Rank!=111) 
names(dataset)
summary(dataset) #ci sono degli NA
sum(is.na(dataset))
dataset=na.omit(dataset) #levo gli NA
sum(is.na(dataset))
summary(dataset)
n=nrow(dataset)
n #587 ho queste università

dataset=subset(dataset, select = -X2024.Rank)
dataset=subset(dataset, select = -Employer.Reputation)
dataset=subset(dataset, select = -International.Faculty)
dataset=subset(dataset, select = -International.Research.Network)
dataset=subset(dataset, select = -Location.Full)



head(dataset) #spiegarlo brevemente

x11()
ggpairs(dataset[,c('QS.Overall.Score','Size','Academic.Reputation','Faculty.Student','Citations.per.Faculty','International.Students','Employment.Outcomes','Sustainability')],pch=16)




set.seed(5)
train = sample(n,0.8*n)
data_train = dataset[train,]
data_test = dataset[-train,]
lm.fit = lm(QS.Overall.Score ~ Size + Academic.Reputation + Faculty.Student + Citations.per.Faculty + International.Students + Employment.Outcomes + Sustainability, data = data_train) 
summary(lm.fit)

#tolgo la size
lm.fit = lm(QS.Overall.Score ~ Academic.Reputation + Faculty.Student + Citations.per.Faculty + International.Students + Employment.Outcomes + Sustainability, data = data_train) 
summary(lm.fit)

# Calcoliamo il MSE sul test set
mean( (data_test$QS.Overall.Score - predict(lm.fit,data_test) )^2 )
# Mse sul training set
mean( (lm.fit$residuals )^2 )



set.seed(1)
glm.fit = glm(QS.Overall.Score ~ Academic.Reputation + Faculty.Student + Citations.per.Faculty + International.Students + Employment.Outcomes + Sustainability, data = dataset)
summary(glm.fit)
#### Cross-Validazione Leave-One-Out 
cv.err = cv.glm(dataset,glm.fit) # NB: non stiamo specificando nulla su K, quindi K=n!
cv.err$delta[1]  # L'errore in crossvalidazione
## Se voglio K-fold, per esempio con 5 folds
cv.err = cv.glm(dataset,glm.fit,K = 5) 
cv.err$delta[1]



step(lm.fit, direction="both",trace=T) #minimizziamo AIC
vif(lm.fit)


#massimizziamo R2_adj
x=model.matrix(lm.fit)[ , -1]
y=data_train$QS.Overall.Score
adjr=leaps(x,y,method="adjr2")
adjr

bestmodel_adjr2_ind = which.max( adjr$adjr2 )
adjr$which[ bestmodel_adjr2_ind, ] 
maxadjr( adjr, 5 ) #prendiamo quello più semplice 1,2,3,4


######################################
set.seed(1)
g = glm(QS.Overall.Score ~ Academic.Reputation + Faculty.Student + Citations.per.Faculty + International.Students, data = dataset)
summary(glm.fit)
lm.fit = lm(QS.Overall.Score ~ Academic.Reputation + Faculty.Student + Citations.per.Faculty + International.Students, data = dataset)
summary(lm.fit)
#### Cross-Validazione Leave-One-Out 
cv.err = cv.glm(dataset,g) # NB: non stiamo specificando nulla su K, quindi K=n!
cv.err$delta[1]  # L'errore in crossvalidazione
## Se voglio K-fold, per esempio con 10 folds
cv.err = cv.glm(dataset,g,K = 10) 
cv.err$delta[1]
mean( (g$residuals )^2 )
#####################################


g = lm(QS.Overall.Score ~ Academic.Reputation + Faculty.Student + Citations.per.Faculty + International.Students, data = data_train)
summary(g)

AIC(g)
vif(g)
X=data_train [c(-1,-2,-3,-4,-9,-10,-11)]
x11()
corrplot(cor(X), method='number')
ggpairs(data_train[,c('Academic.Reputation','Faculty.Student','Citations.per.Faculty','International.Students')])


#validità modello: vediamo se ora i residui sono normali

#errori normali
qqnorm(g$res) #ottimo
qqline(g$res)

#shapiro test
shapiro.test(g$res) #p-value alto
hist(g$res)
boxplot(g$res)

#omoschedasticità
plot(g$fit,g$res) #sono in media attorno allo zero e non ci sono pattern strani tipo aumento o diminuzione
plot(g,which=1)


b=boxcox(g, data = data_train)
best_lambda_ind=which.max(b$y)
best_lambda=b$x[best_lambda_ind]
best_lambda



#analisi punti influenti

#leverages
p=g$rank #p=5
n=dim(data_train)[1] #n=469
lev=hatvalues(g)
plot(g$fitted.values,lev)
abline(h=2*(p/n))
watchout_points_lev=lev[which(lev>2*(p/n))]
watchout_ids_lev=seq_along(lev)[which(lev>2*p/n)]


#residui standardizzati
gs=summary(g)
res_std=g$res/gs$sigma
plot(g$fitted.values,res_std)
abline(h=c(-2,2))
watchout_ids_rstd = which( abs( res_std ) > 2 )
watchout_rstd = res_std[ watchout_ids_rstd ]
watchout_rstd

#residui studentizzati
stud=rstandard(g)
plot(g$fitted.values,stud)
abline(h=c(-2,2))
watchout_ids_stud = which( abs( stud ) > 2 )
watchout_stud = stud[ watchout_ids_stud ]
watchout_stud


x11()
par( mfrow = c( 1, 3 ) )
plot( g$fitted.values, res_std, pch = 16, xlab = 'Fitted values', 
      ylab = 'Standardized residuals', main = 'Standardized residuals' )
points( g$fitted.values[ watchout_ids_rstd ], res_std[ watchout_ids_rstd], 
        col = 'green', pch = 16 )
plot( g$fitted.values, stud, pch = 16, xlab = 'Fitted values', 
      ylab = 'Studentized Residuals', main = 'Studentized Residuals' )
points( g$fitted.values[ watchout_ids_stud ], stud[ watchout_ids_stud ], 
        col = 'pink', pch = 16 )
plot( g$fitted.values, lev, pch = 16, xlab = 'Fitted values', 
      ylab = 'Leverages', main = 'Leverages' )
points( g$fitted.values[ watchout_ids_lev ], lev[ watchout_ids_lev ],
        col = 'orange', pch = 16 )



g_post_lev=lm( QS.Overall.Score ~ Academic.Reputation + Faculty.Student + Citations.per.Faculty + International.Students , data=data_train, subset = ( lev<2*p/n))
summary(g_post_lev)
AIC(g_post_lev)

#validità modello: vediamo se ora i residui sono normali

#errori normali
qqnorm(g_post_lev$res)
qqline(g_post_lev$res)

#shapiro test
shapiro.test(g_post_lev$res) #p-value basso
hist(g_post_lev$res)
boxplot(g_post_lev$res)

#omoschedasticità
plot(g_post_lev$fit,g_post_lev$res) #sono in media attorno allo zero e che aumentano, proviamo ad individuare i punti influenti
plot(g_post_lev,which=1)


g_post_rs=lm( QS.Overall.Score ~ Academic.Reputation + Faculty.Student + Citations.per.Faculty + International.Students, data=data_train, subset = ( abs(stud)<2 ) )
summary(g_post_rs)
AIC(g_post_rs) #si abbassa ma perdiamo noramlità

#validità modello: vediamo se ora i residui sono normali

#errori normali
qqnorm(g_post_rs$res)
qqline(g_post_rs$res)


#shapiro test
shapiro.test(g_post_rs$res)
hist(g_post_rs$res)
boxplot(g_post_rs$res)

#omoschedasticità
plot(g_post_rs$fit,g_post_rs$res) #sono in media attorno allo zero e che aumentano, proviamo ad individuare i punti influenti
plot(g_post_rs,which=1)


g_post_both=lm( QS.Overall.Score ~ Academic.Reputation + Faculty.Student + Citations.per.Faculty + International.Students , data=data_train, subset = ( abs(stud)<2 | lev<2*p/n ))
summary(g_post_both)
AIC(g_post_both)

#validità modello: vediamo se ora i residui sono normali

#errori normali
qqnorm(g_post_both$res)
qqline(g_post_both$res)


#shapiro test
shapiro.test(g_post_both$res)
hist(g_post_both$res)
boxplot(g_post_both$res)

#omoschedasticità
plot(g_post_both$fit,g_post_both$res) #sono in media attorno allo zero e che aumentano, proviamo ad individuare i punti influenti
plot(g_post_both,which=1)


#Interpretazione
summary(g_post_both)



#Previsione
names(dataset_poli)
dataset_poli=subset(dataset_poli, select = -c(X2025.Rank,X2024.Rank,Location,Location.Full,Size,Employer.Reputation,International.Faculty,International.Research.Network,Employment.Outcomes,Sustainability))

#Stima puntuale di y_hat
y_predict=predict(g_post_both,dataset_poli)
y_predict
y_mano=g_post_both$coefficients[1]+g_post_both$coefficients[2]*dataset_poli$Academic.Reputation+g_post_both$coefficients[3]*dataset_poli$Faculty.Student+g_post_both$coefficients[4]*dataset_poli$Citations.per.Faculty+g_post_both$coefficients[5]*dataset_poli$International.Students
y_mano

#Intervallo di previsione per y_hat
y_confidence=predict(g_post_both,dataset_poli,interval='prediction',level=0.95)
y_confidence


#Intervallo di confidenza per E[y_hat]
y_confidence_E=predict(g_post_both,dataset_poli,interval='confidence',level=0.95)
y_confidence_E


#ANOVA su US, UK, FR, DE, IT n=5
names(dataset_anova)
dataset_anova=subset(dataset_anova,select=c(QS.Overall.Score,Location))
dataset_anova=subset(dataset_anova,dataset_anova$Location %in% c('US','UK','FR','DE','IT'))
summary(dataset_anova)
my_colors=brewer.pal(5,'Set2')
boxplot( QS.Overall.Score ~ Location, data=dataset_anova, col = my_colors)

#Ipotesi ANOVA

#Normalità dei gruppi
Ps = tapply( dataset_anova$QS.Overall.Score,dataset_anova$Location , function( x ) ( shapiro.test( x )$p ) )
Ps


dataset_IT=subset(dataset_anova,dataset_anova$Location=='IT')
shapiro.test(dataset_IT$QS.Overall.Score)

reg=lm(QS.Overall.Score~Location,data=dataset_anova)
summary(reg)
anB=boxcox(reg,lambda = seq(-3,3,by=0.01))
best_lambda=anB$x[which.max(anB$y)]
best_lambda


Ps = tapply( (dataset_anova$QS.Overall.Score ^ best_lambda - 1)/best_lambda,dataset_anova$Location , function( x ) ( shapiro.test( x )$p ) )
Ps

boxplot( (QS.Overall.Score ^ best_lambda - 1)/best_lambda ~ Location, data=dataset_anova, col = my_colors)

#Omoschedasticità

leveneTest( (dataset_anova$QS.Overall.Score ^ best_lambda - 1)/best_lambda ,dataset_anova$Location )
bartlett.test(  (dataset_anova$QS.Overall.Score ^ best_lambda - 1)/best_lambda ,dataset_anova$Location )


mod = lm( QS.Overall.Score ~ Location , data=dataset_anova)
summary( mod )
anova( mod )



#ANOVA su UK, FR, DE, IT n=4
names(dataset_anova)
dataset_anova=subset(dataset_anova,dataset_anova$Location %in% c('UK','FR','DE','IT'))
summary(dataset_anova)
my_colors=brewer.pal(4,'Set2')
x11()
boxplot( QS.Overall.Score ~ Location, data=dataset_anova, col = my_colors)
abline( h = mean( dataset_anova$QS.Overall.Score ) )



#Ipotesi ANOVA

#Normalità dei gruppi
Ps = tapply( dataset_anova$QS.Overall.Score,dataset_anova$Location , function( x ) ( shapiro.test( x )$p ) )
Ps


dataset_IT=subset(dataset_anova,dataset_anova$Location=='IT')
shapiro.test(dataset_IT$QS.Overall.Score)

reg=lm(QS.Overall.Score~Location,data=dataset_anova)
summary(reg)
anB=boxcox(reg,lambda = seq(-3,3,by=0.01))
best_lambda=anB$x[which.max(anB$y)]
best_lambda


Ps = tapply( (dataset_anova$QS.Overall.Score ^ best_lambda - 1)/best_lambda,dataset_anova$Location , function( x ) ( shapiro.test( x )$p ) )
Ps

x11()
boxplot( (QS.Overall.Score ^ best_lambda - 1)/best_lambda ~ Location, data=dataset_anova, col = my_colors)


#Omoschedasticità

leveneTest( (dataset_anova$QS.Overall.Score ^ best_lambda - 1)/best_lambda ,dataset_anova$Location )
bartlett.test(  (dataset_anova$QS.Overall.Score ^ best_lambda - 1)/best_lambda ,dataset_anova$Location )


mod = lm( (QS.Overall.Score ^ best_lambda - 1)/best_lambda ~ Location , data=dataset_anova)
summary( mod )
anova( mod )



#ANOVA su US IT n=2
#attento a rifare il dataset anova
names(dataset_anova_2)
dataset_anova_2=subset(dataset_anova_2,select=c(QS.Overall.Score,Location))
dataset_anova_2=subset(dataset_anova_2,dataset_anova_2$Location %in% c('US','IT'))
summary(dataset_anova_2)
my_colors=brewer.pal(2,'Set2')
x11()
boxplot( QS.Overall.Score ~ Location, data=dataset_anova_2, col = my_colors)
abline( h = mean( dataset_anova$QS.Overall.Score ) )


#Ipotesi ANOVA

#Normalità dei gruppi
Ps = tapply( dataset_anova_2$QS.Overall.Score,dataset_anova_2$Location , function( x ) ( shapiro.test( x )$p ) )
Ps


dataset_IT=subset(dataset_anova_2,dataset_anova_2$Location=='IT')
shapiro.test(dataset_IT$QS.Overall.Score)

reg=lm(QS.Overall.Score~Location,data=dataset_anova_2)
summary(reg)
anB=boxcox(reg,lambda = seq(-3,3,by=0.01))
best_lambda=anB$x[which.max(anB$y)]
best_lambda


Ps = tapply( (dataset_anova_2$QS.Overall.Score ^ best_lambda - 1)/best_lambda,dataset_anova_2$Location , function( x ) ( shapiro.test( x )$p ) )
Ps

boxplot( (QS.Overall.Score ^ best_lambda - 1)/best_lambda ~ Location, data=dataset_anova_2, col = my_colors)


#Omoschedasticità

leveneTest( (dataset_anova_2$QS.Overall.Score ^ best_lambda - 1)/best_lambda ,dataset_anova_2$Location )
bartlett.test(  (dataset_anova_2$QS.Overall.Score ^ best_lambda - 1)/best_lambda ,dataset_anova_2$Location )


mod = lm( QS.Overall.Score ~ Location , data=dataset_anova_2)
summary( mod )
anova( mod )
