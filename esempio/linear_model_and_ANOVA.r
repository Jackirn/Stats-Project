library(car)
library(faraway)
library(leaps)
library(MASS)
library(GGally)
library(rgl)
library(dplyr)
library(data.table)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(AID)
library(onewaytests)


# carico il dataset
scoliosi = read.csv(here::here("data","column_3C_weka.csv"), header = TRUE)

# visualizza il dataset
View(scoliosi)

# che dimensioni ha
dim(scoliosi)
# 310 osservazioni e 7 covariate, di cui 1 categorica

# overview delle prime righe
head(scoliosi)

# controllo se ci sono degli NA
print(sapply(scoliosi,function(x) any(is.na(x)))) 
print(sapply(scoliosi, typeof)) 
# tutto FALSE, non ci sono NA, altrimenti avremmo fatto na.omit(scoliosi)

# sommario del dataset
summary(scoliosi)

x11()
# faccio il mega ccpairg per avere un'idea dei dati
ggpairs(scoliosi)

# generiamo il primo modello lineare, come risposta "lumbar_lordosis_angle"
# escludiamo "class" che è la categorica
g1 <- lm(lumbar_lordosis_angle ~ . -class, data = scoliosi)

# esaminiamolo
summary(g1)

# R^2_adj iniziale abbastanza buono come punto di partenza 0.5272
# molto significativo "pelvic_incidence"
# p-value dell'F-test 2.2e-16, c'è evidenza per dire che qualche covariate
# sia poco significativa

# ci sono NA in corrispondenza di "sacral_slope", cercando su Google
# scopriamo che è indice di lineare indipendenza tra le covariate, procediamo
# quindi subito con l'analisi di questo aspetto

# prevediamo "sacral_slope" in funzione di tutto il resto, tranne la nostra
# risposta originale e la categorica
g2 <- lm(sacral_slope~ .-class-lumbar_lordosis_angle, data = scoliosi )

# vediamo cosa otteniamo
summary( g2 )

# osserviamo che R^2_adj è esattamente 1, osservando i beta dei parametri
# scopriamo che 
# sacral_slope + pelvic_tilt = pelvic_incidence
# cercando su internet nei siti di ambito medico abbiamo conferma di questa cosa
# di conseguenza escludiamo questa covariata

# prima di farlo, vediamo la correlazione tramite dei grafici
# X è un sotto-dataset escludendo la risposta e la categorica
X = scoliosi [c(-3,-7)]
cor( X )

x11()
corrplot(cor(X), method='number')
# le correlazioni sono 1, 0.63, 0.81, a conferma di quanto detto

x11()
corrplot(cor(X), method='color')

x11()
heatmap( cor( X ), Rowv = NA, Colv = NA, symm = TRUE, keep.dendro = F)
#image( as.matrix( cor( X ) ), main = 'Correlation of X' )


# procediamo con il nostro modello escludendo la "sacral_slope" e la categorica
g <- lm( lumbar_lordosis_angle ~ .-class-sacral_slope, data = scoliosi )

summary( g )
# come ci aspettavamo  l'R^2_adj è invariato a 0.5272, così come p-value 2.2e-16


# Procediamo con la diagnostica
plot(g,which=1)
# omoschedasticità non fantastica

shapiro.test(g$residuals)
# normalita' schifosa, p-value 3.878e-11

# vediamo il qq pot dei residui
qqnorm( g$res, ylab = "Raw Residuals", pch = 16 )
qqline( g$res )

# dobbiamo fare trasformazione box cox per validare le nostre ipotesi di lavoro
# gia' che ci siamo controlliamo cooks-distance-studentized residuals-leverages

#SBAGLIATO
#boxcox sul primo, con tutto dentro
# b = boxcox(g)
# best_lambda = b$x[ which.max( b$y ) ]
# best_lambda
# 
# gb <- lm( (lumbar_lordosis_angle^best_lambda -1)/best_lambda ~ .-class, data=scoliosi )
# summary( gb )
# 
# plot(gb,which=1)#NO OMOSCHEDASTICIT?, noto nadamento parabolico?
# shapiro.test(gb$residuals) 
# 
# qqnorm( gb$res, ylab = "Raw Residuals", pch = 16 )
# qqline( gb$res )

#LEVERAGES

lev = hatvalues( g )  
lev

p = g$rank # p = 5 
n = dim(scoliosi)[1] # n = 310, estrae il numero di righe

watchout_points_lev = lev[ which( lev > 2 * p/n  ) ]  #seleziono i punti leva
watchout_points_lev  #20
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]  #posizioni dei punti leva

#RESIDUI STANDARDIZZATI

#standardizzo
gs = summary(g)
res_std = g$res/gs$sigma
#cerco quelli >2
watchout_ids_rstd = which( abs( res_std ) > 2 )
watchout_rstd = res_std[ watchout_ids_rstd ]
watchout_rstd   #16

# RESIDUI STUDENTIZZATI
stud = rstandard( g )

watchout_ids_stud = which( abs( stud ) > 2 )
watchout_stud = stud[ watchout_ids_stud ]
watchout_stud

#I PUNTI INFLUENTI TROVATI COI RESIDUI STUDENTIZZATI SONO GLI STESSI
#TROVATI COI RESIDUI STANDARDIZZATI

#plot solo degli studentizzati poiche sono quelli che togliamo
plot( g$fitted.values, stud, ylab = "Studentized Residuals",xlab = "Fitted Values", main = "Studentized Residuals", pch = 16 )
points( g$fitted.values[watchout_ids_stud], 
        stud[watchout_ids_stud], col = 'pink', pch = 16 )
abline( h = c(-2,2), lty = 2, col = 'orange' )
legend('topright', col = c('pink'), 
     c('Studentized Residual'), pch = rep( 16, 3 ), bty = 'n' )


#COOK DISTANCE, PER ORA INUTILE

#Cdist = cooks.distance( g ) #funzione per calcolare la formula di cook

#watchout_ids_Cdist = which( Cdist > 4/(n-p) ) 
#watchout_Cdist = Cdist[ watchout_ids_Cdist ]
#watchout_Cdist  #19

#graficone
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

# PUNTI INFLUENTI

x11()
influencePlot( g, id.method = "identify", main = "influential Plot",sub = "Circle size is proportial to Cook's Distance" )

watchout_influential_ids = row.names(influencePlot( g, main = "influential Plot"))#, id=list(method="identify")))
# "96"  "116" "198" sono influenti, 96 ? molto poco influente, quelli seriamente influenti sono sia nei 
# residui standardizzati che nei studentizzati che nei leverages

influence.measures( g )

#asterischi sui punti di influenza
#i punti di influenza vanno tolti!!! spostano troppo landamento del modello, anche se con essi ho un modello migliore
#il modello non ? rappresntativo di tutti i dati, vanno tolti

# Generiamo di nuovo il modello lineare dopo aver ripulito:
#-i residui studentizzati 
#-i leverages
#-entrambi

g_post_lev <- lm( lumbar_lordosis_angle ~ .-class-sacral_slope, scoliosi, subset = ( lev<2*p/n)  )
summary( g_post_lev )
AIC(g_post_lev)

g_post_rs <- lm( lumbar_lordosis_angle ~ .-class-sacral_slope, scoliosi, subset = ( abs(stud)<2 ) )
summary( g_post_rs )
AIC(g_post_rs)

g_post_both <- lm( lumbar_lordosis_angle ~ .-class-sacral_slope, scoliosi, subset = ( abs(stud)<2 | lev<2*p/n ))
summary( g_post_both )
AIC(g_post_both)

#notiamo che il modello migliore ? quello senza i punti influenti trovati coi residui studentizzati, 
#poiche ? quello con l r quadro maggiore e quello con l AIC minore  

# l'R^2_adj aumenta notevolmente a 0.7261
# p è 2.2e-16, ci sono ancora covariate non significative, stavolta diverse da prima
x11()
plot(g_post_rs,which=1)
# abbiamo sufficiente omoschedasticita'


shapiro.test(g_post_rs$residuals)
# tuttavia non abbiamo normalita' :(
# il p-value e' ancora troppo basso 0.04041

x11()
# vediamo come fittano i quantili
qqnorm( g_post_rs$res, ylab = "Raw Residuals", pch = 16 )
qqline( g_post_rs$res )


# Per ottenere la normalita' facciamo BOX COX
x11()
b = boxcox(g_post_rs)
best_lambdagl = b$x[ which.max( b$y ) ]
best_lambdagl
# lambda = 0.6262626

# generiamo il nuovo LM dove modelliamo una funzione della risposta
g_post_bc <- lm( (lumbar_lordosis_angle^best_lambdagl -1)/best_lambdagl ~ .-class-sacral_slope, data=scoliosi,subset = ( abs(stud) < 2 ) )
summary( g_post_bc )
# R^2_adj diminuisce da 0.7261 a 0.725, nessun problema
# ci sono covariate poco significative


# LE IPOTESI SONO VERIFICATE

plot(g_post_bc,which=1)
# omoschedasticita dei residui

shapiro.test(g_post_bc$residuals)
# p-value = 0.196 => non rifiutiamo la normalita'

qqnorm( g_post_bc$res, ylab = "Raw Residuals", pch = 16 )
qqline( g_post_bc$res )



# SELEZIONE COVARIATE

# rimuoviamo "pelvic_radius" che ha un p-value "one-at-a-time" di 0.0755,
# c'è evidenza per dire che non è significativo
g_without_pr <- lm( (lumbar_lordosis_angle^best_lambdagl -1)/best_lambdagl ~ .-class-sacral_slope-pelvic_radius, data=scoliosi,subset = ( abs(stud) < 2 ) )
summary( g_without_pr )
# R^2_adj scende da 0.725 a 0.723, ma semplifica di molto il modello, quindi ok

# riverifichiamo le ipotesi
plot(g_without_pr,which=1) #noto omoschedasticita dei residui
shapiro.test(g_without_pr$residuals) #ho normalita, p-value da 0.196 a 0.22, migliora la normalita'

qqnorm( g_without_pr$res, ylab = "Raw Residuals", pch = 16 )
qqline( g_without_pr$res )









#ANOVA

my_colors = brewer.pal( length( levels( scoliosi$class ) ), 'Set1')  #estraggo tanti colori

x11()
boxplot( scoliosi$lumbar_lordosis_angle ~ scoliosi$class , xlab = 'class', ylab = 'lordosis',
         main = 'lumbar lordosis angle according to class', col = my_colors )
abline( h = mean( scoliosi$lumbar_lordosis_angle ) )  # linea sulla media globale dell angolo bla bla

tapply( scoliosi$lumbar_lordosis_angle, scoliosi$class, length )#60-100-150
tapply( scoliosi$lumbar_lordosis_angle, scoliosi$class, mean )#35-43-64

Ps = tapply( scoliosi$lumbar_lordosis_angle,scoliosi$class , function( x ) ( shapiro.test( x )$p ) )
Ps
#non abbiamo ipotesi di normalita
Var = tapply( scoliosi$lumbar_lordosis_angle,scoliosi$class , var )
Var  #95-152-268
scoliosi$class=factor(scoliosi$class,ordered=F)  #li ordina

leveneTest(scoliosi$lumbar_lordosis_angle, scoliosi$class)
bartlett.test(scoliosi$lumbar_lordosis_angle, scoliosi$class)
#non ho omoschedasticita

#modello buono, ma non valido
reg=lm(scoliosi$lumbar_lordosis_angle~scoliosi$class,data=scoliosi)
summary(reg)

#uso box cox per avere la normalita
x11()
anB=boxcox(reg,lambda = seq(-3,3,by=0.01))
best_lambda=anB$x[which.max(anB$y)]
best_lambda

Ps2 = tapply( (scoliosi$lumbar_lordosis_angle^best_lambda -1)/best_lambda ,scoliosi$class , function( x ) ( shapiro.test( x )$p ) )
Ps2
#adesso ho normalita 
leveneTest( (scoliosi$lumbar_lordosis_angle^best_lambda -1)/best_lambda ,scoliosi$class )
#pvalue alto, ho omoschedasticita, H0 varianze intragruppi omoschedastiche
bartlett.test(  (scoliosi$lumbar_lordosis_angle^best_lambda -1)/best_lambda ,scoliosi$class )
#stesso risultato

#FUNZIONE TEO INTERNET
boxcoxfr(scoliosi$lumbar_lordosis_angle, scoliosi$class, option = "both", lambda = seq(-3, 3, 0.01), lambda2 = NULL, 
         tau = 0.05, alpha = 0.05, verbose = TRUE)

gA <- lm( (scoliosi$lumbar_lordosis_angle^best_lambda -1)/best_lambda ~ class, data=scoliosi )

summary(gA)  #meglio, tutti significativi, rquadro discreto
anova(gA)  #pvalue basso, rifiuto hp tutte le medie sono uguali



#5) int conf e prev
gp=lm(scoliosi$lumbar_lordosis_angle~scoliosi$degree_spondylolisthesis,data=scoliosi)
summary(gp)
res2=gp$residuals

gp2=lm(scoliosi$lumbar_lordosis_angle~scoliosi$degree_spondylolisthesis,data=scoliosi,subset=(abs(res2)<35))
summary(gp2)

plot(gp2,which=1) #noto omoschedasticita dei residui
shapiro.test(gp2$residuals) #non ho normalita uso box cox

qqnorm( gp2$res, ylab = "Raw Residuals", pch = 16 )
qqline( gp2$res )

x11()
anC=boxcox(gp,lambda = seq(-3,3,by=0.01))
best_lambda2=anC$x[which.max(anB$y)]
best_lambda2

g2_post_bc <- lm( lumbar_lordosis_angle ~ log(scoliosi$pelvic_incidence) , data=scoliosi) 
summary( g2_post_bc )

plot(g2_post_bc,which=1) #noto omoschedasticita dei residui
shapiro.test(g2_post_bc$residuals) #ho normalita, p-value da 0.196 a 0.22, migliora la normalita'

qqnorm( g2_post_bc$res, ylab = "Raw Residuals", pch = 16 )
qqline( g2_post_bc$res )

##MARC

# carico il dataset
scoliosi2 = read.csv(here::here("data","column_3C_weka.csv"), header = TRUE)

temp_var <- predict(gp2, interval="confidence") 

new_df <- cbind(scoliosi2, temp_var) 

ggplot(new_df, aes(scoliosi2$degree_spondylolisthesis, scoliosi2$lumbar_lordosis_angle))+ 
  geom_point() + 
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+ 
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+ 
  geom_smooth(method=lm, se=TRUE)

temp_var2 <- predict(gp2, interval="prediction") 

new_df2 <- cbind(scoliosi2, temp_var) 

ggplot(new_df2, aes(scoliosi2$degree_spondylolisthesis, scoliosi2$lumbar_lordosis_angle))+ 
  geom_point() + 
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+ 
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+ 
  geom_smooth(method=lm, se=TRUE,xlab="grado di spondilolistesi",ylab="angolo di lordosi lombare")

##

x11()
plot(scoliosi$pelvic_incidence,scoliosi$lumbar_lordosis_angle)
abline(a=gp$coefficients[1],b=gp$coefficients[2])

grid = seq( min(scoliosi$pelvic_incidence), max(scoliosi$pelvic_incidence), (max(scoliosi$pelvic_incidence)- min(scoliosi$pelvic_incidence))/309 )

# automatic prediction
ypred = predict( gp, data.frame( grid ), interval = "confidence", se = T )
ypred

names( ypred )

ypred$fit
ypred$se

tc    = qt( 0.975, length( grid ) - 2 )
ypred.sup = ypred$fit + tc * ypred$se
ypred.inf = ypred$fit - tc * ypred$se

IC = cbind( ypred$fit, ypred.inf, ypred.sup )

##Plot the CI of predictions.
plot.new()
matplot( grid, cbind( ypred$fit, ypred.inf, ypred.sup ), lty = c( 1, 2, 2 ), 
         col = c( 1, 'blue', 'blue' ), type = "l", xlab = "pelvic_incidence",
         ylab = "lumbar_lordosis_angle", main = 'IC per la media della risposta' )
points( scoliosi$pelvic_incidence, scoliosi$lumbar_lordosis_angle, col = "black", pch = 16 )

#__6.c__  Compute the Prediction Interval for the one new observation. In this case the standard errors are:

y.pred2 = predict( gp, data.frame( grid ), interval = "prediction", se = T )
# fornisce direttamente gli estremi inf e sup, che prima abbiamo costruito a mano (in un altro caso)

y.pred2$fit[ ,1 ] # predicted values \hat{y}_{new}.
y.pred2$fit[ ,2 ] # LB prediction interval for y_{new}.
y.pred2$fit[ ,3 ] # UB prediction interval for y_{new}.



x11()
matplot( grid, y.pred2$fit, lty = c( 1, 2, 2 ), col = c( 1, 2, 2 ), type = "l",
         xlab = "pelvic_incidence", ylab = "lumbar_lordosis_angle", main = 'IP per singole osservazioni' )
points( scoliosi$pelvic_incidence, scoliosi$lumbar_lordosis_angle, col = "blue", pch = 16 )


##__6.d__ Compare the Intervals obtained at __6.b__ and __6.c__.
x11()
matplot( grid, y.pred2$fit, lty = c( 1, 2, 2 ), col = c( 1, 2, 2 ), type = "l", xlab = "pelvic_incidence", ylab = "lumbar_lordosis_angle", 
         main = "IC per la media e IP per singole osservazioni" )

lines( grid, y.pred$fit[ , 2 ] , col = "blue", lty = 2, xlab = "pelvic_incidence", ylab = "lumbar_lordosis_angle" )
lines( grid, y.pred$fit[ , 3 ] , col = "blue", lty = 2, xlab = "pelvic_incidence", ylab = "lumbar_lordosis_angle" )
points( scoliosi$pelvic_incidence, scoliosi$lumbar_lordosis_angle, col = "black", pch = 16 )

