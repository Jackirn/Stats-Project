
# Metodi e Modelli per l'Inferenza Statistica - Ing. Matematica - a.a. 2023-24

#####################################
### PREDITTORE CATEGORICO e ANOVA ###
#####################################

rm(list=ls())
## 0. Librerie

library( MASS )
library( car ) #for LEVENE TEST
library( faraway )
library(GGally)
library( Matrix )
library(rgl)

## 1. Modello lineare con con predittore categorico

#Importare il dataset Terremoti. Impostare un modello lineare per prevedere la profondit? del terremoto in funzione di 
#latitudine, longitudine e zona sismica. Il modello e' buono? Il modello e' valido?

# 2.1 Importiamo e visualizziamo i dati 

terr = read.csv("Terremoti.csv")

# cosa contiene in dataset?
str(terr)

## visualizziamo i dati numerici
ggpairs(terr[,c('depth','lat','long')])

## coloriamo per zona
ggpairs(terr[,c('depth','lat','long')], aes(col=as.factor(terr$zone)))

## Visualizziamo i dati in 3d: 
open3d()
plot3d(x=terr$lat, y=terr$long, z= terr$depth, size=4, aspect = T)

## La divisione per zona geografica sembra molto significativa. 

# Coloriamo per zona
# N.B. col = data$zone + intero --> variando l'intero cambio i colori (e' solo un parametro grafico)
plot3d(x=terr$lat, y=terr$long, z= terr$depth, col = terr$zone +2, size=4, aspect = T)


## Costruiamo un modello lineare che contenga come covariate solo latitudine e longitudine, 
## ignorando per il momento la variabile categorica zona. 
first = lm(depth ~ lat + long, data = terr)
summary(first)

## diagnostica dei residui

plot(first$fitted.values, first$residuals) 
abline(h=0, col='red')

qqnorm( first$residuals, ylab = "Residuals", pch = 16 )
abline( 0, 1 )

shapiro.test(first$residuals)


## Modello pessimo: R2 bassissimo, pessima distribuzione e comportamento dei residui. 

## Proviamo a visualizzare il piano di regressione identificato:
open3d()
plot3d(x=terr$lat, y=terr$long, z= terr$depth, col = terr$zone , size=4, aspect = T)
beta = first$coefficients
planes3d(a = c(-beta[2],- beta[3],1), d = -beta[1],col = "green",alpha = 0.4)
points3d(terr$lat,terr$long,first$fitted.values, size = 2, col = "blue") 

## Possiamo fare decisamente di meglio, inserendo l'informazione 'zone'.

## Idea: zone diverse corrispondono a modelli lineari diversi, con parametri diversi ma identica 
## sigma^2. Come modellare le variabili categoriche nel modello?
## Matematicamente possiamo scrivere, se le variabili si dividono in K gruppi:
## y_ik = beta_0k + beta_1k * x1_ik + beta_2k * x2_ik + eps_ig, con Var(eps_ik) = sigma^2, 
## ogni i, k = 1,...K. Come modellare tutto in un unico modello?
## Prendiamo il primo gruppo come riferimento, cosi' che per il primo gruppo valga il modello
## y_i1 = beta_01 + beta_11 * x1_i1 + beta_21 * x2_i1 + eps_i1. 
## Poi per un generico gruppo k diverso dal primo, definiamo la variabile dummy d_ik = 1 
## se la i-esima osservazione e' nel gruppo k, d_ik = 0 altrimenti.
## Scriviamo quindi il modello generale per un'osservazione i qualsiasi:
## y_i = beta_01 + beta_11 * x1_i + beta_21 * x2_i + beta_02 * d_i2 + beta_12 * x1_i  * d_i2   
## + beta_21 * x2_i * d_i2 + .... + beta_0K * d_iK +
## beta_1K * x1_i  * d_iK  + beta_2K * x2_i * d_iK
## Cosi', in un unico modello, otteniamo in realta' K modelli lineari diversi: 
## Per il primo gruppo, il modello e' proprio 
## y_i1 = beta_01 + beta_11 * x1_i1 + beta_21 * x2_i1 + eps_i1
## Per ogni altro gruppo avremo
## y_ik = (beta_01 + beta_0k)  + (beta_11 + beta_1k) * x1_i1 + (beta_21 + beta_2k) * x2_i1 + eps_ik


## Nota: se ci sono K gruppi, abbiamo bisogno di K-1 variabili dummy!


## In questo caso abbiamo due gruppi, quindi abbiamo bisogno di un nuovo vettore che abbia 0 in corrispondenza  
## del primo gruppo e 1 in corrispondenza del secondo (o viceversa, ? indifferente)

dummy = ifelse(terr$zone == 1, 0,1)

table(terr$zone)
table(dummy)

terr$dummy = dummy
table(terr$dummy)
terr$dummy = as.factor(terr$dummy)


second = lm(depth ~ lat + long + dummy , data = terr)
summary(second) 

## Miglioramento notevole!


plot(second$fitted.values, second$residuals) 
abline(h=0, col='red')

qqnorm( second$residuals, ylab = "Residuals", pch = 16 )
abline( 0, 1 )

shapiro.test(second$residuals)
## Nota: in realtà R può gestire tutto autonomamente, senza che noi dobbiamo definire variabili dummy:

terr$zone = factor(terr$zone, ordered = F) # Importante mettere factor e ordered = F
second2 = lm(depth ~ lat + long + zone, data = terr) 
summary(second2)
## Il modello e' esattamente identico

# i due piani sono paralleli ovviamente, per costruzione!

## dal grafico sembrerebbe ragionevole inserire anche l'interazione tra zone e le altre covariate numeriche

third = lm(depth ~ lat + long + dummy + dummy * lat + dummy * long, data = terr)
summary(third) ## Miglioramento notevole!

plot(third$fitted.values, third$residuals) 
abline(h=0, col='red')

qqnorm( third$residuals, ylab = "Residuals", pch = 16 )
abline( 0, 1 )

shapiro.test(third$residuals)

open3d()
plot3d(x=terr$lat, y=terr$long, z= terr$depth, col = as.numeric(terr$zone) +1, size=4, aspect = T)
box3d()
axes3d()
beta = third$coefficients
planes3d(a = c(-beta[2],- beta[3],1), d = -beta[1],col = "red",alpha = 0.4)
planes3d(a = c(-beta[2] - beta[5],- beta[3] - beta[6],1), d = -beta[1] - beta[4],col = "green",alpha = 0.4)

points3d(terr$lat,terr$long,third$fitted.values, size = 2, col = "blue") 

# Dal summary, si direbbe che la zona abbia scarsa interazione con la latitudine, proviamo a ridurre il modello:

fourth = lm(depth ~ lat + long + zone + zone*long, data = terr)
summary(fourth)

plot(fourth$fitted.values, fourth$residuals) 
abline(h=0, col='red')

qqnorm( fourth$residuals, ylab = "Residuals", pch = 16 )
abline( 0, 1 )

shapiro.test(fourth$residuals) # ancora i residui non sono normali, infatti anche ad occhio le code si allontanano dalla retta!

##..idee?  ## poly(long,3), boxcox, punti influenti?


## Trasformazione boxcox
summary(terr$depth)  # è sempre negativa!

## rivediamo il modello
fourth = lm(depth ~ lat + long + zone + zone*long, data = terr)
summary(fourth)

fifth = lm((-depth) ~ lat + long + zone + zone*long, data = terr)
summary(fifth)


b = boxcox((-depth) ~ lat + long + zone + zone*long, data = terr)
names(b)
#y likelihood evaluation
#x lambda evaluated
best_lambda_ind = which.max( b$y )
best_lambda = b$x[ best_lambda_ind ]
best_lambda



sixth = lm((((-depth) ^ best_lambda - 1)/best_lambda) ~ lat + long + zone + zone*long, data = terr)
summary(sixth)

plot(sixth$fitted.values, sixth$residuals) 
abline(h=0, col='red')

qqnorm( sixth$residuals, ylab = "Residuals", pch = 16 )
abline( 0, 1 )

shapiro.test(sixth$residuals) # ancora i residui non sono normali!!!!

rm(list=ls())

## 2. Visualizzazione della decomposizione della varianza


#Lavoreremo con il dataset presente nel documento ciclisti.txt. Sono state considerate dieci strade 
#con pista ciclabile ed è stata misurata la distanza tra la linea di mezzeria 
#(linea longitudinale che scorre in mezzo alla strada, dividendo la carreggiata in diverse corsie) 
#e un ciclista sulla pista ciclabile. In queste stesse dieci strade ? stata determinata attraverso 
#fotografie la distanza tra lo stesso ciclista e una macchina passante per la strada considerata.

# Center -> distanza tra la linea di mezzeria e un ciclista sulla pista ciclabile [misuarata in piedi] 
# Car  -> distanza tra lo stesso ciclista e una macchina passante per la strada considerata [misuarata in piedi] 

## 1. Visualizzazione della decomposizione della varianza

   
# Importiamo il dataset CICLISTI:
ciclisti = read.table( "ciclisti.txt", header = TRUE )
      
head(ciclisti)
      
names( ciclisti )
      
dim( ciclisti )
      
n = dim( ciclisti )[1]

      
# Fittiamo ora un modello lineare semplice e calcoliamo le quantità di interesse.
      
reg.ciclisti = lm( Car ~ Center, data = ciclisti )
summary( reg.ciclisti )
      
# y_i ( dati osservati )
y.i = ciclisti$Car
y.i
      
# y medio
y.mean = mean( ciclisti$Car )
y.mean 

# y.hat ( dati fittati dal modello )
y.hat = reg.ciclisti$fitted.value
y.hat

      
# Facciamo il grafico delle quantit? di interesse.

par( mfrow = c( 1, 3 ) )

# SStot = Sum( y_i - y_medio )^2
plot( NULL, xlim = c( 12, 22 ), ylim = c( 5, 12 ),
      xlab = "Center", ylab = "Car", main = "Contributo di y_i a SStot" )
points( ciclisti$Center, ciclisti$Car,
        pch = 16, col = 'blue', cex = 1.2 )

for ( i in 1:n )
  segments( ciclisti$Center [ i ], ciclisti$Car[ i ], ciclisti$Center [ i ], y.mean,
            lwd = 2, col = 'red', lty = 1 )

abline( h = y.mean , col = 'darkblue', lwd = 1.2 )

abline( reg.ciclisti, lwd = 2, col = 'black', lty = 2 )

# SSreg = Sum( y.hat_i - y_medio )^2
plot( NULL, xlim = c( 12, 22 ), ylim = c( 5, 12 ),
      xlab = "Center", ylab = "Car", main = "Contributo di y.hat_i a SSreg" )
points( ciclisti$Center, ciclisti$Car,
        pch = 16, col = 'blue', cex = 1.2 )

for ( i in 1:n )
  segments( ciclisti$Center [ i ], y.hat[ i ], ciclisti$Center [ i ], y.mean,
            lwd = 2, col = 'red', lty = 1 )

abline( h = y.mean , col = 'darkblue', lwd = 1.2 )

abline( reg.ciclisti, lwd = 2, col = 'black', lty = 2 )


# SSerr = Sum( y_i - y.hat_i )^2
plot( NULL, xlim = c( 12, 22 ), ylim = c( 5, 12 ),
      xlab = "Center", ylab = "Car", main = "Contributo di y_i a SSerr" )
points( ciclisti$Center, ciclisti$Car, pch = 16, col = 'blue', cex = 1.2 )

for ( i in 1:n )
  segments( ciclisti$Center [ i ], ciclisti$Car[ i ], ciclisti$Center [ i ], y.hat [ i ],
            lwd = 2, col = 'red', lty = 1 )

abline( h = y.mean , col = 'darkblue', lwd = 1.2 )

abline( reg.ciclisti, lwd = 2, col = 'black', lty = 2 )

par( mfrow = c( 1, 1 ) )
rm(list=ls())

## 3. One-way ANOVA (I) 

# Importiamo i dati `chickwts`. Vogliamo investigare se il peso dei polli ? influenzato dal tipo di 
# alimentazione (y = weights e tau = feed).

# __Soluzione__

#Importiamo i dati.

data( chickwts )
head( chickwts )
tail( chickwts )

attach( chickwts )


#Iniziamo a farci un'idea descrittiva dei dati per avere indicazioni qualitative sulla presenza di 
#differenziazione nella risposta a causa dell'appartenenza ad una o all'altra categoria.

#L'analisi della varianza, nota con l'acronimo di ANOVA, è una tecnica statistica che ha come obiettivo il 
#confronto tra le medie di un fenomeno aleatorio fra differenti gruppi di unit? statistiche. 
#Tale analisi viene affrontata tramite decomposizione della varianza.

#Consiglio grafico: spesso ? utile rappresentare i gruppi di dati con colori diversi, potete trovare alcune 
#palette di colori nel pacchetto RColorBrewer.

library(RColorBrewer)

# display.brewer.all() # mostra le palette disponibili in RColorBrewer

my_colors = brewer.pal( length( levels( chickwts$feed ) ), 'Set2')

summary( chickwts )


boxplot( weight ~ feed, xlab = 'feed', ylab = 'weight',
main = 'Chicken weight according to feed',
col = my_colors )
abline( h = mean( weight ) )

# numerosit? dei gruppi
tapply( chickwts$weight, chickwts$feed, length )
tapply( chickwts$weight, chickwts$feed, mean )

#Sembra che un qualche effetto ci sia, le medie appaiono diverse e sembra vi sia una dominanza stocastica 
#delle distribuzioni dei pesi.

#Scriviamo il modello che vogliamo fittare:

#y_ij = mu_j + varepsilon_ij= mu + tau_j + varepsilon_ij

#Siamo interessati ad eseguire il seguente test:

#H_0: mu_i = mu_j per ogni i,j = {1,..,6} vs  H_1: ne esiste almeno uno diverso 

# Facciamo un'ANOVA manuale. 

# Verifichiamo che siano soddisfatte le ipotesi dell'ANOVA:

# Normalità intragruppo;

n  = length( feed )
ng  = table( feed )
treat  = levels( feed )
g  = length( treat )

# Normalità dei dati nei gruppi
Ps = c( shapiro.test( weight [ feed == treat [ 1 ] ] )$p,
shapiro.test( weight [ feed == treat [ 2 ] ] )$p,
shapiro.test( weight [ feed == treat [ 3 ] ] )$p,
shapiro.test( weight [ feed == treat [ 4 ] ] )$p,
shapiro.test( weight [ feed == treat [ 5 ] ] )$p,
shapiro.test( weight [ feed == treat [ 6 ] ] )$p )
Ps

# In maniera più compatta ed elegante:
Ps = tapply( weight, feed, function( x ) ( shapiro.test( x )$p ) )

Ps # tutti p-value alti = > non rifiuto mai hp di Normalit?

# Omoschedasticità fra i gruppi.

# Varianze dei gruppi omogenee
Var = c( var( weight [ feed == treat [ 1 ] ] ),
var( weight [ feed == treat [ 2 ] ] ),
var( weight [ feed == treat [ 3 ] ] ),
var( weight [ feed == treat [ 4 ] ] ),
var( weight [ feed == treat [ 5 ] ] ),
var( weight [ feed == treat [ 6 ] ] ) )
Var

# In maniera più compatta ed elegante:
Var = tapply( weight, feed, var )
Var

# Per verificare l'omogeneità tra le varianze abbiamo diverse possibilità. 

# Bartlett's test

# H_0: sigma_1 = sigma_2 = ... = sigma_g  vs H_1: H_0^C

# Il test di Bartlett assume che le osservazioni appartenenti ai vari gruppi siano iid da una Normale. 

# Test di uniformità delle varianze
bartlett.test( weight, feed )

#Il test di Bartlett in questo caso accetta H_0.

# Levene's test
# Anche il test di Levene serve per valutare l'omogeneità delle varianze di una variabile calcolato per due o 
# più gruppi. Questo test è un'alternativa a quello di Bartlett, meno sensibile alla non normalità dei dati.

leveneTest( weight, feed )

# Anche il test di Levene è concorde nell'accettare l'ipotesi nulla.

# Ora che abbiamo verificato che le ipotesi sono soddisfatte possiamo procedere con una One-Way ANOVA.

# Prima, però, osserviamo come possiamo decomporre la varianza tenendo conto di questi gruppi --> lavagna

Media  = mean( weight )
Media.1 = mean( weight [ feed == treat [ 1 ] ] )
Media.2 = mean( weight [ feed == treat [ 2 ] ] )
Media.3 = mean( weight [ feed == treat [ 3 ] ] )
Media.4 = mean( weight [ feed == treat [ 4 ] ] )
Media.5 = mean( weight [ feed == treat [ 5 ] ] )
Media.6 = mean( weight [ feed == treat [ 6 ] ] )
Mediag  = c( Media.1, Media.2, Media.3, Media.4, Media.5, Media.6 )
Mediag

# oppure (FORTEMENTE CONSIGLIATO):
Media  = mean( weight )
Mediag  = tapply( weight, feed, mean )
Mediag

SStot  = var( weight ) * ( n-1 )
SSB = sum( ng * ( Mediag-Media )^2 )
SSW  = SStot - SSB

alpha = 0.05
Fstatistic = ( SSB / ( g-1 ) ) / ( SSW / ( n-g ) )

# valori "piccoli" non ci portano a rifiutare
cfr.fisher = qf( 1-alpha, g-1, n-g )
Fstatistic > cfr.fisher
Fstatistic 
cfr.fisher

#F_0 siamo proprio ben oltre la soglia trovata con la distribuzione F, quindi abbiamo un'evidenza forte per rifiutare H0.

# Calcoliamo anche il p-value (area a dx di Fstatistic).
P = 1-pf( Fstatistic, g-1, n-g )
P

# Costruiamo l'ANOVA in modo automatico. 
fit = aov( weight ~ feed )
summary( fit )

fit$coefficients # restituisce la media del gruppo di riferimento più i (g-1) tau
Mediag
## cosa vediamo nel summary?

# Oppure:
mod = lm( weight ~ feed )
summary( mod )

# Meglio fare:
anova( mod )

# Affermiamo quindi che c'è differenza delle medie fra i gruppi.
detach(chickwts)


## 4. Costruzione della matrice disegno di ANOVA

# Poniamoci nel caso di ONE-WAY ANOVA. Per verificare l'esistenza di diversi gruppi, di fatto quello 
# che vogliamo fare ? un modello di regressione lineare con una variabile *factor*  (__variabile dummy__, categorica).

# y = X  %*% beta + epsilon

# Cerchiamo di capire come mai il numero dei regressori sar? g-1, quindi X avr? dimensioni n x g 
# (perch? viene aggiunta l'intercetta).

# Supponiamo di avere un campione di 18 osservazioni suddivisi in 7 gruppi con numerosit? {3,2,3,2,3,2,3} 
# rispettivamente, quale dovrebbe essere la matrice disegno X?  

# Se facciamo `tapply(feed, feed, length)`, R calcola le numerosit? dei gruppi e le riordina in ordine
# alfabetico di nome del gruppo. Se vogliamo utilizzare le numerosit? nell'ordine in cui si presentano 
# i gruppi nel dataset (feed), dobbiamo fare:
data( chickwts )
head( chickwts )
tail( chickwts )

attach( chickwts )
n  = length( feed )

group_names = unique( as.character( feed ) )
ng = tapply( feed, feed, length )[ group_names ]

ng

# Costruiamo la matrice X.full, ovvero una matrice disegno dove consideriamo tutti i gruppi 
# (dimensione = n x (g + 1) ).

# gruppo 1 (nell'ordine dei dati in ( weight,feed )
x1.full = c( rep( 1, ng [ 1 ] ),
             rep( 0, n - ng [ 1 ] ) )

# gruppo 2 (nell'ordine dei dati in ( weight,feed )
x2.full = c( rep( 0, ng [ 1 ] ),
             rep( 1, ng [ 2 ] ),
             rep( 0, n - ng [ 1 ] - ng [ 2 ] ) )

# gruppo 3 (nell'ordine dei dati in ( weight,feed )
x3.full = c( rep( 0, ng [ 1 ] + ng [ 2 ] ),
             rep( 1, ng [ 3 ] ),
             rep( 0, n - ng [ 1 ] - ng [ 2 ] - ng [ 3 ] ) )

# gruppo 4 (nell'ordine dei dati in ( weight,feed )
x4.full = c( rep( 0, n - ng [ 6 ] - ng [ 5 ] - ng [ 4 ] ),
             rep( 1, ng [ 4 ] ),
             rep( 0, ng [ 5 ]  + ng [ 6 ] ) )

# gruppo 5 (nell'ordine dei dati in ( weight,feed )
x5.full = c( rep( 0, n - ng [ 6 ] - ng [ 5 ] ),
             rep( 1, ng [ 5 ] ),
             rep( 0, ng [ 6 ] ) )

# gruppo 6 (nell'ordine dei dati in ( weight,feed )
x6.full = c( rep( 0, n - ng [ 6 ] ),
             rep( 1, ng [ 6 ] ) )

X.full = cbind( rep( 1, n ),
                x1.full,
                x2.full,
                x3.full,
                x4.full,
                x5.full,
                x6.full )

# Visualizziamo questa matrice disegno.
image(Matrix(X.full))

# Vediamo che non ha rango pieno (proviamo che una colonna ? combinazione lineare di un'altra).
X.full[ , 1 ] - rowSums( X.full[ , - 1 ] )

# come si stimano i beta in questo caso? X.full ? ivertibilie?
# Per invertirla manualmente dobbiamo ricorrere alla pseudoinversa di Moore-Penrose.

H.full = X.full %*% ginv( t( X.full ) %*% X.full ) %*% t( X.full )

# H.full = X.full %*% solve( t( X.full ) %*% X.full ) %*% t( X.full )
# R d? errore, perch? singolare!

y  = weight

betas.full = as.numeric( ginv( t( X.full ) %*% X.full ) %*% t( X.full ) %*% y )
betas.full
# `ginv` calcola la matrice pseudo-inversa di Moore-Penrose di una matrice.

# La media nel gruppo j-esimo ?: E[y_j] = mu_j = beta_0 + beta_j per j=1:g

means_by_group = betas.full[ 1 ] + betas.full[ 2:length( betas.full ) ] 
names( means_by_group ) = group_names

means_by_group
tapply( weight, feed, mean )[ unique( as.character( feed ) ) ]

# La media globale ?: mu = sum_{j=1}^g (n_j * mu_j)/n

global_mean = ng %*% means_by_group / n
global_mean

mean( weight )  ## tutto torna

# In alternativa, possiamo scrivere la matrice disegno sotto forma di contrasto, 
# ovvero rimuoviamo una colonna (un parametro), denotando gli elementi dell'ultimo gruppo come assenti
# in tutti gli altri nel seguente modo:

x1.red = c( rep( 1, ng [ 1 ] ),
            rep( 0, n - ng [ 1 ] - ng [ 6 ] ),
            rep( -1, ng [ 6 ] ) )
stopifnot( sum( x1.red - ( x1.full - x6.full ) ) == 0 )

x2.red = c( rep( 0, ng [ 1 ] ),
            rep( 1, ng [ 2 ] ),
            rep( 0, n - ng [ 1 ] - ng [ 2 ] - ng [ 6 ] ),
            rep( -1, ng [ 6 ] ) )
stopifnot( sum( x2.red - ( x2.full - x6.full ) ) == 0 )

x3.red = c( rep( 0, ng [ 1 ] + ng [ 2 ] ),
            rep( 1, ng [ 3 ] ),
            rep( 0, n - ng [ 1 ] - ng [ 2 ] - ng [ 3 ] - ng [ 6 ] ),
            rep( -1, ng [ 6 ] ) )
stopifnot( sum( x3.red - ( x3.full - x6.full ) ) == 0 )

x4.red = c( rep( 0, n - ng [ 6 ] - ng [ 5 ] - ng [ 4 ] ),
            rep( 1, ng [ 4 ] ),
            rep( 0, ng [ 5 ] ),
            rep( -1, ng [ 6 ] ) )
stopifnot( sum( x4.red - ( x4.full - x6.full ) ) == 0 )

x5.red = c( rep( 0, n - ng [ 6 ] - ng [ 5 ] ),
            rep( 1, ng [ 5 ] ),
            rep( -1, ng [ 6 ] ) )
stopifnot( sum( x5.red - ( x5.full - x6.full ) ) == 0 )


X.red  = cbind( rep( 1, n ),
                x1.red,
                x2.red,
                x3.red,
                x4.red,
                x5.red)


# Visualizziamo questa matrice di dimensione n x g.

image(Matrix(X.red))

# Stimiamo ora beta.
H.red  = X.red %*% solve( t( X.red ) %*% X.red ) %*% t( X.red )

betas.red = as.numeric( solve( t( X.red ) %*% X.red ) %*% t( X.red ) %*% y )
betas.red

# La media nel gruppo i-esimo si ottiene nel seguente modo: mu_j = beta_0 + beta_j per i = 1, ..., g-1
#                                                           mu_g = beta_0 - ( beta_1 + ... + beta_{g-1} )

means_by_group = betas.red[ 1 ] + betas.red[ -1 ]
means_by_group = c( means_by_group, betas.red[ 1 ] - sum( betas.red[ -1 ] ) )

means_by_group.full = betas.full[ 1 ] + betas.full[ -1 ]
means_by_group.full = c( means_by_group.full, betas.full[ 1 ] - sum( betas.full[ -1 ] ) )
means_by_group.full

# confrontiamoli tutti
names( means_by_group ) = group_names
means_by_group

tapply( weight, feed, mean )[ group_names ]

names( means_by_group.full ) = group_names
means_by_group.full

# In questo caso abbiamo in tutti i casi dei risultati molto coerenti (nonostante le approssimazioni).

## Ma cosa fa `R` in automatico?

mod_aov = aov( weight ~ feed )
X_aov = model.matrix( mod_aov )

# Visualizziamo la matrice disegno dell'ANOVA implementata in `R`.
image( Matrix( X_aov ) , main='Matrice disegno anova') 


# Vediamo che la matrice disegno creata dall'ANOVA ? di dimensioni n x g. Notiamo che manca la variabile (livello)
# `casein`(la prima in ordine alfabetico) che ? usata come baseline.

mod_lm = lm( weight ~ feed )
X_lm = model.matrix( mod_lm )

# Idem nel caso di modello lineare.
image( Matrix( X_lm ) , main='Matrice disegno lm') 



# Il primo gruppo (nell'ordine *alfanumerico* dei livelli della variabile di stratificazione, feed, 
# e NON nell'ordine di comparsa dei dati) viene soppresso e preso come riferimento (baseline).

# Calcoliamo ora i beta e interpretiamoli.
summary(mod_lm)

betas.lm = coefficients( mod_lm )
betas.lm

# La media nel gruppo j-esimo si ottiene nel seguente modo:  mu_{baseline} = beta_0
#                                                            mu_j = beta_0 + beta_j, per j \neq baseline

means_by_group = c( betas.lm[ 1 ], betas.lm[ 1 ] + betas.lm[ -1 ] )
names( means_by_group ) = levels( feed )

means_by_group
tapply( weight, feed, mean )

detach(chickwts)

## 5. One-way ANOVA (II)

# The example dataset we will use is a set of 24 blood coagulation times.
# 24 animals were randomly assigned to four different diets and the samples were taken in a random order. 
# This data comes from Box, Hunter, and Hunter (1978).

coagulation= read.table(file='coagulation.txt', header=T)

str( coagulation )
dim( coagulation )
names( coagulation )
head( coagulation )

coagulation$diet = as.factor(coagulation$diet)
str( coagulation )

# The first step is to plot the data to check for:

# __1.__ Normality assumption;

# __2.__ Equal variances for each level of the factor.

boxplot( coag ~ diet, data = coagulation )

# In this case, there are no obvious problems. For group C, there are only 4 distinct observations and 
# one is somewhat separated which accounts for the slightly odd looking plot. Always look at sample sizes.

table( coagulation$diet )

# let's check assumptions.
Ps = tapply(coagulation$coag, coagulation$diet, function( x ) ( shapiro.test( x )$p))
Ps 

# Alternative: Levene-Test
leveneTest( coagulation$coag, coagulation$diet )

# We accept normality and homoscedasticity. Now let's fit the model.

mod = lm( coag ~ diet, coagulation )
summary( mod )

# What kind of design matrix has been used in this case? Look at the design matrix to understand the coding:

model.matrix( mod ) #n x g
image(Matrix(model.matrix( mod )))
# The effects returned by ANOVA have to be interpreted as *differences* from a reference level, 
# the baseline (the first in alphabetical order).

# What do we conclude by looking at the p-value?

# For completeness, look at:
anova(mod)

# We have evidence of the fact that different groups have significantly different means.

# Diagnostics

par( mfrow = c(1,2) )

qqnorm( mod$res, pch = 16, col = 'black', main = 'QQ-norm dei residui' )
qqline( mod$res, lwd = 2, col = 'red', lty = 2 )

shapiro.test( mod$res )

plot( mod$fit, mod$res, xlab = "Fitted", ylab = "Residuals", main = "Residual-Fitted plot",
      pch = 16 )

# Since data are integers and fitted values are integers too, a discrete-like pattern must be expected in the QQ plot. 
# Of course, discrete data cannot be normally distributed. However, here residuals are approximately normal and so 
# we can go ahead with the inference.





## 6. Two-ways ANOVA

# Two-ways ANOVA design is thought for variance decomposition in cases where we have two factors, 
# and not only one as before. How is the model? --> lavagna

# As part of an investigation of toxic agents, 48 rats were allocated to

# + 3 poisons ( I, II, III ) and
# + 4 treatments ( A, B, C, D ).

# The response was survival time in tens of hours.

rats =read.table(file='rats.txt', header=T)

str( rats )
rats$poison= as.factor(rats$poison)
rats$treat= as.factor(rats$treat)

head( rats )
tail( rats )
names( rats )

# Some automatic plots.
plot( time ~ treat + poison, data = rats )

par( mfrow = c( 1,2 ) )
boxplot( time ~ treat, data = rats )
boxplot( time ~ poison, data = rats )

pairs(rats) #poco interpretabile

#Some evidence of skewness can be seen, especially since it appears that variance is in some way related to 
#the mean response.

#Check for an interaction using graphical methods:
x11()
interaction.plot( rats$treat, rats$poison, rats$time )
interaction.plot( rats$poison, rats$treat, rats$time )

# Parallel lines would suggest the absence of interaction, yet it is not always easy to figure it out with these plots.

# Before applying a two-way ANOVA, we have to test the following hypotheses:
#NORMALITY (in all groups, 3 x 4 = 12 tests);
#HOMOGENEOUS VARIANCES (among groups).

tapply( rats$time, rats$treat:rats$poison, function( x ) shapiro.test( x )$p )

leveneTest( rats$time, rats$treat:rats$poison )
bartlett.test( rats$time, rats$treat:rats$poison )

# We notice that normality is respected in all 12 groups ( even thogh we observe low p-value for the first group A-I ). 
# The variances homogeneity is violated (see p-value of Levene's test).

# It could be possible to consider variable transformation. 
# We should try a Box-Cox transformation for the output variable, considering the complete model. 

# We fit the complete model considering interactions between considered factors. How is the model?

g = lm( time ~ poison * treat, rats )
#"*" gives the full model: linear effect AND interaction
#g = lm( time ~ poison + treat + poison : treat , rats )

summary( g )
anova( g )

shapiro.test(g$residuals)

b = boxcox( g, lambda = seq(-3,3,by=0.01) )
best_lambda = b$x[ which.max( b$y ) ]
best_lambda

# The best lambda is -0.82, which we approximate to -1 (we can interpret the reciprocal as the death rate).

tapply( 1/rats$time, rats$treat:rats$poison, function( x ) shapiro.test( x )$p )

leveneTest( 1/rats$time, rats$treat:rats$poison )
bartlett.test(  1/rats$time, rats$treat:rats$poison )

# After Box-Cox transformation (without scale-location adjustment), assumptions are respected!

g1 = lm( 1/time ~ poison * treat, rats )

summary(g1)
anova(g1)

shapiro.test(g1$residuals)

# The results tell us that levels of different factors have significantly different means taken singularly, 
# but interactions are not significant.

# Then, we should not consider the interaction:

g1_sel = lm( 1/time ~ poison + treat, data = rats )
summary( g1_sel )
anova( g1_sel )

# check the hypotheses (residual normality and homoscedasticity) of the model.
par( mfrow = c( 1, 2 ) )
qqnorm( g1_sel$res/summary( g1_sel )$sigma, pch = 16, main = 'QQ-norm of residuals'  )
abline( 0, 1, lwd = 2, lty = 2, col = 'red' )

shapiro.test( g1_sel$res )

plot( g1_sel$fitted, g1$res, xlab = "Fitted", ylab = "Residuals",
      main = "Reciprocal response", pch = 16 )
abline(h=0, col='red')

# The hypotheses are respected. 

# confronto di modelli: g1_sel C g1
anova(g1, g1_sel)

rm(list=ls())



