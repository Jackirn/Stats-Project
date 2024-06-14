## "Laboratorio con `R` - 5"
## Metodi e Modelli per l'Inferenza Statistica - Ing. Matematica - a.a. 2023-24

## Topics:  
## - Logistic linear regression  

## 0. Librerie

library( rms )
library(arm)
library(ResourceSelection)
library(pROC)
library(PRROC)


## 1. Regressione logistica semplice

# Prendiamo in esame il dataset relativo ad uno studio clinico su pazienti affetti da 
# disturbi coronarici. In particolare, l'obiettivo dello studio consiste nello spiegare 
# la presenza o l'assenza di significativi disturbi coronarici (CHD) in funzione dell'et? 
# (variabile AGE) dei pazienti. I dati si riferiscono a 100 pazienti. 
# Le variabili del database sono descritte nel file *CHDAGE_data_description.txt*:
  
# CHD: variabile dipendente binaria (1 se il disturbo ? presente, 0  se il disturbo ? assente);
# AGE: variabile indipendente ( continua ).

#Importiamo i dati.

chd = read.table( "CHDAGE_data.txt", head = TRUE )

str( chd )

head( chd )

# Visualizziamo i dati.

plot( chd$AGE, chd$CHD, pch = ifelse( chd$CHD == 1, 3, 4 ),
      col = ifelse( chd$CHD == 1, 'forestgreen', 'red' ),
      xlab = 'Age', ylab = 'CHD', main = 'CHD vs. Age', lwd = 2, cex = 1.5 )

# Eseguiamo quindi un'analisi descrittiva del dataset.
# Per meglio comprendere la natura della relazione ? opportuno suddividere i pazienti 
# in classi d'et? e calcolare la media della variabile dipendente in ciascuna classe.

# Inseriamo nel vettore x i limiti delle classi d'et? che si vogliono creare 
# (questo passaggio ? arbitrario, e va eseguito con buon senso).

min( chd$AGE )
max( chd$AGE )

x  = c( 20, 29, 34, 39, 44, 49, 54, 59, 70 )

# Calcoliamo i punti medi degli intervalli che abbiamo creato
mid = c( ( x [ 2:9 ] + x [ 1:8 ] )/2 )

# Suddividiamo i dati nelle classi che abbiamo creato
GRAGE = cut( chd$AGE, breaks = x, include.lowest = TRUE, right = FALSE )
GRAGE

# Calcoliamo quindi la media della variabile AGE stratificata e sovrapponiamo 
# i valori di y al grafico precedente.

y = tapply( chd$CHD, GRAGE, mean )
y


plot( chd$AGE, chd$CHD, pch = ifelse( chd$CHD == 1, 3, 4 ),
col = ifelse( chd$CHD == 1, 'forestgreen', 'red' ),
xlab = 'Age', ylab = 'CHD', main = 'CHD vs. Age', lwd = 2, cex = 1.5 )
points( mid, y, col = "blue", pch = 16 )

# Dal grafico si intuisce la natura della relazione fra AGE e CHD 
# (all'aumentare dell'et?, aumenta anche il rischio di avere problemi alle coronarie).

# Identifichiamo un modello che descriva adeguatamente i nostri dati --> lavagna
help( glm )

mod = glm( CHD ~ AGE, family = binomial( link = logit ), data=chd )
summary( mod )

# Qual ? quindi il modello stimato? 

# Calcoliamo i valori stimati per il logit della probabilit? di avere disturbi coronarici 
# (sono i logit di pi_i, che giustamente hanno un range continuo).

mod$linear.predictors

# Calcoliamo i valori stimati per la probabilit? di avere disturbi coronarici 
# ( che coincidono con una funzione dell'esponenziale dei valori ottenuti al punto prima ). 
# Sono le pi_i predette, pertanto comprese in [0, 1].

mod$fitted.values
exp(mod$linear.predictors)/(1+exp(mod$linear.predictors))

# Facciamo un grafico della predizione del modello.

plot( chd$AGE, chd$CHD, pch = ifelse( chd$CHD == 1, 3, 4 ),
col = ifelse( chd$CHD == 1, 'forestgreen', 'red' ),
xlab = 'Age', ylab = 'CHD', main = 'CHD vs. Age', lwd = 2, cex = 1.5 )
points( mid, y, col = "blue", pch = 16 )
lines( chd$AGE, mod$fitted, col = 'darkblue' )

# Interpretazione dei coefficienti? OR?

# Calcoliamo l'OR relativo a AGE.

summary( mod )

# Quindi l'OR per un incremento di 10 anni d'et? ?:
  
exp( 10 * coef( mod ) [ 2 ] )

# per ogni incremento di 10 anni d'et?, il rischio di disturbo coronarico aumenta di 3 volte circa.

# il modello sottointende che il logit sia lineare nella variabile et?, 
# ossia che l'OR fra persone di 20 contro 30 anni sia lo stesso che fra individui di 40 contro 50 anni.

# IC per la regressione logistica

# Calcoliamo un intervallo di confidenza al 95% per l'OR per un incremento di 10 anni d'et?.

alpha = 0.05
qalpha =  qnorm( 1 - alpha/2 )
qalpha

IC.sup = exp( 10 * coef( mod ) [ 2 ]  + qalpha * 10 * summary( mod )$coefficients[ 2, 2 ] )
IC.inf = exp( 10 * coef( mod ) [ 2 ] - qalpha * 10 * summary( mod )$coefficients[ 2, 2 ] )
c( IC.inf, IC.sup )

# Per costruire in `R` l'intervallo di confidenza del logit si pu? partire dal calcolo 
# della matrice di covarianza dei parametri beta stimati:

V = vcov( mod )
V

# Per calcolare l'IC predittivo, abbiamo bisogno di calcolare l'errore standard, che in questo 
# caso misura la curvatura della log-likelihood trovata per stimare la probabilit?. 
# Si trova come la radice quadrata del reciproco dell'informazione di Fisher 
# (valutata alla massima verosimiglianza).
# Per esempio, scegliendo un valore casuale (tipo AGE=50) possiamo trovarla come:
x = 50

# errore standard
predict( mod, data.frame( AGE = 50 ), se = TRUE )

# oppure calcolando a mano la Var (beta0 + beta1*x)
sqrt( V [ 1, 1 ]  + x^2 * V [ 2, 2 ]  + 2 * x * V [ 1, 2 ] )

# Rappresentiamo graficamente l'intervallo di confidenza (al 95%) della regressione:

# griglia di valori di x in cui valutare la regressione
grid = ( 20:69 )

se = predict( mod, data.frame( AGE = grid ), se = TRUE )
# errori standard corrispondenti ai valori della griglia

help( binomial )
gl = binomial( link = logit )  # funzione di link utilizzata
# Family objects provide a convenient way to specify the details of the models
# used by functions such as glm.


plot( mid, y, col = "red", pch = 3, ylim = c( 0, 1 ), ylab = "Probability of CHD",
xlab = "AGE", main = "IC per la Regressione Logistica" )
lines( grid, gl$linkinv( se$fit ) )   # la funzione `gl$linkinv` permette di ottenere il valore
                                      # delle probabilit? a partire dal predittore lineare.
lines( grid, gl$linkinv( se$fit - qnorm( 1-0.025 ) * se$se ), col = "red", lty = 2 )
lines( grid, gl$linkinv( se$fit + qnorm( 1-0.025 ) * se$se ), col = "red", lty = 2 )


# la funzione `gl$linkinv` permette di ottenere il valore delle probabilit? a partire 
# dalla link function (logit).

## Goodness of fit

# Varie tecniche sono state sviluppate e confrontate per stabilire la bont? del fit di una regressione 
# logistica. Problema: tali tecniche soffrono di una limitata potenza 
# (tipicamente non superiore al 50%) per campioni di dimensione contenuta (indicativamente n < 400).

# Se la variabile indipendente ? categorica si possono paragonare i valore di Devianza del modello fittato 
# con il valore critico di una distribuzione chi^2( n-p ),  dove p ? il numero di parametri del modello.
# Se la statistica D ? maggiore del valore critico si rifiuta l'ipotesi nulla che il modello sia un buon fit.

# Se la variabile indipendente ? continua ( es in questione ), la procedura precedente perde di validit?. 
# L'alternativa che `R` fornisce richiede l' installazione di 
# due librerie supplementari ( `Design` e `Hmisc`), che contengono le funzioni lrm e residuals per calcolare 
# tale statistica.

help( lrm )

mod2 = lrm( chd$CHD ~ chd$AGE, x = TRUE, y = TRUE )
mod2
# p-value LRT basso --> devianza del modello completo è minore, modello significativo

# Il Model Likelihood Ratio Test riguarda la GOF di due modelli in competizione. 

## Visualize the residuals (between observed and predicted probabilities) with a binned plot
binnedplot(chd$AGE, rstandard(mod))

#In questo caso, osserviamo un fit abbastanza buono del modello (reisidui concentrati intorno allo 0), 
#eccetto per le osservazioni con circa 55 anni, che sono leggermente sovrastimate.
#Alternativamente, possiamo usare come GOF test, il test di Hosmer-Lemeshow. 

hoslem.test( mod$y, fitted( mod ), g = 10 )

#In questo test dobbiamo scegliere g, numero di gruppi. Nel paper originale ? suggerito di scegliere g > p, 
#in questo caso quindi g > 2 (intercetta e AGE). Si vede che, anche cambiando g, giungiamo alla stessa 
#conlusione, ovvero il modello fitta bene i dati (vorrei pvalue alto). In generale la scelta del numero di gruppi a priori ? un 
#limite di questo test.


## 2. Regressione logistica multipla

# In questo esercizio analizzeremo un dataset clinico inerente al peso di neonati.
# Lo scopo dello studio consiste nell'identificare i fattori di rischio associati con il partorire 
# bambini di peso inferiore ai 2500 grammi ( low birth weight ). I dati si riferiscono a n = 189 donne.

# Le variabili del database sono descritte nel file "LOWBWT_data_description.txt":

# __LOW__: variabile dipendente binaria ( 1 se il neonato pesa meno di 2500 grammi, 0 viceversa );

# __AGE__: Age of the Mother in Years 

# __LWT__: Weight in Pounds at the Last Menstrual Period 

# __FTV__ Number of Physician Visits During the First Trimester 

# __RACE__ 3-levels character variable.


# Importiamo i dati.

lw = read.table( "LOWBWTdata.txt", head = TRUE )
str(lw)

lw$RACE   = factor( lw$RACE , ordered = F)  # tratto la variabile RACE come categorica
table(lw$RACE)

mod.low = glm( LOW ~ LWT + RACE + AGE + FTV, family = binomial( link = logit ) , data = lw)
summary( mod.low )

# Riduciamo il modello
mod.low2 = glm( LOW ~ LWT + RACE + AGE, family = binomial( link = logit ) , data = lw)
summary( mod.low2 )

# Riduciamo il modello
mod.low2 = glm( LOW ~ LWT + RACE, family = binomial( link = logit ) , data = lw)

summary( mod.low2 )


# Notiamo che AIC diminuisce (pi? l'AIC ? basso, pi? il modello ? informativo) 

anova( mod.low2, mod.low, test = "Chisq" )

# L'ANOVA indica che la variazione nella devianza risultante dalla rimozione delle variabili non ? 
# statisticamente significativa. Dunque, non c'? motivo di ritenere che il modello contenente 
# solamente LWT e RACE sia meno informativo del modello completo.

# Odds ratio 

# Il predittore RACE ? discreto a 3 livelli. In questo caso il livello 1 viene assunto 
# come categoria di riferimento. E infatti
model.matrix( mod.low2 ) [ 1:15, ]

# OR RACE 2 vs 1?
exp( coef( mod.low2 ) [ 3 ] )

# Le donne di etnia 2 sono una categoria con rischio di parto sottopeso quasi 3 volte superiore alle donne di etnia 1.

# OR 3 vs 1 ( Other vs White )
exp( coef( mod.low2 ) [ 4 ] )

# Le donne di etni 3 sono una categoria con rischio di parto prematuro circa 1.5 volte superiore alle 
# donne di etnia 1.

# Facciamo un check sul GOF del modello.

hoslem.test( mod.low2$y, fitted( mod.low2 ), g = 6 )   #g > 4

# Anche in questo caso, possiamo concludere che il modello d? un buon fit dei dati.


# Tabelle di (mis-)classificazione

# Un modo spesso utilizzato per presentare i risultati di un fit tramite regressione logistica sono 
# le tabelle di (mis-)classificazione. In queste tabelle i dati vengono classificati secondo due chiavi:
  
# __1.__ il vero valore della variabile dipendente dicotoma y;

# __2.__ il valore di una variabile dicotoma y_{mod}, che si deriva dalla stima della probabilit? ottenuta dal modello.
#        I valori di questa variabile si ottengono confrontando il valore della probabilit? con un cut-off. 
#        Di solito si usa il valore di 0.5, ma dipende molto da quello che stiamo indagando. 

soglia = 0.5

valori.reali  = lw$LOW
valori.predetti = as.numeric( mod.low2$fitted.values > soglia )
# 1 se > soglia, 0 se < = soglia
valori.predetti

tab = table( valori.reali, valori.predetti )

tab

# La tabella riportata ? detta matrice di confusione, e riporta le osservazioni dette 
# Veri Positivi (True Positive o TP, osservazioni 1 classificate come 1), 
# Veri Negativi (True Negative o TN, osservazioni 0 classificate come 0), 
# Falsi Positivi (False Positive o FP, osservazioni 0 classificati come 1), 
# Falsi Negativi (Falsi Negativi o FN, osservazioni 1 classificati come 0). 

# Ci sono numerose metriche che permettono di valutare le performance del modello, a seconda delle esigenze:
# Accuracy, Sensitivity, Specificity

# Accuracy

# % di casi classificati correttamente:
round( sum( diag( tab ) ) / sum( tab ), 2 )

# % di casi misclassificati:
round( ( tab [ 1, 2 ] + tab [ 2, 1 ] ) / sum( tab ), 2 )

# Sensitivity
sensitivita =  tab [ 2, 2 ] /( tab [ 2, 1 ] + tab [ 2, 2 ] ) 
sensitivita

# Specificity 
specificita = tab[ 1, 1 ] /( tab [ 1, 2 ] + tab [ 1, 1 ] )
specificita



## 3. Curva ROC

# Costruire la Curva ROC a partire dai valori predetti per la risposta dal modello `mod.low2` 
# dell'analisi della variabile LOWBT.

# Le curve ROC (Receiver Operating Characteristic, anche note come Relative Operating Characteristic) 
# sono degli schemi grafici per un classificatore binario.

# Una curva ROC ? il grafico dell'insieme delle coppie (FP, TP) al variare di un parametro del classificatore. 

fit2 = mod.low2$fitted


#media campionaria della prob di sopravvivenza nel campione

soglia_roc  = seq( 0, 1, length.out = 2e2 )
lens = length( soglia_roc )-1
ascissa_roc  = rep( NA, lens )
ordinata_roc = rep( NA, lens )

for ( k in 1 : lens )
{
  soglia = soglia_roc [ k ]
  
  classification = as.numeric( sapply( fit2, function( x ) ifelse( x < soglia, 0, 1 ) ) )
  
  #  ATTENZIONE, voglio sulle righe il vero e sulle colonne il predetto
  # t.misc = table( lw$LOW, classification )
  
  ordinata_roc[ k ] = sum( classification[ which( lw$LOW == 1 ) ] == 1 ) /
    length( which( lw$LOW == 1 ) )
  
  ascissa_roc[ k ] = sum( classification[ which( lw$LOW == 0 ) ] == 1 ) /
    length( which( lw$LOW == 0 ) )
  
  # ordinata_roc [ k ]  = t.misc [ 1, 1 ] /( t.misc [ 1, 1 ] + t.misc [ 1, 2 ] )
  #
  # ascissa_roc [ k ]  = t.misc [ 2, 1 ] /( t.misc [ 2, 1 ] + t.misc [ 2, 2 ] )
}


# Visualizziamo la curva ROC.

plot( ascissa_roc, ordinata_roc, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity",
      main = "Curva ROC", lwd = 2, col = 'darkblue', ylim = c( 0, 1 ), xlim = c( 0, 1 ) )
abline( h = c( 0, 1 ), v = c( 0, 1 ), lwd = 1, lty = 2, col = 'red' )
abline( a = 0, b = 1, lty = 2, col = 'black' )

# qual era il nostro punto?
abline( v = 1 - specificita,  h = sensitivita, lty = 3, col = 'blue' )
points( 1 - specificita, sensitivita, pch = 4, lwd = 3, cex = 1.5, col = 'blue' )

# Le linee tratteggiate corrispondono alle due metriche calcolate con la threshold = 0.5 che abbiamo scelto. 

# Attraverso l'analisi delle curve ROC si valuta la capacit? del classificatore calcolando 
# l'area sottesa alla curva ROC (Area Under Curve, AUC). 

# R fa tutto in automatico --> calcola AUC e p0ottimale 

PRROC_obj <- roc.curve(scores.class0 = fit2, weights.class0=as.numeric(paste(lw$LOW)),
                       curve=TRUE)

plot(PRROC_obj)

# Se AUC < 0.5, possiamo invertire i positivi e i negativi.
# Se AUC = 0.5, classificatore random
# Se AUC > 0.5, bene

# Dalla ROC curve, scelgo il valore di soglia, classifico nuovamente e calcolo la tabella di misclassificazione e i relativi indici. 





