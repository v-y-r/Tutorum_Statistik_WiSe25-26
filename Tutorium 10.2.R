rm(list =ls ())

#Aufgabe 3, nochmal Daphnia nehmen

daphnia = c(13,15,14,7, 10 )
daphnia2 = daphnia * 10

chisq.test(daphnia2)

#Aufgrund des höheren Aufwand und mehr Proben, sind die Daten robuster, aussagekräftiger 
#und die H0 kann verworfen werden


#Aufgabe 4, Titanic!!!

#a
tit_class = matrix(c(203, 122, 508, 1368), ncol = 2)
row.names(tit_class)  = c('survived', 'dead')
colnames(tit_class)  = c('class I', 'others')

#b
(rel_risk1 = (tit_class[2,1]/colSums(tit_class)[1]))  #relatives Risiko erste Klasse

(rel_risk2 = (tit_class[2,2]/colSums(tit_class)[2]))  #relatives Risiko alle anderen

rel_risk1 / rel_risk2


#c
(odds1 = tit_class[1,1]/ tit_class[2,1])

(odds2 = tit_class[1,2]/ tit_class[2,2])

(OR = odds1 / odds2)

#d
log_odds = log(OR)

se <- sqrt(1/tit_class[1,1] + 1/ tit_class[2,1] +
             1/ tit_class[1,2] + 1/ tit_class[2,2])

# unteres CI
exp(log_odds - 1.96 * se)

# oberes CI
exp(log_odds + 1.96 * se)

#e
(expected <- matrix(c(
  (rowSums(tit_class)[1] * colSums(tit_class)[1]) / sum(tit_class) ,
  (rowSums(tit_class)[2] * colSums(tit_class)[1]) / sum(tit_class) ,
  (rowSums(tit_class)[1] * colSums(tit_class)[2]) / sum(tit_class) ,
  (rowSums(tit_class)[2] * colSums(tit_class)[2]) / sum(tit_class) ),ncol = 2))

(chi = (tit_class - expected) / sqrt(expected))

(chi2 = sum(chi**2))

#f
chisq.test(tit_class)

chisq.test(tit_class, correct = F) #ausschließen der Yates-Korrektur













