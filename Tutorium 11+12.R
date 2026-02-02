rm(list=ls())

library(AICcmodavg)
library(dplyr)
library(car)
library(effects)

#Aufgabe 1

#a
pup_size <- read.table("C:/Users/ruehl/Nextcloud/Uni/9. Semester/Biostatistik/data/pup_size.txt",
              header = T, skip = 2, stringsAsFactor = T)
as.data.frame(table(region = pup_size$region,  m = pup_size$m, sex = pup_size$sex))

#b - d
pup_size$sex = as.factor(pup_size$sex)
pup_size$m = as.factor(pup_size$m)

m1 = glm(sex ~    region * m, data = pup_size, family = binomial) #interaction model
Anova(m1)                                                         #Type II Anova

plot(allEffects(m1))


m2 = glm(sex ~   m + region, data = pup_size, family = binomial)
Anova(m2)


AIC(m1,m2)

plot(allEffects(m2))

#Monat und Region nehmen beide Einfluss, aber keine Signifikanz beim Interaktionsterm, 
#also kein Einfluss des Zeitpunkts der Probenahme
#Aussagekraft niedrig, aufgrund niedriger Stichprobenzahl für manche Ereignisse

#e
summary(m2)

pup_rm = as.data.frame(expand.grid(region = unique(pup_size$region), m = as.factor(c(1,2))))
#neuer df mit Region-Monat-Kombinationen
pup_rm$pc_w = predict(m2, pup_rm, type = 'response')
#predict für Mittelwerte de Weibchenanteils
pup_rm

#f
exp(-0.79657) #Chancenverhältnis, ein Weibchen in Region H zu finden als in AR

#für den prozentualen Anteil muss der Ausgangswert berücksichtigt werden, es gibt also einen
#unterscheid in den Anteilen nach Monat

# AR: im Monat 1 
exp(-0.66062 ) / (1 + exp(-0.66062 ))

# H: im Monat 1 
exp(-0.66062 + -0.79657) / (1 + exp(-0.66062 + -0.79657 ))

( 34.06 - 18.89 )/18.89
#80 % mehr Weibchen in AR in M1

#das Gleiche für den zweiten Monat
(48.08 - 29.45)/29.45
#63 % mehr Weibchen in AR in M2

#das gleiche nur mit (Monat 1-Monat 2)/Monat 2 für Teilaufgabe g



#Aufgabe 2

da <- read.table("C:/Users/ruehl/Nextcloud/Uni/9. Semester/Biostatistik/data/clutch_sizes.csv", header = TRUE, sep = ";")
dga <- subset(da, species == 'Daphnia galeata')
dlo <- subset(da, species == 'Daphnia longispina')
par(mfrow = c(2,1), mar = c(4,4,1,1))
plot(table(clutch_size = dga$clutch_size), xlim = c(0, 4),
     lend = 2, lwd = 10, main = 'D. galeata', ylab = 'Häufigkeit')
plot(table(clutch_size = dlo$clutch_size), xlim = c(0,4),
     lend = 2, lwd = 10, main = 'D. longispina', ylab = 'Häufigkeit')

#b
aggregate(clutch_size ~ species, data = da, function(x) c(m = mean(x), var = var(x)))

#c
m3 <- glm(clutch_size ~ species, family = "poisson", data = da)
summary(m3)

#Gelegegrößen unterscheiden sich signifikant

#d
exp(-1.2657)

#e
#Die Gelegeröße der beiden Arten unterschied sich signikant
#(beta = -1.2657, SE = 0.3414, z =-3.707, p < 0.000209),
#wobei die Gelegeröße von D. longispina 28 % der Gelegegröße von D. galeata betrug.







