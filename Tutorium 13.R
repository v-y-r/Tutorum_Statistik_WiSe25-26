rm(list = ls ())

library(AICcmodavg)
library(dplyr)
library(car)
library(effects)


#Aufgabe 1; BIER!!!
bio25 <- read.csv("C:/Users/ruehl/Nextcloud/Uni/9. Semester/Biostatistik/data/Biostatistik2025.csv", stringsAsFactors = TRUE)

bio25 <- na.omit(bio25) 
bio25_clean <- subset(bio25, KG_cm > 1)

par(mfrow = c(1,2))
plot(bio25_clean$Bierkonsum  ~ bio25_clean$KG_cm, data = bio25_clean , col = as.factor(bio25_clean$Geschlecht), pch = 19)
plot(log10(bio25_clean$Bierkonsum + 10)  ~ bio25_clean$KG_cm, data = bio25_clean , col = as.factor(bio25_clean$Geschlecht), pch = 19)


#Konsum und Körpergröße
cor.test(bio25_clean$Bierkonsum , bio25_clean$KG_cm, method = 'spearman')
#p = 0.002215 -> Bierkonsum korreliert mit Körpergröße

cor.test(bio25_clean$Bierkonsum , bio25_clean$KG_cm, method = 'kendall')
#p = 0.002731 -> Bierkonsum korreliert mit Körpergröße

cor.test(bio25_clean$Bierkonsum , bio25_clean$KG_cm, method = 'pearson')
#p = 0.03661 -> Bierkonsum korreliert mit Körpergröße


#Konsum und Geschlecht
t.test(log10(bio25_clean$Bierkonsum + 10) ~ bio25_clean$Geschlecht)
#p = 0.02523 -> Bierkonsum interagiert mit Geschlecht

bier_model <- lm(log10(bio25_clean$Bierkonsum + 10) ~ bio25_clean$Geschlecht)
summary(bier_model)

anova(bier_model)
#p = 0.005595 -> Bierkonsum interagiert mit Geschlecht

wilcox.test(Bierkonsum ~ Geschlecht, data = bio25_clean)
#p = 0.006985 -> Bierkonsum interagiert mit Geschlecht


#Konsum, Geschlecht und Körpergröße

bier_model_2 <- lm(log10(Bierkonsum + 10) ~ Geschlecht, data = bio25_clean)
anova(bier_model_2)

bier_model_3 <- lm(log10(Bierkonsum + 10) ~ Geschlecht + KG_cm, data = bio25_clean)
Anova(bier_model_3)

bier_model_4 <- lm(log10(Bierkonsum + 10) ~ Geschlecht * KG_cm, data = bio25_clean)
Anova(bier_model_4)

AIC(bier_model, bier_model_2, bier_model_3, bier_model_4)

plot(allEffects(bier_model_4, residuals = TRUE))


#Aufgabe 2; noch mehr BIER!!!!
bio25_clean$Bier_JN = ifelse(bio25_clean$Bierkonsum > 0, 1, 0) #teste für >0, wenn ja, gib Wert 1, sonst Wert 0
m1_logist = glm(Bier_JN ~ Geschlecht , data = bio25_clean, family = binomial)
summary(m1_logist)

m2_logist = glm(Bier_JN ~ KG_cm , data = bio25_clean, family = binomial)
summary(m2_logist)

m3_logist = glm(Bier_JN ~ KG_cm + Geschlecht, data = bio25_clean, family = binomial)
summary(m3_logist)
Anova(m3_logist)

m4_logist = glm(Bier_JN ~ KG_cm * Geschlecht, data = bio25_clean, family = binomial)
summary(m4_logist)

Anova(m4_logist)

AIC(m1_logist, m2_logist, m3_logist, m4_logist)

plot(allEffects(m4_logist, residuals = T))


#Aufgabe 3; Diskussion
#Wir finden signifkante Zusammenhänge sowohl zwischen Geschlechtern (erwartet) als auch im Zusammenhang mit der
#Körpergröße (erwartet, da männliche Studierende größer sind als weibliche Studierende).
#Sowohl logistische Regression als auch lineares Modell mit logtransformierten Daten legen zumindest tendenziell nahe,
#daß sich die Körpergrößenabhängigkeit zwischen den Geschlechtern unterscheidet - und zwar Zusammenhang bei Frauen vorhanden,
#aber nicht bei Männern. Dies ist ein überraschender Befund, da eher umgekehrtes Ergebnis zu erwarten ist.
#Zusammenhang bei Männern könnte mit geringerer Stichprobengröße zu tun haben - es ist aber auch unklar,
#wie aussagekräftig der Zusammenhang bei den Frauen tatsächlich ist. Insgesamt bleibt gewisse Unsicherheit in Bezug auf
#die Körpergrößenabhängig (zusätzlich zu der Geschlechter-bedingten Körpergrößenabhängigkeit).
#Was würden wir schließen, wenn wir solche Daten erhalten hätten bei einem Untersuchungsgegenstand von dem wir kein so
#großes Verständnis haben?











