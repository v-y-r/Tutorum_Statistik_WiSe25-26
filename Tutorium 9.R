rm(list =ls ())

install.packages("AICcmodavg")

library(AICcmodavg)
library(dplyr)
library(car)
library(effects)


#Aufgabe 1

ac <- read.table("C:/Users/ruehl/Nextcloud/Uni/9. Semester/Biostatistik/data/pup_size.txt",
                 header = T, skip = 2, stringsAsFactor = T)
ac$mo <- as.factor(ac$m)
ac$g <- paste(ac$sex, ac$region, ac$mo) #paste erstellt eine Spalte in denen als character 
                                        #die Werte aus allen gegebenen Spalten zeigt

colv <- rep(c('cyan', 'salmon'), 6)
par(mfrow = c(3,4))

for (i in 1:12) {
  x = subset(ac, g == unique(ac$g)[i])
  ttt = paste(unique(x$sex),unique(x$region), unique(x$mo), ', n = ', length(x$l))
  hist(x$l, breaks = seq(5,12,0.5), main = ttt, col = colv[i],
       las = 1, xlab = 'length [mm]')
}


#Aufgabe 2

m_ac = lm(l ~ sex * mo * region, data = ac)
m_ac2 = lm(l ~ (sex + mo + region)**2, data = ac)
m_ac3 = lm(l ~ (sex * mo + region), data = ac)
m_ac4 = lm(l ~ (sex + mo * region), data = ac)
m_ac5 = lm(l ~ (sex *  region + mo), data = ac)


summary(m_ac)$adj.r.squared
summary(m_ac2)$adj.r.squared
summary(m_ac3)$adj.r.squared
summary(m_ac4)$adj.r.squared
summary(m_ac5)$adj.r.squared

AIC(m_ac, m_ac2, m_ac3, m_ac4, m_ac5) #Akaike Information Criterion

BIC(m_ac, m_ac2, m_ac3, m_ac4, m_ac5) #Bayesian Information Criterion

#Beide liefern Information darüber, wie Aussagekräftig ein Model ist. Je niedriger, desto besser 

Anova(m_ac2)
Anova(m_ac3)


#Aufgabe 3

par(mfrow = c(2,2), oma = c(3,3,5,2))
plot(m_ac2) #bester AIC

plot(m_ac3) #bester BIC

plot(allEffects(m_ac2))

plot(allEffects(m_ac3))

#m_ac2: Sowohl Geschlecht, Standort und Monat beeinflussen die Puppengröße
#Unterschied Geschlecht:Monat (Interaktionsterm: F1,503 = 24.8, p < 0.001)
#Unterschied Geschlecht:Region (Interaktionsterm: F2, 503 = 7.2, p < 0.001)

#m_ac3: Geschlecht und Region beeinflussen immer die Größe, Monat nur bei weibchen (s. Plot)
#Unterschied Geschlecht:Monat (Interaktion, F1,507 = 12.9, p < 0.001)






