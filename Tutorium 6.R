rm(list = ls())

#Tutorium 6
#Aufgabe 1
linearer Zusammenhang der Werte, Normalverteilte Residuen (Abweichungen vom LRM), Varianzhomogenität
Kleiner x-Fehler im verlgeich zum y-Fehler, unabhängige Datenpunkte


#Aufgabe 2
#a)
Nein, da die Berechnung unabhängig von der Position im LRM ist. r= s_xy/(s_x × s_y)

#b)
Nein, das legen wir ja selbst fest

#c)
Nein, da R2 Aussage über die Varianz der Werte zur Regressionsgerade gibt. Diese verändern sich nicht
durch vertauschen der Achsen.

#d)
Ja, da wir auf einmal die Achsen spiegeln


#Aufgabe 3
#a)
#estimated coefficient divided by estimated standard error
t1 <- 1.05751/0.01353
t2 <- 0.14970/0.01293

#b)
#sum of sqares divided by degrees of freedom
Mean.SQ1 <- 0.41366/1
Mean.SQ2 <- 0.04631/15

#Mean of Squares Between divided by Mean of Squares Within
F <- Mean.SQ1/Mean.SQ2

#Sum of Squares between divided by total Sum of Squares
R2 <- 0.41366/(0.41366+0.04631)


#Aufgabe 4
#data prep und cleaning
bio24 <- read.csv("C:/Users/ruehl/Nextcloud/Uni/9. Semester/Biostatistik/data/Biostatistik2025.csv")

bio24 <- na.omit(bio24) 
bio24_clean <- subset(bio24, KG_cm > 1)

bio24_clean$KG_Eltern_cm <- (bio24_clean$KG_Mutter_cm + bio24_clean$KG_Vater_cm)/2

#scatterplot erstellen
plot(bio24_clean$KG_Eltern_cm, bio24_clean$KG_cm, pch = 19, xlab = "KG Eltern [cm]", ylab = "KG Studi [cm]")

#Regressionsanalyse durchführen
lm_bio <- lm(KG_cm ~ KG_Eltern_cm, data = bio24_clean)
summary(lm_bio)

#Geradengleichungin Plot einfügen
abline(lm_bio, col = "red")

#diagnostische Plots erstellen und auswerten
par(mfrow = c(2,2))
plot(lm_bio)
#sieht doch gut aus alles. Ein Ausreißer mit Punkt 3, aber sonst liegt es recht nah an den Geraden.
#Cooks'D auch sehr niedrig für alle Werte, also keine Werte die besonders viel Einfluss auf den lm fit haben

#e)
Die mittlere Körpergröße der Studierenden steigt mit 1.17 cm (± 0.22 cm SE) pro cm mittlerer Körpergröße
der Eltern an (t_55 = 5.31, p < 0.001), wobei die Steigung signifikant von 0 verschieden ist, jedoch nicht von 1 
(da Std. Error über 1, wodurch 1 mit eingeschlossen ist)


#Aufgabe 5
#a) Scatterplots
par(mfrow = c(1,1))
plot(bio24_clean$KG_Mutter_cm ~ bio24_clean$KG_Vater_cm, pch = 19, xlab = "KG Väter [cm]", ylab = "KG Mütter [cm]")

#b) Regressionsmodell
lm_eltern <- lm(KG_Mutter_cm ~ KG_Vater_cm, bio24_clean)
summary(lm_eltern)

abline(lm_eltern, col = "red")
#c) Diagnostik
par(mfrow = c(2,2))
plot(lm_eltern)
#p16 hat hohe Hebelwirkung mit Cooks'D > 0.5 -> hohe Hebelwirkung auf das gesamte LRM

#d)
#Alles nochmal machen, aber die Ausreisser rausnehmen

#Abfrage der höchsten Werte (Nur Mütter, weil abhängige Variable)
which.max(bio24_clean$KG_Mutter_cm)
bio24_clean_2 <- bio24_clean[-14,]

par(mfrow = c(1,1))
plot(bio24_clean_2$KG_Mutter_cm ~ bio24_clean_2$KG_Vater_cm, pch = 19, xlab = "KG Mütter [cm]", ylab = "KG Väter [cm]")

lm_eltern_2 <- lm(KG_Mutter_cm ~ KG_Vater_cm, bio24_clean_2)
summary(lm_eltern_2)

abline(lm_eltern_2, col = "red")

par(mfrow = c(2,2))
plot(lm_eltern_2)

 #Auswertung: Besser als vorher, aber immer noch keine signifikanten Zusammenhänge (p-value nicht signifikant).
#p42 hat auch stärkeren Einfluss auf LRM, aber Cooks'D < 0.5, also kein Grund zu wiederholen
#Theoretisch sind wir Nahe an Literaturwerten zum corr.coef von 0.2 (R2 = 0.033 -> r = 0.18)


