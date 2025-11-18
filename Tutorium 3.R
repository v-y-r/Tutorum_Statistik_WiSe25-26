#Biostatistik einlesen
rm(list = ls())

data <- read.csv("C:/Users/ruehl/Nextcloud/Uni/9. Semester/Biostatistik/data/Biostatistik2025.csv")

#Davon nominal: "Augenfarbe", "BG_AB0", "BG_Rh", "Geschlecht"
#Davon ordinal: "Sportliche Betätigung", "Musikalische Betätigung"
#Alle anderen sind metrisch


#NAs pro Spalte
#Entweder aus der summary-Tabelle

summary(data)

#Einzeln für jede Spalte
table(is.na(data$KG_cm))
table(is.na(data$KG_Mutter_cm))
#repeat ad infinitum

# kompliziertere, aber sauberere Lösung
NAs <- data.frame(var = names(data), miss = NA)
for (i in 1:12)
NAs$miss[i] = 62 - table(is.na(data[,i]))[1]

NAs

#NAs entfernen

data <- na.omit(data)


#Data Cleaning Intermission (I'm sorry)
data_clean <- subset(data, KG_cm > 1)
        #Hier wurden alle Zeilen mit unrealistischen Werten entfernt, da diese das Histogramm stören
        #Dazu zählen z.B. Körpergrößen von einem Centimeter


#Histogramme

par(mfrow = c(2,2))

hist(data_clean$KG_Mutter_cm, xlab = "Größe Mütter [cm]", ylab = "Anzahl", main = "",
     breaks =  seq(150, 200, 1), col = "darkgreen")

hist(data_clean$KG_Vater_cm, xlab = "Größe Väter [cm]", ylab = "Anzahl", main = "",
     breaks =  seq(150, 200, 1), col = "pink")

hist(subset(data_clean, Geschlecht == "w")$KG_cm, xlab = "Größe Studierende weiblich [cm]", ylab = "Anzahl", main = "",
     breaks =  seq(150, 200, 1), col = "lightblue")

hist(subset(data_clean, Geschlecht == "m")$KG_cm, xlab = "Größe Studierende männlich [cm]", ylab = "Anzahl", main = "",
     breaks =  seq(150, 200, 1), col = "darkred")

#Und die dazugehörigen Werte

c(mean(data_clean$KG_Mutter_cm), sd(data_clean$KG_Mutter_cm),
  sd(data_clean$KG_Mutter_cm)/mean(data_clean$KG_Mutter_cm))          #Mütter

c(mean(data_clean$KG_Vater_cm), sd(data_clean$KG_Vater_cm),
  sd(data_clean$KG_Vater_cm)/mean(data_clean$KG_Vater_cm))            #Väter

aggregate(data_clean$KG_cm ~ data_clean$Geschlecht, data = data_clean,
          function(x) c(m = mean(x), s = sd(x), cv = sd(x)/mean(x)))  #Studierende


#Scatterplot Größenverhältnis

par(mfrow = c(1,3), oma = c(3,3,1,1))

col_G <- c("darkred", "lightblue")    #Farbfaktoren definieren zum später abrufen

plot(data_clean$KG_cm ~ data_clean$KG_Mutter_cm, col = col_G[as.factor(data_clean$Geschlecht)], pch = 19,
     xlab = "", ylab = "")
mtext(side = 3, "Mutter")


plot(data_clean$KG_cm ~ data_clean$KG_Vater_cm, col = col_G[as.factor(data_clean$Geschlecht)], pch = 19,
     xlab = "", ylab = "")
mtext(side = 3, "Vater")

#Für den letzten plot müssen wir den mean der Körpergröße der Eltern berechnen
#Dafür fügen wir einfach eine Spalte zum data frame hinzu

data_clean$mKG_Eltern_cm <- (data_clean$KG_Mutter_cm + data_clean$KG_Vater_cm)/2

plot(data_clean$KG_cm ~ data_clean$mKG_Eltern_cm, col = col_G[as.factor(data_clean$Geschlecht)], pch = 19,
     xlab = "", ylab = "")
mtext(side = 3, "Eltern")
legend("topleft", pch = 19, col = col_G, c("m", "w"))

#allg. Achsenbeschriftung
mtext(side = 1, outer = TRUE, "Körpergröße Studierende [cm]", cex = 1.3)
mtext(side = 2, outer = TRUE, "Körpergröße Eltern [cm]", cex = 1.3)


#Kovarianz und Korrelationskoeffizienten

cov(data_clean$mKG_Eltern_cm, data_clean$KG_cm)
cor(data_clean$mKG_Eltern_cm, data_clean$KG_cm)   #Mittelwert

cov(data_clean$KG_Mutter_cm, data_clean$KG_cm)
cor(data_clean$KG_Mutter_cm, data_clean$KG_cm)    #Mutter

cov(data_clean$KG_Vater_cm, data_clean$KG_cm)
cor(data_clean$KG_Vater_cm, data_clean$KG_cm)     #Vater


#Sport und Musik
#Faktoren zuweisen
data_clean$Sport_ord <- factor(data_clean$Sportliche.Betaetigung,
                                  levels = c("keine", "wenig", "mittel", "viel"), ordered = TRUE)
data_clean$Musik_ord <- factor(data_clean$musikalische.Betaetigung,
                                  levels = c("keine", "wenig", "mittel", "viel"), ordered = TRUE)

#Plotten

par(mfrow = c(1,2))

boxplot(data_clean$Sport_ord, yaxt = "no")
axis(2, at = c(1:4), levels(data_clean$Sport_ord), las=1)
axis(1, at = c(1), 'Sport')

boxplot(data_clean$Musik_ord, yaxt = "no")
axis(2, at = c(1:4), levels(data_clean$Musik_ord), las=1)
axis(1, at = c(1), 'Musik')


#Statistik!!!
#hier kann man die Quantile verwenden

quantile(data_clean$Sport_ord, 0.5, type = 1, na.rm = TRUE) 
quantile(data_clean$Musik_ord, 0.5, type = 1, na.rm = TRUE) 

#na.rm muss TRUE sein, da in einer Zeile zwei Antworten angegeben wurden


#Musik und Sport in Kombination

#table erstellen

M_S_Kombi <- table(Musik = data_clean$Musik_ord, Sport = data_clean$Sport_ord)

#plotten
par(mfrow = c(1,1))

barplot(M_S_Kombi, col = 1:4, xlab = "Sport", ylab = "Häufigkeit", cex.lab = 1.3)
legend("topleft", fill = 1:4, levels(data_clean$Musik_ord), cex = 0.8, title = "Musik")

mosaicplot(M_S_Kombi, col = 1:4, main = "", las = 1)
#keine Legende beim Mosaikplot

#max und min der Kombinationen

which(M_S_Kombi == max(M_S_Kombi), arr.ind = TRUE)
which(M_S_Kombi == min(M_S_Kombi), arr.ind = TRUE)






