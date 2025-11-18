rm(list = ls())

#Aufgabe 1
#a
5/8

#b
5/8*2/5 = 2/8
#oder
4/8*2/4 = 2/8

#c
(5/8+4/8) - (2/8)

#d
Nein

#e
Ja

#f
Nein
(3/8)+(4/8) = 3/16  #Berechnete Wahrscheinlichkeit der Schnittmenge
1/8                 #Tatsächliche Wahrscheinlichkeit der Schnittmenge
3/16 != 1/8 

#g
1/2 #Zwei Netze mit Leptodora, davon 1 mit Bosmina

#h
1/4 #Vier Netze mit Bosmina, davon 1 mit Leptodora

#i
1/4 #Eines der Netze muss am Ende übrig bleiben -> 1/4 aller Netze enthält Leptodora
    #-> 1/4 aller möglichen Ziehungen hat am Ende Leptodora übrig

#j
2/8*1/7 = 1/28

#k
1-5/8 = 3/8


#Aufgabe 2
#a
P = 0.172 #Steht so im Text

#b
#s. Bild

#c
P = 0.52*0.172 = 0.089

#d
P(Raucher und Krebs) = P(Raucher)*P(Raucher|Krebs) = 0.52*0.172 = 0.089

#e
P(Nichtraucher und Kein Krebs) = 0.48*0.987 = 0.474


#Aufgabe 3
hist(rnorm(10000, mean = 40, sd = 10), breaks = 40) #Histogramm mit 10000 Hechten und Mean/SD aus Aufgabenstellung
abline(v = 50, col = 2, lwd = 2)                    #50cm markieren


pnorm(50, mean = 40, sd = 10, lower.tail = FALSE)   #zeigt Wsl. für Werte rechts von 50 in der Normalverteilung
#oder
1 - pnorm(50, mean = 40, sd = 10)                   #zeigt Wsl. für Werte links von 50 und subtrahiert diese von 1


#Aufgabe 4

x1 <- rnorm(10, mean = 0, sd = 3)

par(mfcol = c(2,7), mar = c(1,1,1,1)) #mfcol sorgt dafür, dass erst die Spalten und dann die Zeilen aufgefüllt werden

hist(x1, main = "")
qqnorm(x1, main = "")
qqline(x1, col = 2, lty = 2)


for ( i in 1:6) {
  x1 = rnorm(10, mean = 0, sd = 3)
  hist(x1, main = '')
  qqnorm(x1, main = '')
  qqline(x1, col = 2, lty = 2, lwd = 2)
  }

#Alternativ kann auch jede Verteilung einzeln gemacht werden

#Das ganze dann mit 100 Werten

for ( i in 1:7) {
  x1 = rnorm(100, mean = 0, sd = 3)
  hist(x1, main = '')
  qqnorm(x1, main = '')
  qqline(x1, col = 2, lty = 2, lwd = 2)
}


#Aufgabe 5

ba_supp <- read.table("C:/Users/ruehl/Nextcloud/Uni/9. Semester/Biostatistik/data/BrainAllometry_Supplement_Data.csv",
                      sep = ",", header = TRUE, skip = 1)

par(mfrow = c(2,2), mar = c(4,4,1,1))

hist(ba_supp$Mean_brain_mass_g, main = "")
qqnorm(ba_supp$Mean_brain_mass_g, main = "")
qqline(ba_supp$Mean_brain_mass_g, main = "", col = 2)

#logarithmieren
ba_supp$log_brain_mass <- log(ba_supp$Mean_brain_mass_g)

#same procedure as every year, James
hist(ba_supp$log_brain_mass, main = "")
qqnorm(ba_supp$log_brain_mass, main = "")
qqline(ba_supp$log_brain_mass, main = "", col = 2)


