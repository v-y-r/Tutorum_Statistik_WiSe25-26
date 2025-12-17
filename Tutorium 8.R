rm(list = ls())

#Aufgabe 1
Anova dient zuerst dem vergleich von mehr als 2 Gruppen innerhalb eines Tests. Damit ist sie besser für
komplexere Experimente als ein T-Test. 

Typ 1 Anova vergleicht die Mittelwerte der getesteten Gruppen anhand einer einzigen unabhängigen Variable.
z.B. Wachstumsgröße und Blattspreite von Grashalmen an drei verschiedenen Standorten. Der Standort ist hier die unabhängige Variable
Typ 1 berücksichtigt die Reihenfolge der abhängigen Variablen, die wichtigste sollte also an erster Stelle stehen

Typ 2 Anova vergleicht die Mittelwerte anhand zweier unabhängiger Variablen
z.B. Wachstumsgröße und Blattspreite von Grashalmen an drei verschiedenen Standorten und bei verschiedener Lichtintensität.
Typ 2 berücksichtigt die Reihenfolge nicht, weswegen sie genutzt werden sollte, wenn die abhängigen Variablen nicht gewichtet sind

Typ 3 Anova vergleicht die Mittelwerte anhand aller anderer Faktoren und Interaktionen.
Sie ist besonders dann anzuwenden, wenn bekannt ist, dass die Interkationen zwischen unabhängiger und abhängiger Variablen
signifikant sind


#Aufgabe 2
Wenn nicht auf eine Interaktion getestet wird, kann die Annahme getroffen werden, dass die Auswirkungen der Faktoren A und B generell sind,
sie also nicht jeweils vom anderen Faktor abhängen.
Da diese Annahme falsch sein kann, sollte deshalb immer in einem linearen Modell mit zwei Faktoren getestet werden


#Aufgabe 3
Dummy-Kodierung: Eine Art der Kodierung bei der einer nicht-numerischen Variable ein numerischer Wert zugeordnet wird.
Dabei gilt wenn eine Variable teil der festgelegten Kategorie ist, bekommt sie den Wert 1, sonst den Wert 0

Männchen_Monat_1 <- 6.44 + 0 * 1.63 + 0 * -0.175 + 0 * -0.81
Weibchen_Monat_1 <- 6.44 + 1 * 1.63 + 0 * -0.175 + 0 * -0.81
Männchen_Monat_2 <- 6.44 + 0 * 1.63 + 1 * -0.175 + 0 * -0.81
Weibchen_Monat_2 <- 6.44 + 1 * 1.63 + 1 * -0.175 + 1 * -0.81


#Aufgabe 4
install.packages("car")
install.packages("effects")
library(car)
library(effects)

bio25 <- read.csv("C:/Users/ruehl/Nextcloud/Uni/9. Semester/Biostatistik/data/Biostatistik2025.csv", stringsAsFactors = TRUE)

bio25 <- na.omit(bio25) 
bio25_clean <- subset(bio25, KG_cm > 1)

#nur ein lineares Model
lin_model <- lm(KG_cm ~ Augenfarbe + Geschlecht, data = bio25_clean)
summary(lin_model)

#zeigt von 5 Möglichen Tests 3 mit signifikanten Unterschieden
#Männer mit blauen Augen sind größer als 0 cm
#Männer mit grauen Augen sind größer als Männer mit blauen Augen
#Männer sind größer als Frauen
#Um hier sagen zu können, welche davon klar als signifikant interpretiert werden können, machen wir eine Anova
#Wenn die Anova auch einen signifikanten Effekt zeigt, können wir es annehmen

#Anova Typ 2
Anova(lin_model, type = 2)

#zeigt, dass wir nur für das Geschlecht die Nullhypothese ablehnen können

#Vergleich mit anderen Anovas
anova(lin_model)      #Mehr signifikante effekte in Typ 1, aber falsch, da wir von gleicher Gewichtung der unabhängigen Variablen ausgehen

#für Typ 3 brauchen wir ein Interaktionsmodell
int_model <- lm(KG_cm ~ Augenfarbe * Geschlecht, data = bio25_clean)
summary(int_model)

anova(int_model)
Anova(int_model, type = 2)
Anova(int_model, type = 3)

#Kein Unterschied in der Ablehnung bei Typ 2 und 3, die Interaktion der unabhängigen Variablen ist also nicht signifikant
#Somit kann die H0 bei Geschlecht verworfen werden, bei Augenfarbe nicht

par(mfrow = c(2,2))
plot(lin_model)
plot(int_model)

#Keine Auffälligkeiten in den Diagnostik-Plots

par(mfrow = c(2,1))
plot(allEffects(lin_model, residuals = TRUE))
plot(allEffects(int_model, residuals = TRUE))

#Zu 95% keine Überlappung der Konfidenzintervalle bei geschlecht im non-interaction model, aber Überlappung bei Augenfarbe
#interaction model plots sind weniger eindeutig


#Aufgabe 5
par(mfrow = c(1,1))

Geschlecht <- c("darkred", "lightblue")
plot(bio25_clean$Schuhgroesse_EU ~ bio25_clean$KG_cm, col = Geschlecht[as.factor(bio25_clean$Geschlecht)], pch = 19)

#wieder unser lineares Modell

lin_model_2 <- lm(Schuhgroesse_EU ~ KG_cm + Geschlecht , data = bio25_clean)
int_model_2 <- lm(Schuhgroesse_EU ~ KG_cm * Geschlecht , data = bio25_clean)

#hier reicht für die Interpretation eine Anova Typ 2

Anova(lin_model_2, type = 2)
Anova(int_model_2, type = 3)

#Effects Plots
plot(allEffects(lin_model_2))

#Schuhgröße ist signifikant Abhängig von Körpergröße und Geschlecht, aber es gibt keine signifikante Interaktion der
#beiden unabhängigen Variablen
#Möglicherweise alpha-Fehler, da die Überlappung der Körpergrößen zwischen männlich und weiblich gering ist
#Die nicht-signifikante Interaktion könnte also aus Datenmangel heraus entstanden sein --> Unsicherheit













