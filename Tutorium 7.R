rm(list = ls())


#Aufgabe 1
#Anforderungen: normalverteilte Residuen, Varianzhomogenität und unabhängige Daten
#Prüfung: qqplot, Plot von Standartresiduen/Modellvorhersagen, Studiendesign


#Aufgabe 2

#a
#alpha 0.05 gilt nur für einen individuellen Test. Werden viele verwandte Tests durchgeführt, steigt die Chance auf einen alpha-Fehler
#dadurch ist alpha = 0.05 nicht mehr eingehalten

#b
#FWER: Wahrscheinlichkeit mindestens einen Typ I Fehler zu begehen bei nah verwandeten Tests (Familie von Tests)
#FDR: erwarteter Anteil von falsch positiven Tests an der Gesamtzahl an Tests, die H0 verwerfen

#c
#FWER - Kontrolle notwendig bei multiplen tests, bei sehr vielen Tests geht dies aberauf Kosten der power und sehr vielen Typ II Fehlern.
#FDR akzeptiert höhere Anzahl an Typ I Fehlern -> Kompromiss zwischen Typ I und Typ II Fehlern


#Aufgabe 3
bio25 <- read.csv("C:/Users/ruehl/Nextcloud/Uni/9. Semester/Biostatistik/data/Biostatistik2025.csv", stringsAsFactors = TRUE)

bio25 <- na.omit(bio25) 
bio25_clean <- subset(bio25, KG_cm > 1)

#a
t.test(bio25_clean$KG_cm ~ bio25_clean$Geschlecht)
#p = 0.0001067 -> Nullhypothese verworfen

#b
model_KG_1 <- lm(KG_cm ~ Geschlecht, data = bio25_clean)
summary(model_KG_1)
anova(model_KG_1)
#auch hier verwerfen wir die Nullhypothese, yippiee

#c
par(mfrow = c(2,2))
plot(model_KG_1)

#d
#d steht für: Das schafft ihr inzwischen bestimmt selber, ihr seid alle kluge Köpfe und habt ganz oft mit mir Diagnostik gemacht :)
#Ich glaub an euch!!!
#PS: Wenn's wirklich nicht geht, meldet euch nochmal bei mir ^^


#Aufgabe 4
t.test(bio25_clean$KG_Mutter_cm, bio25_clean$KG_Vater_cm)
#p = 3.209e-15 -> Eltern sind verschieden groß

#Zwei Möglichkeiten für den nächsten Teil
#paired test
t.test(bio25_clean$KG_Mutter_cm, bio25_clean$KG_Vater_cm, paired = T)

#one-sample test
t.test(bio25_clean$KG_Mutter_cm - bio25_clean$KG_Vater_cm, mu = 0)
#für beide: p = 1.051e-12 -> Eltern sind verschieden groß

#und für unsere visuellen Lerner, hier noch ein plot von dem one sample test model
hist(I(bio25_clean$KG_Mutter_cm - bio25_clean$KG_Vater_cm), main = '', 
     xlab = 'Differenz Körpergröße der Eltern [cm]')

#Auswertung:
#Bei beiden Tests hoch signifiante Unterschiede. Hier hat der paarweise t Test keinen größeren t Wert, da durch
#die Paarbildung nicht wirklich Fehler reduziert werden (keine starke Korrelation in der KG bei den Eltern vorhanden)
#sondern sogar eher große Ausreisser entstehen (siehe Histogram)


#Aufgabe 5

#a
#H0: keine Unterschiede in der KG bei verschiedener Augenfarbe
#H1: die KG unterscheidet sich bei mindestens einer Augenfarbe

#b
par(mfrow = c(2,2))
hist(subset(bio25_clean, Augenfarbe == 'grün')$KG_cm, col = 'green', breaks = seq(150, 200,5), xlab = 'Koerpergroesse [cm]', main = '')
hist(subset(bio25_clean, Augenfarbe == 'grau')$KG_cm, col = 'grey', breaks = seq(150, 200,5), xlab = 'Koerpergroesse [cm]', main = '')
hist(subset(bio25_clean, Augenfarbe == 'braun')$KG_cm, col = 'brown', breaks = seq(150, 200,5), xlab = 'Koerpergroesse [cm]', main = '')
hist(subset(bio25_clean, Augenfarbe == 'blau')$KG_cm, col = 'blue', breaks = seq(150, 200,5), xlab = 'Koerpergroesse [cm]', main = '')

aggregate(KG_cm ~ Augenfarbe, data = bio25_clean, function(x) c(n = length(x), m = mean(x), se = sd(x)/sqrt(length(x))))

#c
model_KG_Augen <- lm(KG_cm ~  Augenfarbe , data =  bio25_clean)
summary(model_KG_Augen)
anova(model_KG_Augen)

#d
par(mfrow = c(2,2))
plot(model_KG_Augen)
#sind die nicht alle wieder wunderchön? Vor allem mein Lieblingsplot, der unten links. Immer noch keinen Plan was der eigentlich macht
#Tatsächlich ist der qqplot Grenzwertig, Normalverteilung ist also nicht sicher gegeben

#e
coefficients(model_KG_Augen)
#blau: 167.9473684
#braun: 167.9473684 + 5.2526316 = 173.2
#grau: 167.9473684 + 11.0526316 = 179
#gruen: 167.9473684 + -0.8807018 = 167.0666667
#Mathe :/

#f
TukeyHSD(aov(model_KG_Augen))

#und plotten
par(mar = c(4,6,2,2), mfrow = c(1,1))
plot(TukeyHSD(aov(model_KG_Augen)), las = 1)

#g
#g steht für: Geschlechtertrennung. Ein archaisches Prinzip was bei der Entstehung des Patriarchats genutzt wurde um systematisch gegen die
#Unabhängigkeit der weiblichen Bevölkerung vorzugehen. Aber wie alles schlechte ist es auch echt schwierig, davon weg zu kommen, weswegen
#die einzigen All-Gender Toiletten in den Naturwissenschaften nachträglich hinzugefügt wurden und es insgesamt nur 2 in der Bio gibt.
#zum Vergleich, wir haben 9 Männer- und Frauen-Toiletten. Versteh ich einfach nicht.
#naja... Weiter zur Teilaufgabe g
m_KG_Au_M <- lm(KG_cm ~  Augenfarbe , data =  subset(bio25_clean, Geschlecht == 'm'))
summary(m_KG_Au_M)
anova(m_KG_Au_M)

m_KG_Au_W <- lm(KG_cm ~  Augenfarbe , data =  subset(bio25_clean, Geschlecht == 'w'))
summary(m_KG_Au_W)
anova(m_KG_Au_W)

#h
#h steht für: Hier gibt's die Diskussion, die ist etwas aufwändiger
#Gesamtdatensatz ergibt das Überraschende Ergebnis dass KG Unterschiede vorhanden sind (grün < grau und blau < grau).
#Dagegen können die H0 bei den geschlechtsspezifischen Anovas nicht verworfen werden (jeweils p > 0.05).
#Grund für den Unterschied ist die ungleiche Verteilung von Geschlechtern auf die Augenfarben:
#Grün und blau mit relativ wenig m: -> kleine KG, grau mit 4 m : 4 w -> große KG
table(bio25_clean$Augenfarbe, bio25_clean$Geschlecht)
#d.h. im Test mit allen Studierenden wird H0 letztendlich fälschlicherweise verworfen aufgrund eines
#nicht ausbalanzierten Datensatzes bezüglich der Geschlechter (wir erinnern uns an den Grenzwertigen qqplot)












