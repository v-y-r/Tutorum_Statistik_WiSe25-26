rm(list =ls ())


#Aufgabe 1, Fischproben

#entweder
(10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1) / ((3 * 2 * 1) * (7 * 6 * 5 * 4 * 3 * 2 * 1))

#oder
factorial(10) / (factorial(3) * factorial(7))


#Aufgabe 2, Wasserflohproben

#a
#H1: Gleichverteilung ist nicht vorhanden
#H0: Gleichverteilung ist vorhanden
#Wir testen auf die Gleichverteilung, also muss im Chi-Test diese unsere H0 sein

#b
daphnia <- c(13,15,14,7, 10 )
erwartet <- rep(sum(daphnia)/5, 5)

#c
chi <- (daphnia - erwartet) / sqrt(erwartet)

#d
chi2 = sum(chi**2)

#e
1 - pchisq(chi2, df = length(daphnia) -1)

#f
chisq.test(daphnia)


#Aufgabe 5, ~ViDeOsPiElE~ FeRnSeHeR mAcHeN kInDeR aGgReSiV!!1!1

Kinder_gesamt <- c(88, 386, 233)
Kinder_aggresiv <- c(5, 87, 67)

#a
percent <- Kinder_aggresiv/Kinder_gesamt

#b
tv <- matrix(rbind(Kinder_aggresiv, Kinder_gesamt-Kinder_aggresiv), ncol = 3)
rownames(tv) = c('aggresiv','nicht-aggresiv')
colnames(tv) = c('wenig','mittel', 'viel')
tv

chisq.test(tv)

#c
#Nicht wirklich, nein. Die Studie ist rein beobachtend, nicht experimentell. Die Daten
#können also durch viele Gründe beeinflusst werden. Dies wurde zwar in die Studie selbst
#korrigiert, aber es wurden dennoch signifikante Assoziationen festgestellt
#z.B. Einkommen der Familie







