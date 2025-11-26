rm(list = ls())

#Aufgabe 1
#a)
Kein Effekt

#b)
sinkt/nimmt ab

#c)
Kein Effekt

#d)
steigt/nimmt zu


#Aufgabe 2
#a)
sinkt/nimmt ab

#b)
steigt/nimmt zu

#c)
kein Effekt

#d)
sinkt/nimmt ab


#Aufgabe 3
Fangfrage; die Nullhypothese wird nie akzeptiert. Man geht von der Nullhypothese immer als wahr aus, bis sie
abgelehnt wird. Man kann eine Nullhypothese nur ablehnen, nicht akzeptieren. Wie wahrscheinlich eine Ablehnung der
Nullhypothese ist, wird durch die statistische Power bestimmt (z.b. Stichprobengröße, Standartabweichung der Werte)


#Aufgabe 4
Die Wahrscheinlichkeit einen Fehler erster Art zu machen ist unabhängig von der Größe der Stichproben.
Wenn ein Signifikanzniveau festgelegt wird, gegen das gestestet wird, dann ist die Chance die Hypothese
falsch anzunehmen entsprechend dem Signifikanzniveau, da sich das Signifikanzniveau nicht von der Stichprobengröße
beeinflussen lässt.


#Aufgabe 5
Die Wahscheinlichkeit einen Fehler zweiter Art zu begehen sinkt bei steigender Stichprobenzahl. Je mehr Stichproben 
genommen werden, desto wahrscheinlicher ist es, dass ein existierender Effekt auch wahrgenommen wird.
Eine höhere Wahrscheinlichkeit der Wahrnehmung eines Effekts reduziert eine falsche Annahme der H0.


#Aufgabe 6
#a)
size_daphnia <- c(1.18, 0.74, 1.07, 1.24, 1.02, 0.91, 1.55, 1.4, 1.7,
                  1.02,0.71,0.96, 1.24, 0.92, 1.02, 1.36, 1.28, 0.99)

hist(size_daphnia, main = "Größenverhältnis: 20°C / 10°C")

#b)
H1: Die Größe der Daphnien wird von der Temperatur beeinflusst
H0: Die Größe der Daphnien ist unabhängig von der Temperatur

t.test(size_daphnia, mu = 1)

#c)
#zwei Schritte
str(t.test(size_daphnia, mu = 1))

t.test(size_daphnia, mu = 1)$conf.int

#d)
power.t.test(n = length(size_daphnia), delta = 1 - mean(size_daphnia), sd = sd(size_daphnia),
             sig.level = 0.05, type='one.sample')

#e)
power.t.test(power = 0.8, delta = 1 - mean(size_daphnia), sd = sd(size_daphnia),
             sig.level = 0.05, type='one.sample')

#f)
hist(log10(size_daphnia), main = "Größenverhältnis: 20°C / 10°C")

t.test(log10(size_daphnia), mu = 0)

power.t.test(n = length(size_daphnia), delta = 0 - mean(log10(size_daphnia)), sd = sd(log10(size_daphnia)),
             sig.level = 0.05, type='one.sample')

power.t.test(power = 0.8, delta = 0 - mean(log10(size_daphnia)), sd = sd(log10(size_daphnia)),
             sig.level = 0.05, type='one.sample')



hist(log(size_daphnia), main = "Größenverhältnis: 20°C / 10°C")

t.test(log(size_daphnia), mu = 0)

power.t.test(n = length(size_daphnia), delta = 0 - mean(log(size_daphnia)), sd = sd(log(size_daphnia)),
             sig.level = 0.05, type='one.sample')

power.t.test(power = 0.8, delta = 0 - mean(log(size_daphnia)), sd = sd(log(size_daphnia)),
             sig.level = 0.05, type='one.sample')

#g)
10^(t.test(log10(size_daphnia), mu = 0)$estimate)
10^(t.test(log10(size_daphnia), mu = 0)$conf.int)

exp(t.test(log10(size_daphnia), mu = 0)$estimate)
exp(t.test(log10(size_daphnia), mu = 0)$conf.int)


#Aufgabe 7
#a)

diff <- 1
sd <- 15
n <- 1000
(se <- sd/sqrt(n)) #Klammern um alles zeigen sofort das Ergebnis

(t <- diff/se)

(p <- 2 * (1 - pt(t, n)))

#T-Test kann nicht verwendet werden, da dafür die eigentlichen Testergebnisse benötigt werden. Mithilfe der Funktion
#oben, kann man p manuell berechnen. Alternativ kann ein random vektor mit sd 15 und mean diff 1 erstellt werden

#b)
(d <- diff/sd)

#c)
n2 <- 25
(se2 <- sd/sqrt(n2))

(t2 <- diff/se2)

(p2 <- 2 * (1 - pt(t2, n2)))

#d)
power.t.test(sd = 12, sig.level = 0.05, power = 0.9, delta= 1, 
              alternative='two.sided', type='one.sample')





