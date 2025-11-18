rm(list = ls())

#Aufgabe 1 habt ihr alle geschafft, wenn ihr ein neues Skript habt


#Aufgabe 2: radius-Vektor erstellen und mit seq() Werte zuweisen

radius <- seq(1,2,0.1)          #Die erste Zahl gibt den Start vor, die zweite Zahl das Ende und die dritte Zahl 
                                #die Schrittweite


#Aufgabe 3: Volumina für alle Radien berechnen und volumen-Vektor zuweisen

4/3 * pi * radius^3             #Formel für Kugelvolumen

volumen <- 4/3 * pi * radius^3  #Das Kugelvolumen dem Vektor 'volumen' zuweisen. Da 'radius' ein Vektor mit 11 Werten ist,
                                #wird die Funktion 11 Mal ausgeführt und man erhält 11 Ergebnisse


#Aufgabe 4: Data Frame 'kugel' erstellen und radien und volumina hinzufügen

kugel <- data.frame(radius = radius, Volumen = volumen) #data.frame erstellt einen Data Frame, wobei man zuerst den
                                                        #Spaltennamen angibt und dann nach einem '=' die Daten, in unserem
                                                        #Fall die Vektoren 'radius' und 'volumen'

head(kugel)                                             #Mit dem Befehl 'head' können wir die ersten 6 Zeilen sehen, um zu
                                                        #prüfen, ob der Befehl erfolgreich war


#Aufgabe 5: Spalte 'Oberfläche' zum Data Frame 'kugel' hinzufügen

kugel$Oberfläche <- 4*pi*radius^2 #Um eine weiter Spalte hinzuzufügen können wir einfach den Data Frame 'kugel' um den
                                  #Spaltennamen erweitern. Dazu verbinden wir den Data Frame via '$' mit dem neuen Namen
                                  #Anschließend müssen nur noch die Daten, in unserem Fall die Formel für die Oberfläche,
                                  #zugewiesen werden. Auch hier werden automatisch 11 Werte erstellt. 

head(kugel)                       #Am Ende immer überprüfen ob alles passt. Dies geht entweder mit dem 'head' Befehl
                                  #oder im 'Environment' Fenster


#Aufgabe 6: 'whitefish-sticklebacks.csv' einlesen mit 'sep = -,' und dem Objekt wh_st2 zuordnen

wh_st2 <- read.table("C:/Users/ruehl/Nextcloud/Uni/9. Semester/Biostatistik/data/whitefish_sticklebacks.csv",sep = ",")
                                  #diesmal nutzen wir für das einlesen 'read.table'. Dabei ändern wir im Befehl nur die
                                  #Argumente, bei denen wir nicht den default nutzen, in unserem Fall 'sep()'

dim(wh_st2)                       #Nach der Zuweisung überprüfen wir die Dimensionen mit 'dim()'. Da wir bei 'sep() ein
                                  #',' angegeben haben, die Datei aber nach ';' trennt, bekommen wir nur eine Spalte
                                  #statt vier Spalten


#Aufgabe 7: Ganz viele Daten einlesen und deren Dimensionen prüfen. Wiederholung ist wichtig!!

death_sf <- read.table("C:/Users/ruehl/Nextcloud/Uni/9. Semester/Biostatistik/data/chap10e6DeathsSpanishFlu1918.csv", sep = ",", header = TRUE)
dim(death_sf)  

ba_supp <- read.table("C:/Users/ruehl/Nextcloud/Uni/9. Semester/Biostatistik/data/BrainAllometry_Supplement_Data.csv", sep = ",", header = TRUE, skip = 1)  
dim(ba_supp)

world_pop <- read.table("C:/Users/ruehl/Nextcloud/Uni/9. Semester/Biostatistik/data/world_population_size.txt", header = TRUE, skip = 12)
dim(world_pop)

daphnia <- read.table("C:/Users/ruehl/Nextcloud/Uni/9. Semester/Biostatistik/data/Daphnia_1990.csv", header = TRUE)
dim(daphnia)

consumer <- read.table("C:/Users/ruehl/Nextcloud/Uni/9. Semester/Biostatistik/data/solomon-etal consumers.csv", sep = ';', header = TRUE, skip = 3)
dim(consumer)

ausfahrt <- read.table("C:/Users/ruehl/Nextcloud/Uni/9. Semester/Biostatistik/data/20151028-SST-bo-RoutineAusfahrt.txt", header = FALSE, skip = 33)
dim(ausfahrt) #Spaltennamen manuell zuordnen


#Aufgabe 8: 'ba_supp' in weitere Data Frames zerlegen

ba_supp_ceta <- ba_supp[ba_supp$order == "Cetacea",]  #Matrixnotation funktioniert nach dem Prinzip 
                                                      #dataframe[dataframe$spalte = "gesuchtes Objekt" , ]

ba_supp_brain_mass <- subset(ba_supp, Mean_brain_mass_g > 1000) #subset-Funktion funktioniert nach dem Prinzip
                                                                #subset(dataframe, Spalte = "gesuchtes Objekt")


#Extra: Manuell Spaltennamen zu Ausfahrt.txt
ausfahrt$Datasets <- ausfahrt$V1
ausfahrt$Press <- ausfahrt$V2
ausfahrt$Temp <- ausfahrt$V3
ausfahrt$Oxygn <- ausfahrt$V4
ausfahrt$pH <- ausfahrt$V5
ausfahrt$Cond <- ausfahrt$V6
ausfahrt$CAP25 <- ausfahrt$V7
ausfahrt$Chl_A <- ausfahrt$V8
ausfahrt$Bottm <- ausfahrt$V9
ausfahrt$DO_mg <- ausfahrt$V10
ausfahrt$Vbatt <- ausfahrt$V11
ausfahrt$IntT <- ausfahrt$V12
ausfahrt$IntD <- ausfahrt$V13


ausfahrt$V1 <- NULL
ausfahrt$V2 <- NULL
ausfahrt$V3 <- NULL
ausfahrt$V4 <- NULL
ausfahrt$V5 <- NULL
ausfahrt$V6 <- NULL
ausfahrt$V7 <- NULL
ausfahrt$V8 <- NULL
ausfahrt$V9 <- NULL
ausfahrt$V10 <- NULL
ausfahrt$V11 <- NULL
ausfahrt$V12 <- NULL
ausfahrt$V13 <- NULL