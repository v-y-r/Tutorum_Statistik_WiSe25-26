#BrainAllometry einlesen
rm(list = ls())

ba_supp <- read.table("C:/Users/ruehl/Nextcloud/Uni/9. Semester/Biostatistik/data/BrainAllometry_Supplement_Data.csv",
                      sep = ",", header = TRUE, skip = 1)  
                      #so wie letzte Woche


#Scatterplot mit mean brain mass auf y-Achse und mean body mass auf x-Achse

plot(Mean_brain_mass_g ~ Mean_body_mass_g, data = ba_supp, log ="xy",
     xlab = "Körpergewicht [g]", ylab = "Hirnmasse [g]", pch = 19, cex = 0.5)

#Hervorhebung der Wale

points(Mean_brain_mass_g ~ Mean_body_mass_g, data = subset(ba_supp, order == 'Cetacea'),
       col = 'cyan', pch = 19, cex = 0.5)

#Hervorhebung Homo sapiens

points(Mean_brain_mass_g ~ Mean_body_mass_g, data = subset(ba_supp, species == 'sapiens'),
       col = 'pink', pch = 18, cex = 1.5)

#Legende machen

legend("topleft", legend = c("H. sapiens", "Cetacea", "Other"), pch = c(18, 19, 19), col = c("pink", "cyan", "black"))


#Statistik!!!

orders = subset(ba_supp, order %in% c("Rodentia", "Carnivora", "Cetacea" , "Primates"))

orders_evaluation = aggregate(cbind(Mean_brain_mass_g, Mean_body_mass_g) ~ order,
                     data = orders, function(x) c(m = mean(x), s = sd(x), cv = sd(x)/mean(x)))              

head(orders_evaluation)


#Histogramm mit Gehirngewichten

par(mfrow = c(2,2))
hist(subset(orders, order == 'Rodentia')$Mean_brain_mass_g, main = 'Rodentia',xlab = 'Gehirngewicht [g]', seq(0, 10000, 500))
hist(subset(orders, order == 'Carnivora')$Mean_brain_mass_g, main = 'Carnivora', xlab = 'Gehirngewicht [g]', seq(0, 10000, 500))
hist(subset(orders, order == 'Primates')$Mean_brain_mass_g, main = 'Primates', xlab = 'Gehirngewicht [g]', seq(0, 10000, 500))
hist(subset(orders, order == 'Cetacea')$Mean_brain_mass_g, main = 'Cetacea', xlab = 'Gehirngewicht [g]', seq(0, 10000, 500))


#Gewichte in log

orders$lbrain  = log10(orders$Mean_brain_mass_g)
par(mfrow = c(2,2))
hist(subset(orders, order == 'Rodentia')$lbrain, main = 'Rodentia', xlab = 'Gehirngewicht [g]', seq(-1, 4,0.25))
hist(subset(orders, order == 'Carnivora')$lbrain, main = 'Carnivora', xlab = 'Gehirngewicht [g]',  seq(-1, 4,0.25))
hist(subset(orders, order == 'Primates')$lbrain, main = 'Primates', xlab = 'Gehirngewicht [g]', seq(-1, 4,0.25))
hist(subset(orders, order == 'Cetacea')$lbrain, main = 'Cetacea', xlab = 'Gehirngewicht [g]' , seq(-1, 4,0.25))


#Boxplot Gewichte

par(mfrow = c(1,2))
boxplot(Mean_brain_mass_g ~ order, data = orders)
boxplot(lbrain ~ order, data = orders, ylab = 'log10((Mean_brain_mass_g)')



