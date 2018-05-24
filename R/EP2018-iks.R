library(econum)

#importation des mesures de l'IKS

source("R/Analyse respiro.R")

repos_load("Data/ecophysio2018/iks/C_2018-04-26_00.00.00_5AE0FA60_IKS.RData")
iks2 <- EcoNumData_IKS.C

repos_load("Data/ecophysio2018/iks/C_2018-04-27_00.00.08_5AE24BE8_IKS.RData")
iks3 <- EcoNumData_IKS.C

repos_load("Data/ecophysio2018/iks/C_2018-04-25_00.01.40_5ADFA944_IKS.RData")
iks1 <-EcoNumData_IKS.C

library(tidyverse)
EP2018_iks <- bind_rows(iks1,iks2,iks3)

rm(iks1,iks2,iks3,EcoNumData_IKS.C)

library(readxl)

#Importation du tableau avec les mesures iks et les mesures avec l'oximètre pour faire le racalage des points
iks_cor <- read_excel("/media/sf_Shared/Projects/ecophysio2018/Data/ecophysio2018/iks/iks-cor.xlsx",
                      col_types = c("text", "numeric", "numeric",
                                    "text"))

#On change la manière dont les dates sont enregistrées
iks_cor$date <- as.POSIXct(iks_cor$date)


#On va avoir un graphique avec la concentration en oxygène en fonction du temps. On va avoir une courbe de la mesure de la sonde en O2 et une mesure WRWO2. Les Deux mesures (du début et de la fin) sont très importantes car elles vont servir à requaler.
#Normalement quand on lance les respiromètres on a la même eau dans tous. Avec 6 respiromètres on a 6 mesures et on peut les comparer entre elles. La variabilité donne une idée car il y a une variabilité de l'appareil, de la sonde (car elle perd en performance au cours du temps) et l'utilisateur (si on agite pas assez il n'y a pas un débi d'eau suffisant et donc on sous estime l'O2 et si on agite trop on oxygénise trop le milieu). Ces mesures permettent d'intégrer toutes les variabilités. LEs mesures à la fin, on ne s'attend pas à ce qu'elles soient les mêmes, mais c'est très importantes qu'elles aient été faite de la même façon. Il y a une calibration 1 point (avec le tampon humidifié), il y a aussi une calibration 2 points avec du sulfite de sodium (pas fait mais mieux car meilleur précision) car il réagit directement avec l'oxygène dans l'eau et on sait qu'ainsi il y a zéro d'oxygène car le sulfite de sodium réagit avec tout l'oxygène. Donc les mesures de début et de fin sont cruciales car on va recaler toutes les mesures dessus.L'idée est de recaler le signal en partant du premier point et d'aller le recaler pour aboutir au point de la fin, en statistiue cela correspond à faire une approximation linéaire entre les deux points.
#Il y a des codes pour faire cela, dans le fichier Analyse respiro.
#calib.values : WTW

#Maintenant on va travailler respiro par respiro pour ainsi regarder la consommation et/ou production en oxygène dans chaque respiromètre

##Respiromètre 1

iks_cor_1 <- filter(iks_cor, respiro == "R1")

###Ici on va corriger les valeurs enregistrées en 02 dans les respiromètres en fonction des mesures que l'on a prise avec l'oxymètre et l'iks

EP2018_iks$O2_1_cor <- correct_monitoring(EP2018_iks$Time, values = EP2018_iks$O2_1, calib.dates = iks_cor_1$date, calib.values = iks_cor_1$O2, extrapolate = TRUE)

plot(EP2018_iks$O2_1_cor)

###On va maintenant extraire les périodes qui nous interesse en coupant dans la série les moments ou le respiromètre est fermé et pour chaque période ajuster une droite et calculer la pente de la droite, on donne une série d'informations (volume du respiro, masse de la bouture,...)

###On repart du bon graphique, celui corrigé

plot_iks_R1 <- plot(EP2018_iks$Time, EP2018_iks[["O2_1_cor"]], type = "l", xlab = "Time",
  ylab = "[O2] mg/L"); grid()

###On va placer dessus des lignes avec les points de début et de fin entre chaque cycle. On le fait deux fois pour bien altener quand cela s'ouvre et se ferme.

#On sépare en fonction de cycle O/F
test <- as.POSIXct("2018-04-25 12:00:00")
c <- (1:46)*60*60
c <- c+test

plot_iks_R1 + abline(v = c)

test <- as.POSIXct("2018-04-25 12:00:00")
h <- (1:46)*120*60
h <- h+test

plot_iks_R1 + abline(v = h, col = "red")

#On sépare par des lignes vert en fonction du jour et de la nuit

test <- as.POSIXct("2018-04-25 09:00:00")
d <- (1:46)*12*60*60
d <- d+test

plot_iks_R1 + abline(v = d, col = "green")
### Avec la fonction identify on sait mettre les bons points entre chaque cycle.

pos1 <- identify(EP2018_iks$Time, EP2018_iks[["O2_1_cor"]])

###On utilise maintenant la fonction respirometry pour voir à chaque cycle comment à évolué l'oxygène.

res_O2_1 <- respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n =1, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1")

res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+1, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+2, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+3, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+4, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+5, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+6, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+7, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+8, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+9, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+10, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+11, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+12, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+13, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+14, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+15, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+16, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+17, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+18, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+19, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+20, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+21, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))
res_O2_1 <- rbind(res_O2_1, respirometry(EP2018_iks, "O2_1_cor", pos = pos1, n = 1+22, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1"))

#On sépare le tableau de mesure du respiromètre 1 en fonction du jour et de la nuit

respi1_day1 = res_O2_1[c('1':'4'), ]
respi1_day2 = res_O2_1[c('11':'16'), ]
respi1_day3 = res_O2_1[c('23':'23'), ]
respi1_day <- rbind(respi1_day1,respi1_day2,respi1_day3)

respi1_night1 = res_O2_1[c('5':'10'), ]
respi1_night2 = res_O2_1[c('17':'22'), ]
respi1_night <- rbind(respi1_night1,respi1_night2)

#on fait la moyenne

respi1_mean_day <-mean(respi1_day$respi)
respi1_mean_night <- mean(respi1_night$respi)


#On refait la même chose pour tout les respiromètre

# Respiromètre 2

iks_cor_2 <- filter(iks_cor, respiro == "R2")

###Ici on va corriger les valeurs enregistrées en 02 dans les respiromètres en fonction des mesures que l'on a prise avec l'oxymètre et l'iks

EP2018_iks$O2_2_cor <- correct_monitoring(EP2018_iks$Time, values = EP2018_iks$O2_2, calib.dates = iks_cor_2$date, calib.values = iks_cor_2$O2, extrapolate = TRUE)

plot(EP2018_iks$O2_2_cor)

###On va maintenant extraire les périodes qui nous interesse en coupant dans la série les moments ou le respiromètre est fermé et pour chaque période ajuster une droite et calculer la pente de la droite, on donne une série d'informations (volume du respiro, masse de la bouture,...)

###On repart du bon graphique, celui corrigé

plot_iks_R2 <- plot(EP2018_iks$Time, EP2018_iks[["O2_2_cor"]], type = "l", xlab = "Time",
                    ylab = "[O2] mg/L"); grid()

###On va placer dessus des lignes avec les points de début et de fin entre chaque cycle. On le fait deux fois pour bien altener quand cela s'ouvre et se ferme.

#On sépare en fonction de cycle O/F
test <- as.POSIXct("2018-04-25 12:00:00")
c <- (1:46)*60*60
c <- c+test

plot_iks_R2 + abline(v = c)

test <- as.POSIXct("2018-04-25 12:00:00")
h <- (1:46)*120*60
h <- h+test

plot_iks_R2 + abline(v = h, col = "red")

#On sépare par des lignes vert en fonction du jour et de la nuit

test <- as.POSIXct("2018-04-25 09:00:00")
d <- (1:46)*12*60*60
d <- d+test

plot_iks_R2 + abline(v = d, col = "green")
### Avec la fonction identify on sait mettre les bons points entre chaque cycle.

pos2 <- identify(EP2018_iks$Time, EP2018_iks[["O2_2_cor"]])

###On utilise maintenant la fonction respirometry pour voir à chaque cycle comment à évolué l'oxygène.

res_O2_2 <- respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n =1, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2")
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+1, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+2, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+3, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+4, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+5, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+6, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+7, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+8, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+9, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+10, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+11, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+12, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+13, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+14, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+15, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+16, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+17, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+18, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+19, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+20, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+21, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))
res_O2_2 <- rbind(res_O2_2, respirometry(EP2018_iks, "O2_2_cor", pos = pos2, n = 1+22, mass = 1.093, ref.respi = 0, vol.respi = 1.3, main = "R2"))

#On sépare le tableau de mesure du respiromètre 2 en fonction du jour et de la nuit

respi2_day1 = res_O2_2[c('1':'4'), ]
respi2_day2 = res_O2_2[c('11':'16'), ]
respi2_day3 = res_O2_2[c('23':'23'), ]
respi2_day <- rbind(respi2_day1,respi2_day2,respi2_day3)

respi2_night1 = res_O2_1[c('5':'10'), ]
respi2_night2 = res_O2_1[c('17':'22'), ]
respi2_night <- rbind(respi2_night1,respi2_night2)

#on fait la moyenne

respi2_mean_day <-mean(respi2_day$respi)
respi2_mean_night <- mean(respi2_night$respi)










#Respiromètre 3

iks_cor_3 <- filter(iks_cor, respiro == "R3")

###Ici on va corriger les valeurs enregistrées en 02 dans les respiromètres en fonction des mesures que l'on a prise avec l'oxymètre et l'iks

EP2018_iks$O2_3_cor <- correct_monitoring(EP2018_iks$Time, values = EP2018_iks$O2_3, calib.dates = iks_cor_3$date, calib.values = iks_cor_3$O2, extrapolate = TRUE)

plot(EP2018_iks$O2_3_cor)

###On va maintenant extraire les périodes qui nous interesse en coupant dans la série les moments ou le respiromètre est fermé et pour chaque période ajuster une droite et calculer la pente de la droite, on donne une série d'informations (volume du respiro, masse de la bouture,...)

###On repart du bon graphique, celui corrigé

plot_iks_R3 <- plot(EP2018_iks$Time, EP2018_iks[["O2_3_cor"]], type = "l", xlab = "Time",
                    ylab = "[O2] mg/L"); grid()

###On va placer dessus des lignes avec les points de début et de fin entre chaque cycle. On le fait deux fois pour bien altener quand cela s'ouvre et se ferme.

#On sépare en fonction de cycle O/F
test <- as.POSIXct("2018-04-25 12:00:00")
c <- (1:46)*60*60
c <- c+test

plot_iks_R3 + abline(v = c)

test <- as.POSIXct("2018-04-25 12:00:00")
h <- (1:46)*120*60
h <- h+test

plot_iks_R3 + abline(v = h, col = "red")

#On sépare par des lignes vert en fonction du jour et de la nuit

test <- as.POSIXct("2018-04-25 09:00:00")
d <- (1:46)*12*60*60
d <- d+test

plot_iks_R3 + abline(v = d, col = "green")

### Avec la fonction identify on sait mettre les bons points entre chaque cycle.

pos3 <- identify(EP2018_iks$Time, EP2018_iks[["O2_3_cor"]])

###On utilise maintenant la fonction respirometry pour voir à chaque cycle comment à évolué l'oxygène

res_O2_3 <- respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n =1, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3")
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+1, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+2, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+3, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+4, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+5, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+6, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+7, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+8, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+9, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+10, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+11, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+12, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+13, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+14, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+15, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+16, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+17, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+18, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+19, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+20, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+21, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))
res_O2_3 <- rbind(res_O2_3, respirometry(EP2018_iks, "O2_3_cor", pos = pos3, n = 1+22, mass = 1.290, ref.respi = 0, vol.respi = 1.3, main = "R3"))

#On sépare le tableau de mesure du respiromètre 3 en fonction du jour et de la nuit

respi3_day1 = res_O2_3[c('1':'4'), ]
respi3_day2 = res_O2_3[c('11':'16'), ]
respi3_day3 = res_O2_3[c('23':'23'), ]
respi3_day <- rbind(respi3_day1,respi3_day2,respi3_day3)

respi3_night1 = res_O2_3[c('5':'10'), ]
respi3_night2 = res_O2_3[c('17':'22'), ]
respi3_night <- rbind(respi3_night1,respi3_night2)

#on fait la moyenne

respi3_mean_day <-mean(respi3_day$respi)
respi3_mean_night <- mean(respi3_night$respi)






#Respiromètre 4

iks_cor_4 <- filter(iks_cor, respiro == "R4")

###Ici on va corriger les valeurs enregistrées en 02 dans les respiromètres en fonction des mesures que l'on a prise avec l'oxymètre et l'iks

EP2018_iks$O2_4_cor <- correct_monitoring(EP2018_iks$Time, values = EP2018_iks$O2_4, calib.dates = iks_cor_4$date, calib.values = iks_cor_4$O2, extrapolate = TRUE)

plot(EP2018_iks$O2_4_cor)

###On va maintenant extraire les périodes qui nous interesse en coupant dans la série les moments ou le respiromètre est fermé et pour chaque période ajuster une droite et calculer la pente de la droite, on donne une série d'informations (volume du respiro, masse de la bouture,...)

###On repart du bon graphique, celui corrigé

plot_iks_R4 <- plot(EP2018_iks$Time, EP2018_iks[["O2_4_cor"]], type = "l", xlab = "Time",
                    ylab = "[O2] mg/L"); grid()

###On va placer dessus des lignes avec les points de début et de fin entre chaque cycle. On le fait deux fois pour bien altener quand cela s'ouvre et se ferme.

#On sépare en fonction de cycle O/F
test <- as.POSIXct("2018-04-25 12:00:00")
c <- (1:46)*60*60
c <- c+test

plot_iks_R4 + abline(v = c)

test <- as.POSIXct("2018-04-25 12:00:00")
h <- (1:46)*120*60
h <- h+test

plot_iks_R4 + abline(v = h, col = "red")

#On sépare par des lignes vert en fonction du jour et de la nuit

test <- as.POSIXct("2018-04-25 09:00:00")
d <- (1:46)*12*60*60
d <- d+test

plot_iks_R4 + abline(v = d, col = "green")
### Avec la fonction identify on sait mettre les bons points entre chaque cycle.

pos4 <- identify(EP2018_iks$Time, EP2018_iks[["O2_4_cor"]])

###On utilise maintenant la fonction respirometry pour voir à chaque cycle comment à évolué l'oxygène.

res_O2_4 <- respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n =1, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4")
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+1, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+2, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+3, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+4, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+5, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+6, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+7, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+8, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+9, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+10, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+11, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+12, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+13, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+14, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+15, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+16, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+17, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+18, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+19, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+20, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+21, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))
res_O2_4 <- rbind(res_O2_4, respirometry(EP2018_iks, "O2_4_cor", pos = pos4, n = 1+22, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R4"))

#On sépare le tableau de mesure du respiromètre 3 en fonction du jour et de la nuit

respi4_day1 = res_O2_4[c('1':'4'), ]
respi4_day2 = res_O2_4[c('11':'16'), ]
respi4_day3 = res_O2_4[c('23':'23'), ]
respi4_day <- rbind(respi4_day1,respi4_day2,respi4_day3)

respi4_night1 = res_O2_4[c('5':'10'), ]
respi4_night2 = res_O2_4[c('17':'22'), ]
respi4_night <- rbind(respi4_night1,respi4_night2)

#on fait la moyenne

respi4_mean_day <-mean(respi4_day$respi)
respi4_mean_night <- mean(respi4_night$respi)



#Respiromètre 5

iks_cor_5 <- filter(iks_cor, respiro == "R5")

###Ici on va corriger les valeurs enregistrées en 02 dans les respiromètres en fonction des mesures que l'on a prise avec l'oxymètre et l'iks

EP2018_iks$O2_5_cor <- correct_monitoring(EP2018_iks$Time, values = EP2018_iks$O2_5, calib.dates = iks_cor_5$date, calib.values = iks_cor_5$O2, extrapolate = TRUE)

plot(EP2018_iks$O2_5_cor)

###On va maintenant extraire les périodes qui nous interesse en coupant dans la série les moments ou le respiromètre est fermé et pour chaque période ajuster une droite et calculer la pente de la droite, on donne une série d'informations (volume du respiro, masse de la bouture,...)

###On repart du bon graphique, celui corrigé

plot_iks_R5 <- plot(EP2018_iks$Time, EP2018_iks[["O2_5_cor"]], type = "l", xlab = "Time",
                    ylab = "[O2] mg/L"); grid()

###On va placer dessus des lignes avec les points de début et de fin entre chaque cycle. On le fait deux fois pour bien altener quand cela s'ouvre et se ferme.

#On sépare en fonction de cycle O/F
test <- as.POSIXct("2018-04-25 12:00:00")
c <- (1:46)*60*60
c <- c+test

plot_iks_R5 + abline(v = c)

test <- as.POSIXct("2018-04-25 12:00:00")
h <- (1:46)*120*60
h <- h+test

plot_iks_R5 + abline(v = h, col = "red")

#On sépare par des lignes vert en fonction du jour et de la nuit

test <- as.POSIXct("2018-04-25 09:00:00")
d <- (1:46)*12*60*60
d <- d+test

plot_iks_R5 + abline(v = d, col = "green")
### Avec la fonction identify on sait mettre les bons points entre chaque cycle.

pos5 <- identify(EP2018_iks$Time, EP2018_iks[["O2_5_cor"]])

###On utilise maintenant la fonction respirometry pour voir à chaque cycle comment à évolué l'oxygène.

res_O2_5 <- respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n =1, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5")
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+1, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+2, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+3, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+4, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+5, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+6, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+7, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+8, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+9, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+10, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+11, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+12, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+13, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+14, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+15, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+16, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+17, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+18, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+19, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+20, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+21, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))
res_O2_5 <- rbind(res_O2_5, respirometry(EP2018_iks, "O2_5_cor", pos = pos5, n = 1+22, mass = 1.220, ref.respi = 0, vol.respi = 1.3, main = "R5"))

#On sépare le tableau de mesure du respiromètre 5 en fonction du jour et de la nuit

respi5_day1 = res_O2_5[c('1':'4'), ]
respi5_day2 = res_O2_5[c('11':'16'), ]
respi5_day3 = res_O2_5[c('23':'23'), ]
respi5_day <- rbind(respi5_day1,respi5_day2,respi5_day3)

respi5_night1 = res_O2_5[c('5':'10'), ]
respi5_night2 = res_O2_5[c('17':'22'), ]
respi5_night <- rbind(respi5_night1,respi5_night2)

#on fait la moyenne

respi5_mean_day <-mean(respi5_day$respi)
respi5_mean_night <- mean(respi5_night$respi)










#Respiromètre 6

iks_cor_6 <- filter(iks_cor, respiro == "R6")

###Ici on va corriger les valeurs enregistrées en 02 dans les respiromètres en fonction des mesures que l'on a prise avec l'oxymètre et l'iks

EP2018_iks$O2_6_cor <- correct_monitoring(EP2018_iks$Time, values = EP2018_iks$O2_6, calib.dates = iks_cor_6$date, calib.values = iks_cor_6$O2, extrapolate = TRUE)

plot(EP2018_iks$O2_6_cor)

###On va maintenant extraire les périodes qui nous interesse en coupant dans la série les moments ou le respiromètre est fermé et pour chaque période ajuster une droite et calculer la pente de la droite, on donne une série d'informations (volume du respiro, masse de la bouture,...)

###On repart du bon graphique, celui corrigé

plot_iks_R6 <- plot(EP2018_iks$Time, EP2018_iks[["O2_6_cor"]], type = "l", xlab = "Time",
                    ylab = "[O2] mg/L"); grid()

###On va placer dessus des lignes avec les points de début et de fin entre chaque cycle. On le fait deux fois pour bien altener quand cela s'ouvre et se ferme.

#On sépare en fonction de cycle O/F
test <- as.POSIXct("2018-04-25 12:00:00")
c <- (1:46)*60*60
c <- c+test

plot_iks_R6 + abline(v = c)

test <- as.POSIXct("2018-04-25 12:00:00")
h <- (1:46)*120*60
h <- h+test

plot_iks_R6 + abline(v = h, col = "red")

#On sépare par des lignes vert en fonction du jour et de la nuit

test <- as.POSIXct("2018-04-25 09:00:00")
d <- (1:46)*12*60*60
d <- d+test

plot_iks_R6 + abline(v = d, col = "green")
### Avec la fonction identify on sait mettre les bons points entre chaque cycle.

pos6 <- identify(EP2018_iks$Time, EP2018_iks[["O2_6_cor"]])

###On utilise maintenant la fonction respirometry pour voir à chaque cycle comment à évolué l'oxygène.

res_O2_6 <- respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n =1, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6")
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+1, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+2, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+3, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+4, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+5, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+6, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+7, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+8, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+9, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+10, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+11, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+12, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+13, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+14, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+15, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+16, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+17, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+18, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+19, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+20, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+21, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))
res_O2_6 <- rbind(res_O2_6, respirometry(EP2018_iks, "O2_6_cor", pos = pos6, n = 1+22, mass = 0.900, ref.respi = 0, vol.respi = 1.3, main = "R6"))

#On sépare le tableau de mesure du respiromètre 6 en fonction du jour et de la nuit

respi6_day1 = res_O2_6[c('1':'4'), ]
respi6_day2 = res_O2_6[c('11':'16'), ]
respi6_day3 = res_O2_6[c('23':'23'), ]
respi6_day <- rbind(respi6_day1,respi6_day2,respi6_day3)

respi6_night1 = res_O2_6[c('5':'10'), ]
respi6_night2 = res_O2_6[c('17':'22'), ]
respi6_night <- rbind(respi6_night1,respi6_night2)

#on fait la moyenne

respi6_mean_day <-mean(respi6_day$respi)
respi6_mean_night <- mean(respi6_night$respi)






#Respiromètre 7

iks_cor_7 <- filter(iks_cor, respiro == "R7")

###Ici on va corriger les valeurs enregistrées en 02 dans les respiromètres en fonction des mesures que l'on a prise avec l'oxymètre et l'iks

EP2018_iks$O2_7_cor <- correct_monitoring(EP2018_iks$Time, values = EP2018_iks$O2_7, calib.dates = iks_cor_7$date, calib.values = iks_cor_7$O2, extrapolate = TRUE)

plot(EP2018_iks$O2_7_cor)

###On va maintenant extraire les périodes qui nous interesse en coupant dans la série les moments ou le respiromètre est fermé et pour chaque période ajuster une droite et calculer la pente de la droite, on donne une série d'informations (volume du respiro, masse de la bouture,...)

###On repart du bon graphique, celui corrigé

plot_iks_R7 <- plot(EP2018_iks$Time, EP2018_iks[["O2_7_cor"]], type = "l", xlab = "Time",
                    ylab = "[O2] mg/L"); grid()

###On va placer dessus des lignes avec les points de début et de fin entre chaque cycle. On le fait deux fois pour bien altener quand cela s'ouvre et se ferme.

#On sépare en fonction de cycle O/F
test <- as.POSIXct("2018-04-25 12:00:00")
c <- (1:46)*60*60
c <- c+test

plot_iks_R7 + abline(v = c)

test <- as.POSIXct("2018-04-25 12:00:00")
h <- (1:46)*120*60
h <- h+test

plot_iks_R7 + abline(v = h, col = "red")

#On sépare par des lignes vert en fonction du jour et de la nuit

test <- as.POSIXct("2018-04-25 09:00:00")
d <- (1:46)*12*60*60
d <- d+test

plot_iks_R7 + abline(v = d, col = "green")
### Avec la fonction identify on sait mettre les bons points entre chaque cycle.

pos7 <- identify(EP2018_iks$Time, EP2018_iks[["O2_7_cor"]])

###On utilise maintenant la fonction respirometry pour voir à chaque cycle comment à évolué l'oxygène.

res_O2_7 <- respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n =1, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7")
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+1, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+2, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+3, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+4, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+5, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+6, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+7, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+8, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+9, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+10, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+11, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+12, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+13, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+14, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+15, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+16, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+17, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+18, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+19, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+20, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+21, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))
res_O2_7 <- rbind(res_O2_7, respirometry(EP2018_iks, "O2_7_cor", pos = pos7, n = 1+22, mass = 0.916, ref.respi = 0, vol.respi = 1.3, main = "R7"))

#On sépare le tableau de mesure du respiromètre 6 en fonction du jour et de la nuit

respi7_day1 = res_O2_7[c('1':'4'), ]
respi7_day2 = res_O2_7[c('11':'16'), ]
respi7_day3 = res_O2_7[c('23':'23'), ]
respi7_day <- rbind(respi7_day1,respi7_day2,respi7_day3)

respi7_night1 = res_O2_7[c('5':'10'), ]
respi7_night2 = res_O2_7[c('17':'22'), ]
respi7_night <- rbind(respi7_night1,respi7_night2)

#on fait la moyenne

respi7_mean_day <-mean(respi7_day$respi)
respi7_mean_night <- mean(respi7_night$respi)




#Respiromètre 8
#
iks_cor_8 <- filter(iks_cor, respiro == "R8")

###Ici on va corriger les valeurs enregistrées en 02 dans les respiromètres en fonction des mesures que l'on a prise avec l'oxymètre et l'iks

EP2018_iks$O2_8_cor <- correct_monitoring(EP2018_iks$Time, values = EP2018_iks$O2_8, calib.dates = iks_cor_8$date, calib.values = iks_cor_8$O2, extrapolate = TRUE)

plot(EP2018_iks$O2_8_cor)

###On va maintenant extraire les périodes qui nous interesse en coupant dans la série les moments ou le respiromètre est fermé et pour chaque période ajuster une droite et calculer la pente de la droite, on donne une série d'informations (volume du respiro, masse de la bouture,...)

###On repart du bon graphique, celui corrigé

plot_iks_R8 <- plot(EP2018_iks$Time, EP2018_iks[["O2_8_cor"]], type = "l", xlab = "Time",
                    ylab = "[O2] mg/L"); grid()

###On va placer dessus des lignes avec les points de début et de fin entre chaque cycle. On le fait deux fois pour bien altener quand cela s'ouvre et se ferme.

#On sépare en fonction de cycle O/F
test <- as.POSIXct("2018-04-25 12:00:00")
c <- (1:46)*60*60
c <- c+test

plot_iks_R8 + abline(v = c)

test <- as.POSIXct("2018-04-25 12:00:00")
h <- (1:46)*120*60
h <- h+test

plot_iks_R8 + abline(v = h, col = "red")

#On sépare par des lignes vert en fonction du jour et de la nuit

test <- as.POSIXct("2018-04-25 09:00:00")
d <- (1:46)*12*60*60
d <- d+test

plot_iks_R8 + abline(v = d, col = "green")
### Avec la fonction identify on sait mettre les bons points entre chaque cycle.

pos8 <- identify(EP2018_iks$Time, EP2018_iks[["O2_8_cor"]])

###On utilise maintenant la fonction respirometry pour voir à chaque cycle comment à évolué l'oxygène.

res_O2_8 <- respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n =1, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8")
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+1, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+2, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+3, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+4, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+5, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+6, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+7, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+8, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+9, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+10, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+11, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+12, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+13, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+14, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+15, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+16, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+17, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+18, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+19, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+20, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+21, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))
res_O2_8 <- rbind(res_O2_8, respirometry(EP2018_iks, "O2_8_cor", pos = pos8, n = 1+22, mass = 0, ref.respi = 0, vol.respi = 1.3, main = "R8"))

#On sépare le tableau de mesure du respiromètre 8 en fonction du jour et de la nuit

respi8_day1 = res_O2_8[c('1':'4'), ]
respi8_day2 = res_O2_8[c('11':'16'), ]
respi8_day3 = res_O2_8[c('23':'23'), ]
respi8_day <- rbind(respi8_day1,respi8_day2,respi8_day3)

respi8_night1 = res_O2_8[c('5':'10'), ]
respi8_night2 = res_O2_8[c('17':'22'), ]
respi8_night <- rbind(respi8_night1,respi8_night2)

#on fait la moyenne

respi8_mean_day <-mean(respi8_day$respi)
respi8_mean_night <- mean(respi8_night$respi)


#On fusionne tout
numero_respiro <- c("1","1","2","2","3","3","4","4","5","5","6","6","7","7","8","8")
periode <- c("jour","nuit","jour","nuit","jour","nuit","jour","nuit","jour","nuit","jour","nuit","jour","nuit","jour","nuit")
respi <- c(respi1_mean_day, respi1_mean_night,respi2_mean_day, respi2_mean_night,respi3_mean_day, respi3_mean_night,respi4_mean_day, respi4_mean_night,respi5_mean_day, respi5_mean_night,respi6_mean_day, respi6_mean_night,respi7_mean_day, respi7_mean_night,respi8_mean_day, respi8_mean_night)
respi_mean <-matrix(c(numero_respiro,periode,respi), ncol = 3)

colnames(respi_mean) <- c("numero_respiro","periode","respi")
show(respi_mean)
write.csv(respi_mean,file="respi_mean.csv")


