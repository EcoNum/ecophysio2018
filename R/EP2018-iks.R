library(econum)

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

#Importation du tableau avec les mesures iks et les mesures avec l'oximètre pour faire la correction
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

###On va maintenant extraire les périodes qui nous interesse en coupant dans la série les moment ou le respiromètre est fermé et pour chaque période ajuster une droite et calculer la pente de la droite, on donne une série d'informations (volume du respiro, masse de la bouture,...)

###On repart du bon graphique, celui corrigé

plot_iks_R1 <- plot(EP2018_iks$Time, EP2018_iks[["O2_1_cor"]], type = "l", xlab = "Time",
  ylab = "[O2] mg/L"); grid()

###On va placer dessus des lignes avec les points de début et de fin entre chaque cycle. On le fait deux fois pour bien altener quand cela s'ouvre et se ferme.

test <- as.POSIXct("2018-04-25 12:00:00")
t <- (1:46)*60*60
t <- t+test

plot_iks_R1 + abline(v = t)

test <- as.POSIXct("2018-04-25 12:00:00")
t <- (1:46)*120*60
t <- t+test

plot_iks_R1 + abline(v = t, col = "red")

### Avec la fonction identify on sait mettre les bons points entre chaque cycle.

pos <- identify(EP2018_iks$Time, EP2018_iks[["O2_1_cor"]])

###On utilise maintenant la fonction respirometry pour voir à chaque cycle comment à évolué l'oxygène.

res_O2_1 <- respirometry(EP2018_iks, "O2_1_cor", pos = pos, n =1, mass = 0.988, ref.respi = 0, vol.respi = 1.3, main = "R1")

res_O2_1

#On fait un tableau avec rbind pour avoir un tableau général des données (res_O2_1, res_O2_2,...)
