#On va regarder la croissance pour savoir quelles boutures sélectionner pour le respiro.
#On utilise une fonction qui calcule le pourcentage du taux de croissance en faisant la dernière mesure divisée par la première.

#Importation du jeux de données du respiro
library(readxl)
ecophysio_respiro <- read_excel("/media/sf_Shared/Projects/ecophysio2018/Data/ecophysio_respiro.xlsx")
View(ecophysio_respiro)

#Changement des unités

attr(ecophysio_respiro$O2,"units") <- "mg/l"
attr(ecophysio_respiro$O2_IKS,"units") <- "mg/l"
attr(ecophysio_respiro$S,"units") <- "PSU"
attr(ecophysio_respiro$T,"units") <- "°C"
attr(ecophysio_respiro$pH,"units") <- ""

ecophysio_respiro$O2 <- as.numeric(ecophysio_respiro$O2)
ecophysio_respiro$O2_IKS <- as.numeric(ecophysio_respiro$O2_IKS)
ecophysio_respiro$S <- as.numeric(ecophysio_respiro$S)
ecophysio_respiro$T <- as.numeric(ecophysio_respiro$T)
ecophysio_respiro$pH <- as.numeric(ecophysio_respiro$pH)

#separation de la colonne sample pour avoir les différents respiro

separate(ecophysio_respiro, col = sample, into = c("respiro", "O/F","date2"), sep = "-")-> ecophysio_respiro2

# graphique de l'évolution des paramètres

plot_O2 <- ggplot(data=ecophysio_respiro2, aes(x=date2, y= O2, colour = respiro, group = respiro)) +
  geom_line() +
  geom_point()
plot_O2 + scale_fill_manual(values=c('#A5260A','#D1B606','#048B9A','#6B5731', '#708D23','#D473D4','#E6E697','#C4698F','#A91101')) + scale_x_discrete(labels=c("9h", "10h", "11h","12h","13h","14h","15h","16h"))+ xlab("Heure") + ylab("Oxygène")

plot_S <- ggplot(data=ecophysio_respiro2, aes(x=date2, y= S, colour = respiro, group = respiro)) +
  geom_line() +
  geom_point()
plot_S + scale_fill_manual(values=c('#A5260A','#D1B606','#048B9A','#6B5731', '#708D23','#D473D4','#E6E697','#C4698F','#A91101')) + scale_x_discrete(labels=c("9h", "10h", "11h","12h","13h","14h","15h","16h"))+ xlab("Heure") + ylab("Salinité")

plot_T <- ggplot(data=ecophysio_respiro2, aes(x=date2, y= T, colour = respiro, group = respiro)) +
  geom_line() +
  geom_point()
plot_T + scale_fill_manual(values=c('#A5260A','#D1B606','#048B9A','#6B5731', '#708D23','#D473D4','#E6E697','#C4698F','#A91101')) + scale_x_discrete(labels=c("9h", "10h", "11h","12h","13h","14h","15h","16h"))+ xlab("Heure") + ylab("Température")

plot_pH <- ggplot(data=ecophysio_respiro2, aes(x=date2, y= pH, colour = respiro, group = respiro)) +
  geom_line() +
  geom_point()
plot_pH + scale_fill_manual(values=c('#A5260A','#D1B606','#048B9A','#6B5731', '#708D23','#D473D4','#E6E697','#C4698F','#A91101')) + scale_x_discrete(labels=c("9h", "10h", "11h","12h","13h","14h","15h","16h"))+ xlab("Heure") + ylab("pH")

