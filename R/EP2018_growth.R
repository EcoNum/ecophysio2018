#On commence par importer toutes les données du monitoring du poids au format du labo pour les mettre sous forme d'un bon tableau pour travailler avec

library(econum)
library(tidyverse)


direction <- "Data/ecophysio2018/weight/"

all_data <- dir(direction, full.names = TRUE)

repos_load(all_data[1])
growth <- EcoNumData_weight

for(f in all_data[-1]){
  repos_load(f)
  growth1 <- EcoNumData_weight
  growth <- dplyr::bind_rows(growth, growth1)
  remove(growth1)
}
remove(all_data, direction, f, EcoNumData_weight)

#On rajoute une colonne pour savoir on est à combien de jour de monitoring

growth$number_day <- as.numeric(round(difftime(growth$date,  growth$date[1], units = "day"),digits = 0))
growth$number_day <- as.numeric(growth$number_day)

#On rajoute une colonne qui nous donne le ratio de croissance des boutures en pourcentage par rapport à la première pesée

growth%>%
  group_by(localisation, species, id)%>%
  mutate(growth_rate = ((((weight - weight[1])/weight[1]))))-> growth

#On rajoute une colonne qui nous donne le taux de croissance par jour

growth%>%
  group_by(localisation, species, id)%>%
  mutate(growth_rate_by_date = (growth_rate/number_day)*100)-> growth

#On modifie juste la colonne growth_rate pour l'avoir en pourcentage plus clair

growth[9] <- growth[9]*100

#On renomme le jeux de données pour le mettre à un format plus clair d'utilisation et on nettoie l'environment

ep2018_growth <- growth

rm(growth)

#Graphique général du pourcentage de la croissance de nos boutures, en fonction du nombre de jours après la première pesée

ggplot(ep2018_growth, aes(x = number_day, y = growth_rate, color = as.factor(id))) + geom_point() + geom_line ()

# On peut rajouter une colonne avec le numéro du respiro
ep2018_growth$respiro <- as.character(gep2018_growth$id)
ep2018_growth$respiro[ep2018_growth$id == "1"] <- "A"
ep2018_growth$respiro[ep2018_growth$id == "2"] <- "A"
ep2018_growth$respiro[ep2018_growth$id == "3"] <- "M"
ep2018_growth$respiro[ep2018_growth$id == "4"] <- "M"
ep2018_growth$respiro[ep2018_growth$id == "5"] <- "M"
ep2018_growth$respiro[ep2018_growth$id == "6"] <- "M"
ep2018_growth$respiro[ep2018_growth$id == "7"] <- "M"
ep2018_growth$respiro[ep2018_growth$id == "8"] <- "M"
ep2018_growth$respiro[ep2018_growth$id == "9"] <- "M"
ep2018_growth$respiro[ep2018_growth$id == "10"] <- "A"
ep2018_growth$respiro[ep2018_growth$id == "11"] <- "M"
ep2018_growth$respiro[ep2018_growth$id == "12"] <- "M"
ep2018_growth$respiro[ep2018_growth$id == "13"] <- "B"
ep2018_growth$respiro[ep2018_growth$id == "14"] <- "M"
ep2018_growth$respiro[ep2018_growth$id == "15"] <- "M"
ep2018_growth$respiro[ep2018_growth$id == "16"] <- "B"
ep2018_growth$respiro[ep2018_growth$id == "17"] <- "M"
ep2018_growth$respiro[ep2018_growth$id == "18"] <- "M"
ep2018_growth$respiro[ep2018_growth$id == "19"] <- "B"
ep2018_growth$respiro[ep2018_growth$id == "20"] <- "M"

##On filtre le tableau pour garder uniquement les boutures ayant été en respirométrie

growth_RA<- filter(growth, respiro == "A")
growth_RB<- filter(growth, respiro == "B")

#On refusionne les deux jeux de données respiro A et B pour ainsi faire un seul graphique comparatif

growth_R <- rbind(growth_RA, growth_RB)

#On peut maintenant faire un graphique comparatif

ggplot(growth_R, aes(x = number_day, y = growth_rate_by_date, fill=as.factor(id),color = respiro)) + geom_point() + geom_line () + geom_vline(xintercept = c(8,10), color = "blue", size=0.2)

#On peut aussi faire un graphique qui compare l'évolution des boutures dans le mésocosme A0 avec celles dans le respiro A et celles dans le respiro B

ggplot(growth, aes(x = number_day, y = growth_rate,color = as.factor(id))) + geom_point() + geom_line () + geom_vline(xintercept = c(8,10), color = "blue", size=0.2)+ xlab("Nombre de jour") + ylab("taux de croissance") +
  facet_grid( ~ respiro) + ggtitle("Mesure de la croissance des boutures") + theme(legend.position = "none")

ggplot(growth, aes(x = number_day, y = growth_rate_by_date,color = as.factor(id))) + geom_point() + geom_line () + geom_vline(xintercept = c(8,10), color = "blue", size=0.2)+ xlab("Nombre de jour") + ylab("taux de croissance") +
  facet_grid( ~ respiro) + ggtitle("Mesure de la croissance des boutures") + theme(legend.position = "none")
