
#importation des données de nutriments
date[EcoNumData_aa3.inorga$sample_date == "2018-04-25 09:00:04"] <- "2018-04-25 09:00:00"
library(econum)
library(plotly)
library(tidyverse)
repos_load(file = "Data/ecophysio2018/aa3/180425A-inorga_2018-04-25_13.48.23_5ADFC500_aa3.RData")

#élimination des doublons et erreur

## Recoding EcoNumData_aa3.inorga$sample_date into EcoNumData_aa3.inorga$sample_date_rec
EcoNumData_aa3.inorga$sample_date <- EcoNumData_aa3.inorga$sample_date
EcoNumData_aa3.inorga$sample_date[EcoNumData_aa3.inorga$sample_date == "2018-04-25 09:00:01"] <- "2018-04-25 09:00:00"
EcoNumData_aa3.inorga$sample_date[EcoNumData_aa3.inorga$sample_date == "2018-04-25 09:00:02"] <- "2018-04-25 09:00:00"
EcoNumData_aa3.inorga$sample_date[EcoNumData_aa3.inorga$sample_date == "2018-04-25 09:00:03"] <- "2018-04-25 09:00:00"
EcoNumData_aa3.inorga$sample_
EcoNumData_aa3.inorga$sample_date <- EcoNumData_aa3.inorga$sample_date
EcoNumData_aa3.inorga$sample_date[EcoNumData_aa3.inorga$sample_date == "2018-04-25 09:00:04"] <- "2018-04-25 09:00:00"
## Recoding EcoNumData_aa3.inorga$sample_date into EcoNumData_aa3.inorga$sample_date_rec
EcoNumData_aa3.inorga$sample_date_rec <- EcoNumData_aa3.inorga$sample_date
EcoNumData_aa3.inorga$sample_date_rec[EcoNumData_aa3.inorga$sample_date == "2018-04-25 10:00:00"] <- "2018-04-25 12:00:00"




#On renomme le jeux de données en nutri

nutri <- EcoNumData_aa3.inorga

#on retire l'ancien de l'environnement
rm(EcoNumData_aa3.inorga)

#On filtre pour garder ce qui nous interesse

samp <- filter(nutri, sample_type == "SAMP")

#On refait la même chose pour les données de nutriments du 26

repos_load(file = "Data/ecophysio2018/aa3/180426A-inorga_2018-04-26_11.00.22_5AE11680_aa3.RData")

nutri <- EcoNumData_aa3.inorga
rm(EcoNumData_aa3.inorga)
samp1 <- filter(nutri, sample_type == "SAMP")

#on fusionne les données de nutriments du 26/04 et du 25/04 dans un seul jeux de données que l'on appelle nutri

nutri <- bind_rows(samp, samp1)

#On supprime de l'environnement les jeux de données inutiles

rm(samp, samp1)


#Fonction??
nutri$sample_date <- as.POSIXct(nutri$sample_date)

#On sépare la colonne sample pour avoir ainsi juste la localisation, la position et le cycle
nutri<-separate(nutri, col = sample, into = c("localisation", "position", "cycle"), sep = "-",remove = FALSE)

#On réalise un premier graphique qui permet de voir quels échantillons ont besoin d'être réanalysé

ggplot(data = nutri, mapping = aes(x = sample_date, y = PO4_conc, color = localisation))+
  geom_point()

#Avec la fonction ggplotly le graphique devient interactif
ggplotly()

#A suivre

chart(data = nutri, formula = PO4_conc ~ sample_date %col=% localisation, auto.labs = FALSE) +
  geom_point()

ggplotly()

chart(data = nutri, formula = NH4_conc ~ sample_date %col=% localisation, auto.labs = FALSE) +
  geom_point()

ggplotly()

