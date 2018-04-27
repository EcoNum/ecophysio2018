
# Package
library(econum)
library(plotly)
library(tidyverse)

#importation des données de nutriments 180425A

repos_load(file = "Data/ecophysio2018/aa3/180425A-inorga_2018-04-25_13.48.23_5ADFC500_aa3.RData")

nutri <- EcoNumData_aa3.inorga
nutri <- filter(nutri, sample_type == "SAMP")

## Correction erreurs d'encodage
nutri$sample_date[nutri$sample_date == "2018-04-25 09:00:01"] <- "2018-04-25 09:00:00"
nutri$sample_date[nutri$sample_date == "2018-04-25 09:00:02"] <- "2018-04-25 09:00:00"
nutri$sample_date[nutri$sample_date == "2018-04-25 09:00:03"] <- "2018-04-25 09:00:00"
nutri$sample_date[nutri$sample_date == "2018-04-25 09:00:04"] <- "2018-04-25 09:00:00"
nutri$sample_date[nutri$sample_date == "2018-04-25 10:00:00"] <- "2018-04-25 12:00:00"


nutri$sample_date <- as.POSIXct(nutri$sample_date)
ggplot(data = nutri, mapping = aes( x = sample_date, y = NO3_conc )) +
  geom_point()

#importation des données de nutriments 180426A

repos_load(file = "Data/ecophysio2018/aa3/180426A-inorga_2018-04-26_11.00.22_5AE11680_aa3.RData")

nutri1 <- EcoNumData_aa3.inorga
nutri1 <- filter(nutri1, sample_type == "SAMP")

nutri1$sample_date <- as.POSIXct(nutri1$sample_date)
ggplot(data = nutri1, mapping = aes( x = sample_date, y = NO3_conc )) +
  geom_point()



repos_load(file = "Data/ecophysio2018/aa3/180426CR1-inorga_2018-04-26_15.35.14_5AE11680_aa3.RData")


nutri2 <- EcoNumData_aa3.inorga
nutri2 <- filter(nutri2, sample_type == "SAMP")

nutri2$sample_date <- as.POSIXct(nutri2$sample_date)
ggplot(data = nutri2, mapping = aes( x = sample_date, y = NO3_conc)) +
  geom_point()


repos_load(file = "Data/ecophysio2018/aa3/180427A-inorga_2018-04-27_13.50.08_5AE26800_aa3.RData")


nutri3 <- EcoNumData_aa3.inorga
nutri3 <- filter(nutri3, sample_type == "SAMP")
nutri3 <- filter(nutri3, project != "NA")

nutri3$sample_date <- as.POSIXct(nutri3$sample_date)
ggplot(data = nutri3, mapping = aes( x = sample_date, y = NO3_conc)) +
  geom_point()


nutri <- bind_rows(nutri, nutri1, nutri2, nutri3 )
rm(nutri1, nutri2, nutri3, EcoNumData_aa3.inorga)

ggplot(data = nutri, mapping = aes( x = sample_date, y = NO3_conc)) +
  geom_point(na.rm = TRUE)

## Visualisation avec les respiro

nutri <- separate(nutri, col = sample, into = c("loca", "position", "cycle"), remove = FALSE)

ggplot(data = nutri, mapping = aes( x = sample_date, y = NO3_conc, color = loca)) +
  geom_point(na.rm = TRUE)+
  geom_line(na.rm = TRUE)

ggplot(data = nutri, mapping = aes( x = sample_date, y = PO4_conc, color = loca)) +
  geom_point(na.rm = TRUE)+
  geom_line(na.rm = TRUE)

ggplot(data = nutri, mapping = aes( x = sample_date, y = NH4_conc, color = loca)) +
  geom_point(na.rm = TRUE)+
  geom_line(na.rm = TRUE)

nutri %>%
  mutate( expe = case_when(loca == "A0" ~ "A0",
                           loca =="R1" ~ "hl",
                           loca == "R2" ~ "hl",
                           loca == "R3" ~ "hl",
                           loca == "R4" ~ "hl",
                           loca =="R5" ~ "ll",
                           loca == "R6" ~ "ll",
                           loca == "R7" ~ "ll",
                           loca == "R8" ~ "ll")) -> nutri

nutri <- filter(nutri, loca != "NA")

ggplot(data = nutri, mapping = aes( x = sample_date, y = NO3_conc, color = loca)) +
  geom_point(na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  facet_wrap( ~ expe, ncol = 3) +
  theme_bw()

ggplot(data = filter(nutri, PO4_conc <= 4), mapping = aes( x = sample_date, y = PO4_conc, color = loca)) +
  geom_point(na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  facet_wrap( ~ expe, ncol = 3) +
  theme_bw()

ggplot(data = nutri, mapping = aes( x = sample_date, y = NH4_conc, color = loca)) +
  geom_point(na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  facet_wrap( ~ expe, ncol = 3) +
  theme_bw()
