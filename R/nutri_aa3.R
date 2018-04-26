SciViews::R
library(econum)
library(plotly)

repos_load(file = "Data/ecophysio2018/aa3/180425A-inorga_2018-04-25_13.48.23_5ADFC500_aa3.RData")

nutri <- EcoNumData_aa3.inorga
rm(EcoNumData_aa3.inorga)
samp <- filter(nutri, sample_type == "SAMP")

repos_load(file = "Data/ecophysio2018/aa3/180426A-inorga_2018-04-26_11.00.22_5AE11680_aa3.RData")

nutri <- EcoNumData_aa3.inorga
rm(EcoNumData_aa3.inorga)
samp1 <- filter(nutri, sample_type == "SAMP")

nutri <- bind_rows(samp, samp1)
rm(samp, samp1)

nutri$sample_date <- as.POSIXct(nutri$sample_date)
nutri <- separate(nutri, col = sample, into = c("localisation", "position", "cycle"), sep = "-",remove = FALSE)

# Premier graphique pour trouver les échantillons devant être réanalisé.
#
chart(data = nutri, formula = PO4_conc ~ sample_date %col=% localisation, auto.labs = FALSE) +
  geom_point()

ggplotly()

chart(data = nutri, formula = PO4_conc ~ sample_date %col=% localisation, auto.labs = FALSE) +
  geom_point()

ggplotly()

chart(data = nutri, formula = NH4_conc ~ sample_date %col=% localisation, auto.labs = FALSE) +
  geom_point()

ggplotly()

