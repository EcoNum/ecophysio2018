SciViews::R
library(econum)
library(plotly)

repos_load(file = "Data/ecophysio2018/aa3/180425A-inorga_2018-04-25_13.48.23_5ADFC500_aa3.RData")

nutri <- EcoNumData_aa3.inorga
rm(EcoNumData_aa3.inorga)
nutri <- filter(nutri, sample_type == "SAMP")

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

