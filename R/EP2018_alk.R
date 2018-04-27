library(econum)
library(tidyverse)

direction <- "Data/ecophysio2018/AT/"

# create a list with all names
all_data <- dir(direction, full.names = TRUE)

load(all_data[1])
EcoNumData_AT -> alk

attr(x = alk, which = "metadata")$sample -> t1
attr(x = alk, which = "metadata")$sampledate -> t2
attr(x = alk, which = "Dickson2007")$AT__ui -> t3

alk <- data.frame(sample = c(t1), sampledate = c(t2), alk = c(t3))
remove(EcoNumData_AT, t1, t2, t3)

for (f in all_data[-1]) {
  #print(f)
  load(f)
  EcoNumData_AT -> J1
  attr(x = J1, which = "metadata")$sample -> t1
  attr(x = J1, which = "metadata")$sampledate -> t2
  attr(x = J1, which = "Dickson2007")$AT__ui -> t3
  J1 <- data.frame(sample = c(t1), sampledate = c(t2), alk = c(t3))
  alk <- dplyr::bind_rows(alk, J1)
  remove(J1, EcoNumData_AT, t1, t2, t3)
}

remove(all_data, f)

alk <- separate(data = alk, col = sample, into = c("cycle", "loca"), sep = "-", remove = FALSE)

alk$loca[alk$loca =="01"] <- "R1"
alk$loca[alk$loca =="02"] <- "R2"
alk$loca[alk$loca =="03"] <- "R3"
alk$loca[alk$loca =="04"] <- "R4"
alk$loca[alk$loca =="05"] <- "R5"
alk$loca[alk$loca =="06"] <- "R6"
alk$loca[alk$loca =="07"] <- "R7"
alk$loca[alk$loca =="07 "] <- "R7"
alk$loca[alk$loca =="08"] <- "R8"

alk %>%
  mutate( expe = case_when(loca == "A0" ~ "A0",
                           loca =="R1" ~ "hl",
                           loca == "R2" ~ "hl",
                           loca == "R3" ~ "hl",
                           loca == "R4" ~ "hl",
                           loca =="R5" ~ "ll",
                           loca == "R6" ~ "ll",
                           loca == "R7" ~ "ll",
                           loca == "R8" ~ "ll")) -> alk





alk %>%
  filter(cycle == "11") %>%
  mutate(sampledate = sampledate + 60*60) -> alk1
alk <- filter(alk, cycle != "11")

alk <- bind_rows(alk, alk1)

ggplot(data = filter(alk, expe != "A0"), mapping = aes( x = sampledate, y = alk, color = loca)) +
  geom_point(na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  facet_wrap( ~ expe, ncol = 2) +
  theme_bw()

