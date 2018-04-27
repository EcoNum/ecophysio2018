# Import and combine all data set EcoNumData_weigh ####
# using package
library(econum)
library(tidyverse)


# code to combine several EcoNumData_weigh
direction <- "Data/ecophysio2018/weight/"

# create a list with all names
all_data <- dir(direction, full.names = TRUE)
# load data
repos_load(all_data[1])
growth <- EcoNumData_weight

# create a loop to combine all dataset
for(f in all_data[-1]){
  repos_load(f)
  growth1 <- EcoNumData_weight
  growth <- dplyr::bind_rows(growth, growth1)
  remove(growth1)
}
remove(all_data, direction, f, EcoNumData_weight)


growth$number_day <- as.numeric(round(difftime(growth$date,  growth$date[1], units = "day"),digits = 0))
growth$number_day <- as.numeric(growth$number_day)

growth%>%
  group_by(localisation, species, id)%>%
  mutate(growth_rate = ((((weight - weight[1])/weight[1]))))-> growth

growth%>%
  group_by(localisation, species, id)%>%
  mutate(growth_rate_by_date = (growth_rate/number_day)*100)-> growth

ggplot(growth) +
  geom_point(mapping = aes(x=number_day, y=growth_rate, color=as.factor(id)))






