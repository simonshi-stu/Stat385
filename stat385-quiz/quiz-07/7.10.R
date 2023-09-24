library(tidyverse)

who_longer <- who %>%
  pivot_longer(col = 5:ncol(who), names_to = "name", values_to = "data", values_drop_na = TRUE) %>%
  group_by(country, year) %>%
  summarise(cases = sum(data)) %>%
  select(country, year, cases) %>%
  arrange(country, year)

