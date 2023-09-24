library(tidyverse)
library(nycflights13)

la_aircraft <- flights %>%
  filter(dest %in% c("BUR", "LAX", "LGB", "ONT", "SNA", "VNY")) %>%
  left_join(planes, by = "tailnum") %>%
  group_by(manufacturer) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
