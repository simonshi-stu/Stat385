library(tidyverse)
library(nycflights13)

nyc_to_dest_delay <- flights %>%
  group_by(dest) %>%
  summarize(
    med_delay = median(dep_delay + arr_delay)
  ) %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  select(faa_code = dest, port_name = name, med_delay) %>%
  arrange(desc(med_delay))
