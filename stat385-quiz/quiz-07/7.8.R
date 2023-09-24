library(tidyverse)
library(Lahman)

allstar_appearances <- AllstarFull %>%
  group_by(playerID) %>%
  filter(n() >= 1) %>%
  summarize(appearances = n()) %>%
  left_join(People, by = "playerID") %>%
  select(playerID, nameFirst, nameLast, appearances) %>%
  arrange(desc(appearances))


