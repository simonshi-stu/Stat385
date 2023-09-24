library(tidyverse)
library(Lahman)

postseason_games <- BattingPost %>%
  group_by(playerID) %>%
  summarize(n = sum(G)) %>%
  left_join(People, by = "playerID") %>%
  select(playerID, nameFirst, nameLast, n) %>%
  arrange(desc(n))
