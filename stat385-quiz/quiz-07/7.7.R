library(tidyverse)
library(Lahman)

career_salaries <- Salaries %>%
  group_by(playerID) %>%
  summarize(earnings = sum(salary)) %>%
  left_join(People, by = "playerID") %>%
  select(playerID, nameFirst, nameLast, earnings) %>%
  arrange(desc(earnings))
