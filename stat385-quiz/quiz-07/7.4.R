library(tidyverse)
library(Lahman)

team_payroll <- Salaries %>%
  group_by(teamID, yearID) %>%
  summarize(payroll = sum(salary)) %>%
  left_join(Teams, by = c("teamID", "yearID")) %>%
  select(yearID, teamID, name, payroll) %>%
  arrange(desc(payroll))

