library(tidyverse)
library(Lahman)

franch_wins <- Teams %>%
  select(franchID, yearID, G, W, L) %>%
  group_by(franchID) %>%
  summarize(
    start = min(yearID),
    end = max(yearID),
    n = sum(G),
    w = sum(W),
    l = sum(L)
  ) %>%
  mutate(w_perc = w / n) %>%
  filter(n > 1000) %>%
  left_join(TeamsFranchises, by = "franchID") %>%
  select(franchID, franchName, start, end, n, w, l, w_perc) %>%
  arrange(desc(w_perc))
