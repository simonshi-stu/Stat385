library(tidyverse)
ifb <- read_csv('https://raw.githubusercontent.com/wadefagen/datasets/master/illini-football/illini-football-scores.csv')
write_csv(ifb, file = 'data/football.csv')