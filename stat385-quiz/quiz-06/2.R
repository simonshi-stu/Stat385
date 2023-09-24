library(dplyr)
sw_df <- starwars %>% filter(homeworld=='Tatooine' | homeworld== 'Corellia')
