library(dplyr)
sw_df = count(starwars, species)
print(sw_df, n = nrow(sw_df))
