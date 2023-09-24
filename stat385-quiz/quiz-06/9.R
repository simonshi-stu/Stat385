library(dplyr)
sw_df = count(starwars, homeworld)
print(sw_df, n = nrow(sw_df))
