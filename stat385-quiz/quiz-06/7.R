library(dplyr)
sw_df = mutate(starwars, height = height / 2.54)
print(sw_df)
