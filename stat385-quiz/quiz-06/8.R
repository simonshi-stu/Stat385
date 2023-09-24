library(dplyr)
sw_df = mutate(starwars, mass = mass * 2.204623)
print(sw_df)
