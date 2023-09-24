elo_update <- function(r_a, r_b, s_a, k = 32){
  x1 <- r_a + k * (s_a - (1/(1 + 10**((r_b - r_a) / 400))))
  return(round(x1))
}
temp = elo_update(r_a = 1600, r_b = 1700, s_a = 1)
print(temp)
