rock_paper_scissors <- function(shapes = c("rock", "paper", "scissors"),
                               n_players = 2){
  return(sample(x = shapes, size = n_players, replace = TRUE))
}
print(rock_paper_scissors())

# playing rock-paper-scissors-lizard-spock with two players
print(rock_paper_scissors(shapes = c("rock", "paper", "scissors", "lizard", "spock")))

# playing rock-paper-scissors with three players
print(rock_paper_scissors(n_players = 3))

# using frog-slug-snake with Japanese names and three players
print(rock_paper_scissors(shapes = c("namekuji", "kawazu", "hebi"), n_players = 3))


print(log(1))
print(log2(2))
print(log(2, base = 2))
g = function(x, y =1){
  x - y
}
print(g(2))
a = c(2, 10, 15)
print(a)

set.seed(42)
random_numbers = rexp(n = 1000, rate = 0.3)
print(random_numbers)
