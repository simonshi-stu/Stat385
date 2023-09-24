count_and_prop_greater = function(x, c) {
  comparison = x > c
  c(count = sum(comparison), prop = mean(comparison))
}
count_and_prop_less = function(x, c) {
  comparison = x < c
  c(count = sum(comparison), prop = mean(comparison))
}
count_and_prop_equal = function(x, c) {
  comparison = x == c
  c(count = sum(comparison), prop = mean(comparison))
}
x = c(1,2,3)
c = c(2)
temp = count_and_prop_equal(x, c)
print(temp)
