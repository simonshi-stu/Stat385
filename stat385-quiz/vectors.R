bar = seq(from = 0, to = 100, by = 5)
foo = seq(from = -10L, to = 10L, by = 1L)
as.integer(foo)
print(typeof(foo))
baz <- rep(c("a", "b", "c"), times = 10)
powers_of_foo = 10**foo
mean_of_bar = mean(bar)
count_of_baz = table(baz)
print(count_of_baz)

      