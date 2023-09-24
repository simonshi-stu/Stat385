next_day_weather = function(current) {
  if (current <= 70) {
    return(current + 3)
  } else if (current <= 72) {
    return(current - 1)
  } else if (current <= 74) {
    return(current + 1)
  } else if (current <= 76) {
    return(current + 1)
  } else if (current <= 78) {
    return(current + 3)
  } else if (current <= 80) {
    return(current - 5)
  } else {
    return(current - 2)
  }
}
generate_forecast = function(first_day, num_days) {
  forecast = double(length = num_days)
  forecast[1] = first_day
  for (day in 2:num_days) {
    forecast[day] = next_day_weather(forecast[day - 1])
  }
  return(forecast)
}
typeof(as.integer("abcd"))



dist_cat = function(x) {
  if (x <= 2) {
    return(c("adjacent"))
  } else if (x > 2 && x <= 4) {
    return(c("close"))
  } else {
    return(c("far"))
  }
}
dist_from = function(x, from) {
  x = x
  dist = abs(x - from)
  cat = c()
  for (i in 1:length(dist)) {
    cat = append(cat, dist_cat(dist[i]))
  }
  b = data.frame(x, dist, cat)
}


some_df = data.frame(n = rep(232, times = 37), g = rep("STAT 385", times = 37),
                     m = c(1:37))
some_df
calc_olympic_score = function(judges_scores) {
  name = sort(judges_scores)
  min = min(name)
  max = max(name)
  count = 0
  count1 = 0
  for (i in 1:length(name)) {
    if (name[i] == min && count == 0) {
      count = count + 1
      name = name[-i]
    }
    if (name[i] == max && count1 == 0) {
      count1 = count1 + 1
      name = name[-i]
    }
  }
  return(mean(name))
}


even_odd_rows = function(x, odd) {
  if (odd) {
    i = c(1:nrow(ex_df))
    i = which(i %% 2 != 0)
    return(x[i,])
  } else {
    i = c(1:nrow(ex_df))
    i = which(i %% 2 == 0)
    return(x[i,])
  }
}

ex_df = data.frame(
  x = 1:10,
  y = 10:1
)
even_odd_rows(ex_df, FALSE)
i = c(1:nrow(ex_df))
i = which(i %% 2 != 0)

some_list = list(o = c(42), b = c("Exams are fun!"), l = c(9:352))

fractions = c(1/(1:133))
fractions




only_lists = function(x) {
  c = list()
  for (i in 1:length(x)) {
    if (is.list(x[[i]])) {
      c = append(c, x[i])
    }
  }
  return(c)
}

ex_list = list(
  a = 1:3,
  b = list(x = 1:3),
  c = 42,
  d = list(y = 2:4)
)
length(ex_list[[2]])
is.list(ex_list[[1]])
ex_list[2]
only_lists(x = ex_list)


selection_sort = function(x) {
  y = vector(mode = "numeric", length = length(x))
  for (i in 1:length(x)) {
    value = min(x)
    index = which(x == value)[1]
    y[i] = value
    x = x[-index]
  }
  return(y)
}

selection_sort(x = c(2, 2, 1, 1, 4, 6, 8, 2, 10))
x = c(2, 2, 1, 1, 4, 6, 8, 2, 10)
y = vector(mode = "numeric", length = length(x))
typeof(y)


split_int = function(x) {
  str = as.character(x)
  str = strsplit(str, "")
  str = as.integer(str[[1]])
  return(str)
}
is_valid = function(cc) {
  str = split_int(cc)
  last = str[length(str)]
  str = str[-length(str)]
  count = 0
  for (i in length(str):1) {
    if (count == 0) {
      count = count + 1
      str[i] = 2*str[i]
    } else {
      count = 0
    }
  }
  str = lapply(str, split_int)
  str = sapply(str, sum)
  add = sum(str)
  if (10-(add %% 10) == last) {
    return(TRUE)
  }
  return(FALSE)
}


is_valid(49189)


?index
?lapply
?sapply






is_even = function(x) {
  if (x %% 2 == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

juggler_next = function(x) {
  if (is_even(x) == TRUE) {
    x = floor(x**(1/2))
    return(x)
  } else {
    x = floor(x**(3/2))
    return(x)
  }
}

juggler_seq = function(a0, n) {
  c = c()
  c = append(c, a0)
  for (i in 2:(n+1)) {
    a0 = juggler_next(a0)
    c = append(c, a0)
  }
  return(c)
}
juggler_trim = function(x) {
  count = 0
  d = x
  c = c()
  for (i in 1:length(x)) {
    if (x[i] == 1) {
      if (count == 0) {
        count = count + 1
        next
      } else {
        c = append(c, i)
      }
    }
  }
  d = d[-c]
  return(d)
}

estimate_pi = function(xs, ys) {
  times = 4* (sum(sqrt(xs**2 + ys**2) <= 1)/ length(xs))
  return(times)
}
c=c(3, 5, 11, 36, 6, 2, 1, 1, 1, 1, 1)

juggler_trim(c)




estimate_pi = function(xs, ys) {
  pi = 4*((sqrt(xs**2 + ys**2) <= 1) / length(xs))
  return(sum(pi))
}





set.seed(42)
x = runif(n = 1000)
y = runif(n = 1000)
estimate_pi(xs = x, ys = y)
























