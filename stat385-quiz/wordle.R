check_wordle = function(guess, truth) {
  guess = strsplit(guess, split = "")[[1]]
  truth = strsplit(truth, split = "")[[1]]
  result = ifelse(guess %in% truth, "yellow", "grey")
  result[guess == truth] = "green"
  names(result) = guess
  return(result)
}

x = 0
i = 0
while(x >= 0) {
  x = x + 2
  if (x > 100) {
    x = -x
  }
  i = i + 1
}
print(i)


between = function(x, a, b) {
  r = c()
  for (i in x) {
    if(i < b && i > a) {
      r = append(r, i)
    }
  }
  return(r)
}


outside = function(x, a, b) {
  r = c()
  for(i in x) {
    if(i > b || i < a) {
      r = append(r, i)
    }
  }
  return(r)
}


fib_seq = function(length) {
  fibo = numeric(length)
  fibo[1] = 1L
  fibo[2] = 1L
  for (i in 3:length) {
    fibo[i] = fibo[i-1] + fibo[i-2]
  }
  fibo = as.integer(fibo)
  return(fibo)
}


solve_quadratic = function(a,b,c) {
  diff = b**2 - 4*a*c
  if (diff == 0) {
    return(-b/(2*a))
  } else if(diff > 0) {
    root1 = (-b - sqrt(diff)) / (2*a)
    root2 = (-b + sqrt(diff)) / (2*a)
    return(c(root1, root2))
  } else {
    root1 = complex(real = -b/(2*a), imaginary = -sqrt(-1*diff)/(2*a))
    root2 = complex(real = -b/(2*a), imaginary = sqrt(-1*diff)/(2*a))
    return(c(root1, root2))
  }
}


check_vec_total = function(x) {
  total = sum(x,na.rm = TRUE)
  if (total <= 13) {
    return(c("small total"))
  } else if (total > 13 && total <= 26) {
    return(c("medium total"))
  } else if (total > 26) {
    return(c("large total"))
  }
}


count_collatz_steps = function(n) {
  count = 0
  while (n != 1) {
    if (n %% 2 == 0) {
      n = n / 2
    } else {
      n = 3*n + 1
    }
    count = count + 1
  }
  return(count)
}
n = 9
print(count_collatz_steps(n))


is_prime = function(x) {
  if(x <= 1) {
    return(FALSE)
  } else if(x == 2) {
    return(TRUE)
  } else {
    if (0 %in% (x %% 2:ceiling(sqrt(x)))) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}


find_primes = function(x){
  primes = Filter(is_prime, x)
  return(primes)
}
print(15 %% 2:ceiling(sqrt(13)))
print(find_primes(x = 1:10))


assign_letter_grade = function(x, type = "decimal") {
  grades = data.frame(percent = c(90, 80, 70, 60, 0),
                       decimal = c(0.9, 0.8, 0.7, 0.6, 0),
                       letter = c("A", "B", "C", "D", "F"))
  if (type == "decimal") {
    cutoffs = grades$decimal
  } else if (type == "percent") {
    cutoffs = grades$percent
  }
  
  if (x >= cutoffs[1]) {
    return(grades$letter[1])
  } else if (x >= cutoffs[2]) {
    return(grades$letter[2])
  } else if (x >= cutoffs[3]) {
    return(grades$letter[3])
  } else if (x >= cutoffs[4]) {
    return(grades$letter[4])
  } else {
    return(grades$letter[5])
  }
}


create_gradebook = function(x, type = "decimal") {
  if (type == "decimal") {
    gradebook = data.frame(decimal = x,
                           percent = x * 100)
  } else {
    gradebook = data.frame(decimal = x / 100,
                           percent = x)
  }

  
  gradebook$letter = sapply(x, assign_letter_grade, type = type)
  
  return(gradebook)
}

print(create_gradebook(x = c(75, 85, 95), type = "percent"))


dist_cat = function(x) {
  if (x <= 2) {
    return(c("adjacent"))
  } else if (x > 2 && x <= 4) {
    return(c("close"))
  } else if (x > 4) {
    return(c("far"))
  }
}

dist_from = function(x, from) {
  x = x
  dist = abs(x - from)
  cat = c()
  for (i in 1:length(dist)) {
    cat = c(cat, dist_cat(dist[i]))
  }
  print(cat)
  return(data.frame(x, dist, cat))
}

print(dist_from(x = c(1, 3, 5, 7, 9, 11, 13, 15, 17), from = 8.5))



dist_cat = function(x) {
  if (x <= 2) {
    return(c("adjacent"))
  } else if (x > 2 && x <= 4) {
    return(c("close"))
  } else if (x > 4) {
    return(c("far"))
  }
}
dist_from = function(x, from) {
  x = x
  dist = abs(x - from)
  #cat = c()
  #for (i in 1:length(dist)) {
   # cat = c(cat, dist_cat(dist[i]))
  #}
  cat = sapply(dist, dist_cat)
  return(data.frame(x, dist, cat))
}
dist_from(x = c(1, 3, 5, 7, 9, 11, 13, 15, 17), from = 8.5)





shift = function(x, right = TRUE) {
  if (length(x) <= 1) {
    return(x)
  }
  if (right == TRUE) {
    n = length(x)
    c = numeric(n)
    for (i in 2:length(x)) {
      c[i] = x[i - 1] 
    }
    c[1] = x[length(x)]
    return(c)
  } else {
    n = length(x)
    c = numeric(n)
    for (i in 1:length(x) - 1) {
      c[i] = x[i + 1] 
    }
    c[length(x)] = x[1]
    return(c)
  }
}
print(shift(letters[1:4]))
print(shift(letters[1:4], right = FALSE))




estimate_pi = function(xs, ys) {
  times = 4* (sum(sqrt(xs**2 + ys**2) <= 1)/ length(xs))
  return(times)
}

set.seed(42)
x = runif(n = 1000)
y = runif(n = 1000)
print(estimate_pi(xs = x, ys = y))




count_letter = function(string, letter) {
  count = 0
  str_string = strsplit(string, "")
  for (i in 1:length(str_string[[1]])) {
    if (letter[1] == str_string[[1]][i]) {
      count = count + 1
    }
  }
  return(count)
}

calc_vowel_stats = function(string, prop) {
  if (prop == FALSE) {
    count = 0
    str_string = strsplit(string, "")
    for (i in 1:length(str_string[[1]])) {
      if (str_string[[1]][i] == "a" || str_string[[1]][i] == "e" || 
          str_string[[1]][i] == "i" || str_string[[1]][i] == "o" ||
          str_string[[1]][i] == "u") {
        count = count + 1
      }
    }
    return(count)
  } else {
    count = 0
    str_string = strsplit(string, "")
    print(str_string[])
    for (i in 1:length(str_string[[1]])) {
      if (str_string[[1]][i] == "a" || str_string[[1]][i] == "e" || 
          str_string[[1]][i] == "i" || str_string[[1]][i] == "o" ||
          str_string[[1]][i] == "u") {
        count = count + 1
      }
    }
    prop = count / length(str_string[[1]]) 
    return(prop)
  }
}


print(count_letter(string = "mississippi", letter = "i"))
print(calc_vowel_stats(string = "metropolitan", prop = FALSE))
calc_vowel_stats(string = "metropolitan", prop = TRUE)
string = "mississippi"
letter = "i"
str_string = strsplit(string, "")
str_string
length(str_string[[1]])
str_string[[1]][2] == letter[1]


once_sum = function(nums) {
  c = c()
  for (i in 1:length(nums)) {
    count = 0
    for (j in 1:length(nums)) {
      if (i == j) {
        next
      }
      if (nums[i] == nums[j]) {
        count = count + 1
      }
    }
    if (count == 0) {
      c = append(c, nums[i])
    }
  }
  return(sum(c))
}
some_nums = c(1, 2, 3, 4, 5, 5)
once_sum(some_nums)



both_logs = function(x, a, b) {
  temp = log(x, base = exp(a))
  temp2 = log(x, base = exp(b))
  print(temp)
  if (is.integer(temp) == TRUE &&
      is.integer(temp2) == TRUE) {
    return(x)
  }
  return(integer(0))
}
either_logs = function(x, a, b) {
  temp = log(x, base = exp(a))
  temp2 = log(x, base = exp(b))
  if (is.integer(temp) == TRUE ||
      is.integer(temp2) == TRUE) {
    return(x)
  }  
  return(integer(0))
}
either_logs(2,1,1)



sum_subsequences = function(x, gap) {
  c = c()
  for (i in 1:length(x)) {
    if (i == 1) {
      c = append(x[i], c)
    } else if (count == gap) {
      c = append(x[i], c)
      count = 0
    } else {
      count = count + 1
      next
    }
  }
  print(c)
  return(sum(c))
}
vec = c(1:10)
sum_subsequences(x = vec, gap = 1)
sum_subsequences(x = vec, gap = 2)
count = 0
count %% 3 == 0





calc_cumulative = function(x, f = sum) {
  d = c()
  for (i in 1:length(x)) {
    print(x[1:i])
    b = x[1:i]
    d = append(d, f(b))
  }
  return(d)
}

calc_cumulative(x = 1:10, f = sum)

?append



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
  for (i in 1:length(x)) {
    if (i == 1) {
      if (count == 0) {
        count = count + 1
        next
      } else {
        x = x[-i]
      }
    }
  }
  return(x)
}

x <- c(2,1,1,1)

#remove 1, 4, and 5
x <- x[-2]

#view updated vector
x

?setdiff

?remove


x = 50:1
y = list(a = 1:2,
         b = "b",
         c = data.frame(a = 1),
         d = mean,
         e = list(a = 1, 
                  b = 2, 
                  c = list(a = 1, 
                           b = list(a = 1)))
)
z = data.frame(a = 1:5,
               b = "z",
               c = c(TRUE, FALSE, TRUE, FALSE, TRUE)
)
typeof(z[1, 2])

typeof(as.double("7 + 2"))
typeof(5:1)

some_df = data.frame(k=rep(240, times = 37), i = rep("STAT 385", times = 37), 
                     r = c(1:37))
some_df

pts <- list(x = cars[,1], y = cars[,2])
pts

some_list = list(n = c(42), t = c("Exams are fun!"), b = c(6:295))
some_list
typeof(some_list[[1]])


even_odd_rows = function(x, odd = TRUE) {
  df = x
  df[] = NA
  if (odd == TRUE) {
    for (i in 1:length(x[[1]])) {
      if (i %% 2 != 0) {
        df[i,] = x[i,]
      }
    }
    df[rowSums(is.na(df)) == 0, ]
    return(df)
  } else {
    for (i in 1:length(x[[1]])) {
      if (i %% 2 == 0) {
        df[i,] = x[i,]
      }
    }
    na.omit(df)
    return(df)
  }
}
ex_df = data.frame(
  x = 1:10,
  y = 10:1
)
even_odd_rows(ex_df)



df = ex_df
df
df[] = NA
df


for (i in 1:length(ex_df[[1]])) {
  if (i %% 2 != 0) {
    df[i,] = ex_df[i,]
  }
}
df
na.omit(df)
length(ex_df)

ex_list = list(
  a = 1:3,
  b = list(x = 1:3),
  c = 42,
  d = list(y = 2:4)
)
length(ex_list)
for (i in 1:length(ex_list)) {
  if (typeof(ex_list[i]) == "list") {
    
  }
}
only_lists(x = ex_list)


only_lists = function(x) {
  
}
fractions = c()
for (i in 1:117) {
  fractions = append(fractions, 1.0/i)
}
typeof(fractions)

names = c(8.7,9.9,5.6,8.4,7.3,5.6)

calc_olympic_score = function(judges_scores) {
  temp = sort(judges_scores)
  c = c()
  if (temp[1] == temp[length(judges_scores)]) {
    for (i in 2:(length(judges_scores)-1)) {
      c = append(c, i)
    }
  } else {
    for (i in 2:(length(judges_scores)-1)) {
      c = append(c, i)
    }
  }

  return(mean(c))
}

?sort


