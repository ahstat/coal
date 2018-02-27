n = 20
wt = new("Word tree", x_init = as.bigq(1L, 1L), x_integer_only = TRUE)
wt = pushn(wt, n)

# Get the vector of numeric values
numeric_values = lapply(wt@df$values, function(x){x[[1]]})
numeric_values = do.call(c, numeric_values)
numeric_values = as.numeric(numeric_values)

# Data frame of word, level and related integer value for Collatz
df = wt@df
df = data.frame(word = df$word, level = df$level, values = numeric_values)

idx = 1:nrow(df) - 1
plot(idx, df$values, 
     xlab = 'Index 0="", 1="f", 2="g" etc. restricted to integer values',
     ylab = "Output of x, f(x), g(x), etc.",
     main = "Init: x = 1")

plot(idx, log(1 + df$values),
     xlab = 'Index 0="", 1="f", 2="g" etc. restricted to integer values',
     ylab = "Output of log(1+x), log(1+f(x)), log(1+g(x)), etc.",
     main = "Init: x = 1")

plot(log(sort(1+df$values)))
plot(log(1+diff(unique(sort(df$values)))))

## Sum at each level
df %>% group_by(level) %>% summarize(sum(values))
# 1 2 4 9 18 41 83 190 385 869 1794
# 3997 8223 18384 37798 83536 173661 380134 789931 1731373 3595965

## Number of non NA at each level
# We g(x)=2x, we have g(y) valid for each valid y.
# So u_{n+1} > u_{n}
nonNA_each_level = df %>% group_by(level) %>% summarize(length(values))
# 1 2 2 3 4 5 6 9 10 13 18 23 29 40 51 67 88 116 154 201 268

# Diff of number of NA at each level
# Since u_{n+1} >= u_{n} with the previous sequence, we
# can take the diff u_{n+1} - u_{n} >= 0 which counts the number
# of valid f at each level
diff(nonNA_each_level$`length(values)`)
# 1 0 1 1 1 1 3 1 3 5 5 6 11 11 16 21 28 38 47 67
# (not in OEIS)

# For comparison, here is the number of 'go to the left' if
# all moves are valid on the binary tree
2^(1:nrow(nonNA_each_level) - 1)

# Number of non (N or 0) at each level
df %>% group_by(level) %>% filter(values > 0) %>% summarize(length(values))
# 1 1 1 2 2 3 4 6 7 10 14 19 24 34 45 59 79 107 142 188 253
# (not in OEIS)

# Close formula?