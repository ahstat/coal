# Problem: get all paths reaching to the same point, exactly
# Let a, b, c, d, m, n. Find all class for coeff_1.
n = 10
wt = new("Word tree")
wt = pushn(wt, n)
wt = add_signature(wt)
wt = compute_coord(wt)
wt = add_common_index(wt)
wt = add_coeffs(wt)
df = wt@df

##
# Filter words with 5 'f' and 5 'g'
##
get_b_info_for = function(nb_f = 5, nb_g = 5, df) {
  idx = which(df$nb_f == nb_f & df$nb_g == nb_g)
  words = df[idx,]$word
  
  b = df[idx,]$seq_coeff_1
  b = do.call(c, b)
  b = as.numeric(b)
  names(b) = words
  
  a = df[idx,]$seq_coeff_x
  a = do.call(c, a)
  a = as.numeric(a)
  names(a) = words
  
  quot = b/(1-a)
  names(quot) = words
  
  len_b = length(b)
  len_unique_b = length(unique(b))
  diff_len = len_b - len_unique_b
  print(paste0("There are ", len_b, " elements and ", len_unique_b,
               " different elements so ", diff_len, " in common."))
  rle_b = rle(sort(b))
  idx_dup = which(rle_b[[1]] > 1)
  value_dup = rle_b[[2]][idx_dup]
  print("Equal words:")
  for(i in 1:length(value_dup)) {
    print(names(b)[which(abs(value_dup[i] - b) < 1e-10)])
  }
  
  if(length(b) > 1) {
    main = paste0("Log of sorted differences of b for (", nb_f, ", ", nb_g, ")")
    plot(log(0.0001+diff(sort(b))), type = "l",
         main = main)
    
    plot(quot, main = "Which fix point x for each iteration? (How to sort?)") 
    
    plot(quot-floor(quot), main = "Fix point x modulo 1")
    abline(h = 0, col = "red")
    
    plot(log(0.0001+quot-floor(quot)),
         main = "Log of fix point modulo 1 (integer fix point when very low)")
    abline(h = log(0.0001), col = "red")
    text(log(0.0001+quot-floor(quot)), names(quot), pos = 3, cex = 0.7)
  }
}

get_b_info_for(3, 4, df)
# There are 35 elements and 34 different elements so 1 in common.
# Equal words are "ffggggf" and "gffgfgg"
# Note: (3, 4) is the lowest (nb_f, nb_g) with at least one common element.

get_b_info_for(5, 5, df)
# There are 252 elements and 231 different elements so 21 in common.

##
# Fixed point
##
wt = add_coeffs(pushn(new("Word tree"), 15))
fixed = as.numeric(do.call(c, wt@df$fixed_point))
plot(log(abs(sort(fixed))), main = "Log of fixed point (red lines are integers below 32)")
abline(h=log(abs(1:32)), col = "red")
# One for negative fixed points, one for positive one,
# very different behavior.
plot(abs(sort(fixed)), main = "Abs of fixed points")
plot(log(diff(sort(fixed))), main = "...")
# Something we can see easily: when coeffx < 1, > 1 = 1 as a function of a, c.
# Get a condition to have large fixed points.

##
# Global behavior (to see behavior when value of a changes...)
##
# Just to see...
log_a = log(as.numeric(do.call(c, df$seq_coeff_x)))
log_1_minus_b = log(1 - as.numeric(do.call(c, df$seq_coeff_1)))

plot(log_a, log_1_minus_b)

df_ab = data.frame(log_a = log_a, log_1_minus_b = log_1_minus_b)

for(val in unique(log_a)) {
  idx = which(df_ab$log_a == val)
  if(length(idx) > 1) {
    plot(diff(sort(df_ab$log_1_minus_b[idx])), main = val)
  }
}