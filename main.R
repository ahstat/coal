# Coal: Composition of Real Linear Functions
rm(list=ls())
setwd("~/Documents/GitHub/coal/")
library(igraph)
library(RUnit)
library(data.table)
library(dplyr)
library(gmp) # for bigq
library(distr) # only for distribution plot
source("helpers/1_linear_function.R") # define linear function class
source("helpers/2_evaluate_x.R") # evaluate words in some x
source("helpers/3_word_tree.R") # define word tree class
source("helpers/4_word2coordinates.R") # convert words to vector of coordinates
source("helpers/5_word2index.R") # index for the words
source("helpers/6_random_words.R") # sample random words
source("helpers/7_plot_trajectories.R") # to show trajectory of words
source("helpers/8_plot_trees.R") # to show tree of words
source("helpers/9_continuous.R") # for continuous words

######################
# A complete example #
######################
## Define linear functions
# In this example, we want to compose two functions f and g into words.
# We define f, g, and then combine them into linfunc.
#
# For f, we consider two explicit functions related to the formal f: ax+b
# f_(a1, b1) = (1/3)x-(1/3) and 
# f_(a2, b2) = (1/4)x-(1/4).
f_test = new("Linear function", 
             "a", "b", # formal names for coefficients in f: ax+b
             c(as.bigq(1L, 3L), as.bigq(1L, 4L)), # slopes a
             c(as.bigq(1L, 3L), as.bigq(1L, 4L))) # intercepts b
# For g, we consider the same number of explicit functions for g: cx+d
# g_(c1, d1) = 2x-0 and 
# g_(c2, d2) = 3x-0.
g_test = new("Linear function", 
             "c", "d", # cx+d
             c(as.bigq(2L, 1L), as.bigq(3L, 1L)), 
             c(as.bigq(0L, 1L), as.bigq(0L, 1L)))
# We combine functions f and g
linfunc = c(f_test, g_test)

## Define vocabulary
# We consider the two letters "f" and "g", representing the linear functions
# f and g.
voc = c("f", "g")

## Define initial words
# All words must have the same length. 
# Here we take all words with (nb_f, nb_g) = (1, 2), so we get (x1, x2, x3):
words_init = c("ggf", "gfg", "fgg")

## Define the initial values
# Define initial values as -1, 0 and 1.
x_init = c(as.bigq(-1L, 1L), as.bigq(1L, 1L), as.bigq(0L, 1L))

## Choose whether we only keep integer values
# If TRUE, we replace non integer values with NA, and delete word if the
# related matrix of values only contain NA values.
x_integer_only = TRUE

## Create the word tree
wt = new("Word tree", voc = voc, words_init = words_init,
         x_init = x_init, linfunc = linfunc,
         x_integer_only = x_integer_only)
df = wt@df

## The three functions related to the initial words
df$linfun

## The three matrix of values related to the initial words
# Each row is a x_init (either x1, x2, x3)
# each column is a linear function (either (a1,b1,c1,d1) or (a2,b2,c2,d2))
df$values

## Pushing 9 times
wt = pushn(wt, 9)

## Adding nb_f and nb_g for each row, called 'signature' here
wt = add_signature(wt)

## Adding coordinates
#wt = compute_coord(wt)

## Adding common index
#wt = add_common_index(wt)

## Adding coefficients (in letters and as bigq)
wt = add_coeffs(wt)

## Outputs:
df = wt@df


values = do.call(c, lapply(df$fixed_point, function(x){x[[1]]}))
#values = do.call(c, lapply(df$values, function(x){x[[1]]}))
out = data.table(word = wt@df$word, values = as.numeric(values))

plot(out$values)



plot(out$values - floor(out$values))






plot_tree(get_parents(as.vector(out$word)))






###############################################################################

wt = new("Word tree")
wt = pushn(wt, 10)
wt = add_coeffs(wt)
df = wt@df
df = df[,c("word", "level", "fixed_point")]
df[1:30,]
fixed_point = df$fixed_point
names(fixed_point) = df$word


values = do.call(c, lapply(df$fixed_point, function(x){x[[1]]}))
#values = do.call(c, lapply(df$values, function(x){x[[1]]}))
out = data.table(word = wt@df$word, values = as.numeric(values))

plot(out$values)



plot(out$values - floor(out$values))











###############################################################################

# (a, b, c, d, V) fixed
# For (x, m) fixed, we seek all n such that:
# |alpha * x + beta| <= |x|
# We also define n_{x, m} the limiting n.


###############
# fg function #
###############
library(akima)
N = 100
sequence_c = seq(from = 0, to = 3, length.out = N)
sequence_a = seq(from = 0, to = 3, length.out = N)
ac = expand.grid(sequence_c, sequence_a)
a = ac$Var2
c = ac$Var1
out = (a*c - 2*a + 1)/(1 - a*c)
data <- data.frame(x=a, y=c, distance=out - floor(out))

idx_nan = which(is.nan(data$distance))
idx_inf = which(is.infinite(data$distance))
idx_remove = c(idx_nan, idx_inf)
if(length(idx_remove) > 0) {
  data = data[-idx_remove,]
}
gr <- interp(x=data$x, y=data$y, z=data$distance, xo=sequence_a, yo=sequence_c)
image(gr, xlab = "longitude", ylab = "latitude") # you can of course modify the color palette and the color categories. See ?image for more explanation
contour(gr, add = TRUE)


###
# #
###
w = "fg"
prob_supp = prob_supp_of("f", w, gmp = FALSE)
seq_m = seq(from = 0, to = 50, length.out = 500)
seq_n = seq(from = 0, to = 50, length.out = 500)
mn = expand.grid(seq_n, seq_m)

a = 3
b = 1
c = 1/2
d = 0

m = mn[[2]]
n = mn[[1]]
y = sapply(1:length(m), function(i) {x0(prob_supp, m[i], n[i],
                                a, b, c, d)})
M = matrix(y, ncol = length(seq_n), byrow = TRUE)
out = M - floor(M) # sign(M)*log(abs(M))
image.plot(seq_m,seq_n,out, xlab = "m", ylab = "n")
#contour(out, add = TRUE)


###
# #
###
w = "fg"
prob_supp = prob_supp_of("f", w, gmp = FALSE)
seq_m = seq(from = 0, to = 5, length.out = 100)
seq_n = seq(from = 0, to = 5, length.out = 100)
mn = expand.grid(seq_n, seq_m)

a = 2
b = 1
c = 1/2 #1/2
d = 0

m = mn[[2]]
n = mn[[1]]
y = sapply(1:length(m), function(i) {x0(prob_supp, m[i], n[i],
                                        a, b, c, d)})
M = matrix(y, ncol = length(seq_n), byrow = TRUE)
out = M - floor(M) # sign(M)*log(abs(M))

out = ifelse(abs(M) < 100, 0, M - floor(M)) # etc.

#out = log(0.0001 + abs(M))
image.plot(seq_m,seq_n,out, xlab = "m", ylab = "n")
#contour(out, add = TRUE)

###


n = 10000
min = 0
max = 2
a = runif(n, min, max) + 1i * runif(n, min, max)
b = rep(1, n) #runif(n, min, max) + 1i * runif(n, min, max)
c = runif(n, min, max) + 1i * runif(n, min, max)
d = rep(0, n) #runif(n, min, max) + 1i * runif(n, min, max)
y = sapply(1:n, function(i) {x0(prob_supp, 1, 1, a[i], b[i], c[i], d[i])})
plot(y, xlim = c(-2, 2), ylim = c(0,4), pch = ".")
abline(h=0, col = "orange")



###########################
# Random unif with "hole" #
###########################
w = "fg"
prob_supp = prob_supp_of("f", w, gmp = FALSE)
n = 10000
min = 0
max = 2
a = runif(n, min, max) + 1i * runif(n, min, max)
b = rep(1, n) #runif(n, min, max) + 1i * runif(n, min, max)
c = runif(n, min, max) + 1i * runif(n, min, max)
d = rep(0, n) #runif(n, min, max) + 1i * runif(n, min, max)
y = sapply(1:n, function(i) {x0(prob_supp, 1, 1, a[i], b[i], c[i], d[i])})
plot(y, xlim = c(-2, 2), ylim = c(0,4), pch = ".")
abline(h=0, col = "orange")
# un trou! 20171017

w = "gf"
prob_supp = prob_supp_of("f", w, gmp = FALSE)
n = 10000
min = 0
max = 2
a = runif(n, min, max) + 1i * runif(n, min, max)
b = rep(1, n) #runif(n, min, max) + 1i * runif(n, min, max)
c = runif(n, min, max) + 1i * runif(n, min, max)
d = rep(0, n) #runif(n, min, max) + 1i * runif(n, min, max)
y = sapply(1:n, function(i) {x0(prob_supp, 1, 1, a[i], b[i], c[i], d[i])})
plot(y, xlim = c(-2, 2), ylim = c(0,4), pch = ".")
abline(h=0, col = "orange")

# Note: for more complex words, it can go below Im(y)=0

###########
#         #
###########
w = "fgfgfgfffggggggfffggfgfgf"
# w = "fg"
prob_supp = prob_supp_of("f", w, gmp = FALSE)
m = get_nb_without("g", w)
n = get_nb_without("f", w)
a = 1/3
b = -1/3
c = 2
d = 0
x0(prob_supp, m, n, 3+1i, b, c, d)

t = seq(from = 0, to = 3, by = 0.01)
y = sapply(t, function(t) {x0(prob_supp, m, n, exp(pi*1i*t), b, c, d)})
plot(y, type = "l")
text(Re(y), Im(y), t, cex = 0.7, pos = 3)

# just random?
t = seq(from = 0, to = 2, by = 0.01)
y = sapply(t, function(t) {x0(prob_supp, m, n, 1i, b, exp(pi*1i*t), d)})
plot(y, type = "l")
text(Re(y), Im(y), t, cex = 0.7, pos = 3)

# then divide by t !
t = seq(from = 0, to = 2, by = 0.001)
y = sapply(t, function(t) {x0(prob_supp, m, n, 1i, b, exp(pi*1i*t)/t, d)})
plot(y, type = "l")
text(Re(y), Im(y), t, cex = 0.7, pos = 3)

###################
# One move period #
###################
one_move_period = function(eps, w, m, n, a, b, c, d) {
  t = seq(from = 0, to = 2, by = eps) # one period is 2
  y = sapply(t, function(t) {x0(move(prob_supp, 2-t), m, n, a, b, c, d)})
  return(data.frame(t=t, y=y))
}

one_move_period_beta = function(eps, w, m, n, a, b, c, d) {
  t = seq(from = 0, to = 2, by = eps) # one period is 2
  y = sapply(t, function(t) {beta(move(prob_supp, 2-t), m, n, a, b, c, d)})
  return(data.frame(t=t, y=y))
}
           1/8
      1/4  9/8
  1/2 5/4  5/8
1         13/8
  3/2 3/4  3/8
      7/4 11/8
           7/8
          15/8

x/2
3x + 1 = (2x + 1) + x
                  
         
a = 2
b = 0
c = 2
d = 1
m = 1 #get_nb_without("g", w)
n = 1 #get_nb_without("f", w)

w = "fg"
prob_supp = prob_supp_of("f", w, gmp = FALSE)
plot(one_move_period_beta(0.01, w, m, n, a, b, c, d), type = "l")

w = "fgfgfgfgfgfgfgfgfg"
prob_supp = prob_supp_of("f", w, gmp = FALSE)
plot(one_move_period(0.01, w, m, n, a, b, c, d), type = "l")

w = "fgfgfgfgfgfgfgfgfgf"
prob_supp = prob_supp_of("f", w, gmp = FALSE)
lines(one_move_period(0.01, w, m, n, a, b, c, d), type = "l", col = "blue")

w = "gfgfgfgfgfgfgfgfgf"
prob_supp = prob_supp_of("f", w, gmp = FALSE)
lines(one_move_period(0.01, w, m, n, a, b, c, d), type = "l", col = "green")

w = "gfgfgfgfgfgfgfgfgfg"
prob_supp = prob_supp_of("f", w, gmp = FALSE)
lines(one_move_period(0.01, w, m, n, a, b, c, d), type = "l", col = "red")

beta_inf = -0.50




plot(one_move_period(0.01, w, m, n, a, b, c, d), type = "l")

a = 1/3
b = -1/3
c = 2
d = 0
#w = "gf"
w = "gfggfgggfggggfgggggfggggggfgggggggfggggggggfgggggggggfggggggggggfgggggggggggf"
prob_supp = prob_supp_of("f", w, gmp = FALSE)
m = get_nb_without("g", w)
n = get_nb_without("f", w)
plot(log(one_move_period(0.005, w, m, n, a, b, c, d)), type = "l")

a = 1/3
b = -1/3
c = 2
d = 0
w = "gfggfgfg"
prob_supp = prob_supp_of("f", w, gmp = FALSE)
m = get_nb_without("g", w)
n = get_nb_without("f", w)
out = one_move_period(0.005, w, m, n, a, b, c, d)
plot(out$t[-1], diff(abs(log(out$y))), type = "l")
plot(out$t[-c(1,2)], diff(diff(abs(log(out$y)))), type = "l")
# Looks like 0, but it is not for some, not so good.



for(var in seq(from = -0.001, to = 10, by = 0.1)) {
  a = var
  plot(one_move_period(0.01, w, m, n, a, b, c, d)$t, 
       one_move_period(0.01, w, m, n, a, b, c, d)$y - 
         one_move_period(0.01, w, m, n, a-0.01, b, c, d)$y, type = "l",
       main = a)
}

init = 0
#for(var in seq(from = -0.001, to = 2, by = 0.01)) {
for(var in seq(from = 0.3149800, to = 0.3149811, by = 0.0000001)) {
  a = var
  out = one_move_period(0.01, w, m, n, a, b, c, d)
  t = out$t
  y = out$y 
    plot(t, y, type = "l", main = var)#, ylim = c(-3,3))
}

# a = 1/4*2^(1/3) # changing behavior for a ~ 0.3149803
# log(a) = -4 + (1/3) * log(2)

# Idea: derivatives with a, b, c, d. But to do what?
# x0 when varying d i.e. derivative with d. (with a,b,c... also)







##
#
##

x0_word = function(w, a, b, c, d) {
  m = get_nb_without("g", w)
  n = get_nb_without("f", w)
  prob_supp = prob_supp_of("f", w, gmp = FALSE)
  return(x0(prob_supp, m, n, a, b, c, d))
}
# TODO: special case when w = "fffff...ff" ou "gggg...gg"

w = "ffggfgffgffgfgf"
a = 1/3
b = -1/3
c = 2
d = 0
x0_word("ggf", a, b, c, d) # 4

##################################
# Outputs linear formal for Sage #
##################################
n = 10
wt = new("Word tree")
wt = pushn(wt, n)
wt = add_signature(wt)
wt = add_coeffs(wt) # ???
df = wt@df

df2 = df[,c("word", "level", "nb_f", "nb_g", "coeff_1")]

add_mult = function(string) {
  for(i in 1:2) {
    string = gsub("aa", "a*a", string)
    string = gsub("ab", "a*b", string)
    string = gsub("ac", "a*c", string)
    string = gsub("ad", "a*d", string)
    string = gsub("ba", "b*a", string)
    string = gsub("bb", "b*b", string)
    string = gsub("bc", "b*c", string)
    string = gsub("bd", "b*d", string)
    string = gsub("ca", "c*a", string)
    string = gsub("cb", "c*b", string)
    string = gsub("cc", "c*c", string)
    string = gsub("cd", "c*d", string)
    string = gsub("da", "d*a", string)
    string = gsub("db", "d*b", string)
    string = gsub("dc", "d*c", string)
    string = gsub("dd", "d*d", string)
  }
  return(string)
}
add_mult("aaabbaaaa+cdddc+ab+b")

df2$coeff_1 = sapply(df2$coeff_1, add_mult)
write.csv(df2, "formal.csv", row.names = FALSE)

####################
# A simple example #
####################
n = 10
voc = c("f", "g")
words_init = c("ggf", "gfg", "fgg")
linfunc = c(f_base, g_base)
x_init = as.bigq(1L, 1L)
wt = new("Word tree", voc = voc,
         words_init = words_init, linfunc = linfunc,
         x_init = x_init, x_integer_only = TRUE)
wt = pushn(wt, n)



######################
# A complete example #
######################
## Define linear functions
# We consider two linear functions for f: ax+b
# f_(a1, b1) = (1/3)x-(1/3) and 
# f_(a2, b2) = (1/4)x-(1/4).
f_test = new("Linear function", 
             "a", "b", # ax+b
             c(as.bigq(1L, 3L), as.bigq(1L, 4L)), # slopes a
             c(-as.bigq(1L, 3L), -as.bigq(1L, 4L))) # slopes b
# We consider the same number of linear functions for g: cx+d
# g_(c1, d1) = 2x-0 and 
# g_(c2, d2) = 3x-0.
g_test = new("Linear function", 
             "c", "d", # cx+d
             c(as.bigq(2L), as.bigq(3L)), 
             c(as.bigq(0L), as.bigq(0L)))
linfunc = c(f_test, g_test)

## Define vocabulary
# We consider the two letters "f" and "g", representing the linear functions
# f and g.
voc = c("f", "g")

## Define initial words
# All words must have the same length. 
# Here we take all words with signature (1, 2), so we get (x1, x2, x3)
words_init = c("ggf", "gfg", "fgg")

## Define the initial values
# Define initial values as -1, 0 and 1.
x_init = c(-as.bigq(1L, 1L), as.bigq(0L, 1L), as.bigq(1L, 1L))

## Choose whether we only keep integer values
# If TRUE, we replace non integer values with NA, and delete word if the
# related matrix of values only contain NA values.
x_integer_only = FALSE

## Create the word tree
wt = new("Word tree", voc = voc, words_init = words_init,
         x_init = x_init, linfunc = linfunc,
         x_integer_only = x_integer_only)
df = wt@df

## The three functions related to the initial words
df$linfun

## The three matrix of values related to the initial words
# Each row is a x_init (either x1, x2, x3)
# each column is a linear function (either (a1,b1,c1,d1) or (a2,b2,c2,d2))
df$values

## Pushing 3 times
wt = pushn(wt, 3)

## Adding signature
wt = add_signature(wt)

## Adding coordinates
wt = compute_coord(wt)

## Adding common index
wt = add_common_index(wt)

## Adding coefficients (in letters and as bigq)
wt = add_coeffs(wt)

## Outputs:
df = wt@df
coord = wt@coord




values = do.call(c, lapply(df$values, function(x){x[[1]]}))
data.table(word = wt@df$word, values = as.numeric(values))









png('out.png', width = 3000, height = 1500)
plot_tree(get_parents(as.vector(out$word)))
dev.off()



############
# Examples #
############
# Some can be a little long to run

# source("helpers/example_1_concatenation.R")
# source("helpers/example_2_distance_between_words.R")
# source("helpers/example_3_b_for_each_signature.R")
# source("helpers/example_4_integer_bottom_to_up.R")





























###############################################################################

##############################################
# Get the words corresponding to a signature #
##############################################
add_letters_one = function(out, signature_number, voc = c("f", "g")) {
  mat = signature_func(out, voc)
  listed_words = list()
  for(i in 1:length(voc)) {
    idx = which(mat[,i] < signature_number[i])
    listed_words[[i]] = add_letters(rownames(mat)[idx], voc = voc[i])
  }
  out = unlist(listed_words)
  return(out)
}

get_words_with_signature = function(signature_number, voc = c("f", "g")) {
  if(length(signature_number) != length(voc)) {
    stop("signature_number must correspond one-to-one to the vocabulary.")
  }
  sum_sign = sum(signature_number)
  out = ""
  if(sum_sign > 0) {
    for(k in 1:sum_sign) {
      out = add_letters_one(out, signature_number, voc)
    }
  }
  return(out)
}

get_words_with_signature(c(1,1))
get_words_with_signature(c(1,1,2), voc = c("f", "g", "h"))
get_words_with_signature(c(4,5))

###################
# Signature order #
###################
abcd_grid = function(x_min = -3L, x_max = 3L,
                     y_min = -3L, y_max = 3L,
                     nb_elements_total = 10000) {
  # Create grid_x and grid_y
  nb_lim = floor(sqrt(nb_elements_total))
  grid_temp = as.bigq(1L, 2713L) + as.bigq(0:(nb_lim-1))/(nb_lim-1)
  grid_x = (x_max - x_min)*grid_temp + x_min
  grid_y = (y_max - y_min)*grid_temp + y_min
  
  # coeff a and c
  abcd = expand.grid(grid_y, grid_x)
  colnames(abcd) = c("c", "a")
  
  # remove a-c == 0
  idx_del = which(abcd$a == abcd$c)
  abcd = abcd[-idx_del,]
  
  # coeff b and d
  abcd$b = 1 / (abcd$a - abcd$c)
  abcd$d = 1 / (abcd$a - abcd$c)
  idx_keep = abcd$b * abcd$c + abcd$d < abcd$a * abcd$d + abcd$b # (+)
  abcd = abcd[idx_keep,]
  
  return(abcd)
  
  # grid = as.bigq(-5:5)/2 + as.bigq(1L, 2713L)
  # abcd = expand.grid(grid, grid, grid, grid)
  # abcd = abcd[paste0("Var", length(abcd):1)]
  # colnames(abcd) = letters[1:4]
}

matrix_abcd = function(wt_linfunc) {
  M = NULL
  for(i in 1:length(wt_linfunc)) {
    letter_slopes_i = wt_linfunc[[i]]@coeff_x
    values_slopes_i = wt_linfunc[[i]]@seq_coeff_x
    values_slopes_i = as.numeric(values_slopes_i)
    
    letter_intercept_i = wt_linfunc[[i]]@coeff_1
    values_intercept_i = wt_linfunc[[i]]@seq_coeff_1
    values_intercept_i = as.numeric(values_intercept_i)
    
    M_first = matrix(c(values_slopes_i, values_intercept_i),
                     ncol = 2)
    colnames(M_first) = c(letter_slopes_i, letter_intercept_i)
    M = cbind(M, M_first)
  }
  df_sorting = as.data.frame(M)
  return(df_sorting)
}














signature_number = c(4,4)

## Define abcd grid
val_bor = 2L
# abcd = abcd_grid(x_min = -val_bor-1, x_max = val_bor-1, 
#                  y_min = -val_bor-1, y_max = val_bor-1,
#                  nb_elements_total = 10000)
abcd = abcd_grid(x_min = 0, x_max = 2, 
                 y_min = 0, y_max = 2,
                 nb_elements_total = 10000)

## Define related linear functions
f_test = new("Linear function", "a", "b", abcd$a, abcd$b)
g_test = new("Linear function", "c", "d", abcd$c, abcd$d)
linfunc = c(f_test, g_test)

## Define initial words
words_init = get_words_with_signature(signature_number)

## Create the word tree
wt = new("Word tree", words_init = words_init, linfunc = linfunc)
wt = add_common_index(wt)
wt = add_coeffs(wt)
df = wt@df
# df

output = matrix(as.numeric(do.call(c, df$seq_coeff_1)), ncol = nrow(df))
colnames(output) = df$word

df_sorting = matrix_abcd(wt@linfunc)
# out_final = cbind(M, output)

eps = 1e-10

names = colnames(output)
ordr = apply(output, 1, function(x){
  if(all((diff(sort(x)) < eps) == FALSE)) {
    paste(names[order(x)], collapse = "<") 
  } else {
    NA
  }
})

df_sorting = cbind(df_sorting, ordr)

idx_remove = which(is.na(df_sorting$ordr))
if(length(idx_remove) > 0) {
  df_sorting = df_sorting[-idx_remove,]
}

# Colors
levels_words = levels(df_sorting$ordr)
ramp = rainbow(length(levels_words))

color = rep(NA, nrow(df_sorting))
for(i in 1:length(levels_words)) {
  color[df_sorting$ordr == levels_words[i]] = ramp[i]
}

#plot(jitter(df_sorting$a), jitter(df_sorting$c), col = color)
png("out_3.png", width = 700, height = 700)
plot(df_sorting$a, df_sorting$c, col = color, pch = 15, asp = 1,
     main = signature_number)
dev.off()

length(levels_words) # == 24

# For signature (m, n):
# nb_words = n parmi n+m
# nb_colors = ?

# nb_colors:
# (1,1) = 1
# (1,2) = 3
# (1,3) = 3
# (1,4) = 3
# (1,5) = 3

# (2,1) = 3
# (2,2) = 24
# (2,3) = 
# (2,4) = 
# (2,5) = 

# (3,1) = 3
# (3,2) = 
# (3,3) = 
# (3,4) = 
# (3,5) = 

# (4,1) = 3
# (4,2) = 
# (4,3) = 
# (4,4) = 
# (4,5) = 

# (5,1) = 3
# (5,2) = 
# (5,3) = 
# (5,4) = 
# (5,5) = 


ordr = as.numeric(matrix(ordr, ncol = 1))
# todo: replace with a factor, and ordr is a factor, not numeric or string.
#  then for many a,b,c,d
#  then plot...
cbind(M, ordr)

df$seq_coeff_1




plot(sapply(get_words_with_signature(c(4,10)), word2index))



# TODO:
# - When some equality, discard the order
# - Points plein
# - ...



###############################################################################

##############
# Continuous #
##############


###############

add_letters_n = function(n, init = "") {
  if(n == 1) {
    return(add_letters(init))
  } else {
    return(add_letters_n(n-1, add_letters(init)))
  }
}

add_letters_to = function(n) {
  out = c()
  for(i in 1:n) {
    out = c(out, add_letters_n(i))
  }
  return(out)
}


a = 1/3
b = -1/3
c = 2
d = 0

x0_word = function(w, a, b, c, d) {
  m = get_nb_without("g", w)
  n = get_nb_without("f", w)
  prob_supp = prob_supp_of("f", w, gmp = FALSE)
  out = x0(prob_supp, m, n, a, b, c, d)
  return(out)
}

all_words_keep = function(n) {
  all_words = add_letters_to(n)
  idx_keep = which(sapply(all_words, function(w) {get_nb_without("g", w)}) >= 1)
  all_words = all_words[idx_keep]
  idx_keep = which(sapply(all_words, function(w) {get_nb_without("f", w)}) >= 1)
  all_words = all_words[idx_keep]
  return(all_words)
}


out = sapply(all_words_keep(10), function(w){x0_word(w,a, b, c, d)})
plot(out, type = "l", asp = 1)



x0(move(prob_supp, t), m, n, a, b, c, d)


############################


a = 1 - 1i
b = 1 + 0i
c = 3 + 0i
d = 4 + 0.5i# b * (c - 1) / (a - 1) # = b * (1-c)/(1-a)

N = 10
m_n = expand.grid(1:N, 1:N)
m = m_n[[2]]
n = m_n[[1]]

c_frak = c^n
a_frak = a^m
b_frak = (1/2) * b / (1-a)
d_frak = (1/2) * d / (1-c) # = b_frak si commut

out = 2*((1-a_frak)*b_frak*c_frak + (1-c_frak)*d_frak)/(1-a_frak*c_frak)

plot(out, type = "o")
text(out, paste0("(", paste(m, n, sep = ","), ")"), cex = 0.7, pos = 3)

###############################################################################



a = 1 - 1i
b = 1 + 0.7i
c = 3 + 0i
d = 4 + 0.5i

N = 100

get_out = function(a,b,c,d,N=1000) {
  m = (N-1):N
  n = (N-1):N
  c_frak = c^n
  a_frak = a^m
  b_frak = (1/2) * b / (1-a)
  d_frak = (1/2) * d / (1-c) # = b_frak si commut
  out = 2*((1-a_frak)*b_frak*c_frak + (1-c_frak)*d_frak)/(1-a_frak*c_frak)
  if(is.na(out[1])) {
    return(NA)
  }
  if(is.na(out[2])) {
    return(NA)
  }
  if(abs(out[2] - out[1]) > 0.01) {
    return(NA)
  } else {
    return(out[2])
  }
}


sequence = seq(from = -10, to = 10, length.out = 100)
a_c = expand.grid(sequence, sequence)
a_frak = a_c[[2]]
c_frak = a_c[[1]]
out = sapply(1:nrow(a_c), function(x){get_out(1i+a_c[[2]][x],b,-0.5i+a_c[[1]][x],d)})

plot(out, type = "p")
#text(out, paste0("(", paste(a_frak, c_frak, sep = ","), ")"), cex = 0.7, pos = 3)


out = data.frame(a = a_frak, c = c_frak, out = out)

out[which(is.na(out$out)),]


######################


a = 1/3
b = -1/3
c = 2
d = 0

# Changing d or b only have scale effects
a = 1/3
b = -1
c = 2
d = 0

# Why a line?: ok because just rotating the line when all real
a = 1/3
b = -1 + 1i
c = 2
d = 0

# Changing a=1/3 to a+ti is very interesting
a = 1/3 + 0.001i
b = -1/3
c = 2+ 1i
d = 0
out = sapply(add_letters_to(10), function(w){x0_word(w,a, b, c, d)})
plot(out, type = "p")

# Changing a=1/3 to a+ti is very interesting
a = 1/3 + 0.001i
b = -1/3
c = 2
d = 0
out = sapply(add_letters_to(10), function(w){x0_word(w,a, b, c, d)})
plot(out, type = "p")

# This one is full of pentagon!!! ???!!!
a = 1/3 + 1i
b = -1/3
c = 2
d = 0
out = sapply(add_letters_to(10), function(w){x0_word(w,a, b, c, d)})
plot(out, type = "l")

# This one is full of pentagon!!! ???!!!
a = 1/3 + 0.8i
b = -1/3
c = 2
d = 0
out = sapply(add_letters_to(10), function(w){x0_word(w,a, b, c, d)})
plot(out, type = "l")

###########
# sinus.R #
###########
x = seq(from = 0, to = 2*pi, length.out = 1000)
nb_move = 5 # recommanded = 5
plot(0, 0, xlim = c(0,1), ylim = c(0, 1), type = "n")
for(t in seq(from = 0, to = 2*pi, length.out = nb_move)) {
  lines(x/(2*pi), (sin(x+t)+x-sin(t))/(2*pi), type = "l", col = "blue")
}
lines(x/(2*pi), (sin(x) + x)/(2*pi), type = "l", col = "red")
lines(x/(2*pi), (cos(x) + x - 1)/(2*pi), type = "l")