#####################################################################
# Fixed point for words of the form f^m g^n with fixed coefficients #
#####################################################################
# Settings:
# - For all x: f(x)=3x+1, g(x)=x/2
# - For m, n integers, x0(m,n) is the fixed point of w = f^m g^n
#   For other values, we generalize f^m g^n according to 9_continuous.R
w = "fg"
prob_supp = prob_supp_of("f", w, gmp = FALSE)
seq_m = seq(from = 0, to = 1, length.out = 200) # 500
seq_n = seq(from = 0, to = 0.2, length.out = 200) # 500
mn = expand.grid(seq_n, seq_m)
m = mn[[2]]
n = mn[[1]]
a = 1; b = 1; c = 10; d = 0
# a = 1/3; b = -1/3; c = 2; d = 0

# We compute the fixed point x0 for all (m,n)
y = sapply(1:length(m), function(i) {
  x0(prob_supp, m[i], n[i], a, b, c, d)
})
M = matrix(y, ncol = length(seq_n), byrow = TRUE)

# Fractional part and plot
frac_part = M - floor(M)
library(fields)
png("outputs/fg_frac_fixed_.png", 800, 800)
image.plot(seq_m,seq_n, frac_part, xlab = "m", ylab = "n")
dev.off()

####################################################
# Fixed point for word fg with random coefficients #
####################################################
# Settings:
# - For all x: f(x)=ax+1, g(x)=cx with a, c random complex numbers
# - x0 is the fixed point of w = fg
n = 10000
min = 0
max = 2
a = runif(n, min, max) + 1i * runif(n, min, max)
b = rep(1, n)
c = runif(n, min, max) + 1i * runif(n, min, max)
d = rep(0, n)
y = sapply(1:n, function(i) {x0(prob_supp, 1, 1, a[i], b[i], c[i], d[i])})
plot(y, xlim = c(-2, 2), ylim = c(0,4), pch = ".")
abline(h=0, col = "orange")
# Explanation:
# For all x: fg(x)=acx+1 so x0 = 1/(1-ac)
#
# Let a = (1+i)/2 and c following Unif(min, max)
# with min from -10 to 0 and max from 0 to 10.
n = 100000
min = -10
max = 10
seq_min = seq(from=-10, to = 0, by = 0.5)
seq_max = seq(from=0, to = 10, by = 0.5)
n_height = length(seq_min)
n_width = length(seq_max)
png(paste0("outputs/unif", min, "_", max, ".png"),
    width = 300 * n_width,
    height = 300 * n_height)
par(mar=c(0,0,0,0), par(mfrow=c(n_height, n_width)))
for(min in seq_min) {
  print(min)
  for(max in seq_max) {
    #print(max)
    a = (1/2) * (1 + 1i)
    c = runif(n, min, max) + 1i * runif(n, min, max)
    plot(1/(1-a*c), pch = ".", xlab = "", ylab = "", bty="n",
         axes = 0, xlim = c(-1/2, 1/2), ylim = c(-1/2, 1/2))
  }
}
dev.off()

##########################################################
# Fixed point for random words with complex coefficients #
##########################################################
set.seed(1234)
words = random_words(100, 14)
for(i in 1:30) {
  print(i)
  w = words[i]
  
  prob_supp = prob_supp_of("f", w, gmp = FALSE)
  m = get_nb_without("g", w)
  n = get_nb_without("f", w)
  
  name = paste0(i, "_", w, ".png")
  
  ## Setting 1:
  # a = exp(2iπt) for t in [0, 1]
  # b = -1/3
  # c = 2
  # d = 0
  b = -1/3; c = 2; d = 0
  t = seq(from = 0, to = 1, by = 0.0003)
  y = sapply(t, function(t) {x0(prob_supp, m, n, exp(2*1i*pi*t), b, c, d)})
  png(paste0("outputs/x0_complex/setting1_", name), 800, 800)
  plot(y, type = "l", asp = 1, main = w)
  # text(Re(y), Im(y), t, cex = 0.7, pos = 3)
  dev.off()
  
  ## Setting 2:
  # a = 1i
  # b = -1/3
  # c = exp(iπt)/t for t in [-20, 20]
  # d = 0
  a = 1i; b = -1/3; d = 0
  t = seq(from = -20, to = 20, by = 0.0003)
  y = sapply(t, function(t) {x0(prob_supp, m, n, a, b, exp(pi*1i*t)/t, d)})
  png(paste0("outputs/x0_complex/setting2_", name), 800, 800)
  plot(y, type = "l", asp = 1, main = w)
  dev.off()
  
  ## Setting 3:
  # a = 1i
  # b = -1/3
  # c = exp(2iπt)/t for t in [-20, 20]
  # d = 0
  a = 1i; b = -1/3; d = 0
  t = seq(from = -20, to = 20, by = 0.0003)
  y = sapply(t, function(t) {x0(prob_supp, m, n, a, b, exp(2*pi*1i*t)/t, d)})
  png(paste0("outputs/x0_complex/setting3_", name), 800, 800)
  plot(y, type = "l", asp = 1, main = w)
  dev.off()
}

###################################################
# Fixed points during a complete period of a word #
###################################################
## Compute the fixed points for a complete period of a word
one_move_period = function(eps, prob_supp, m, n, a, b, c, d) {
  t = seq(from = 0, to = 2, by = eps) # one period is 2
  y = sapply(t, function(t) {x0(move(prob_supp, 2-t), m, n, a, b, c, d)})
  return(data.frame(t=t, y=y))
}

a = 2; b = 0; c = 2; d = 1
m = 1; n = 1

## Fixed point for words from fg to gf to fg
w = "fg"
prob_supp = prob_supp_of("f", w, gmp = FALSE)
plot(one_move_period(0.01, prob_supp, m, n, a, b, c, d), type = "l",
     ylab = "Fixed point",
     main = "Fixed point for words from fg to gf to fg")

## Difference of fixed point for words from fg to gf to fg, with a varying
for(a in seq(from = 0.0101, to = 10, by = 0.025)) {
  one_period = one_move_period(0.01, prob_supp, m, n, a, b, c, d)
  one_period_next = one_move_period(0.01, prob_supp, m, n, a+0.01, b, c, d)
  delta = one_period_next$y - one_period$y
  png(paste0("outputs/diff_moving/", a, ".png"), 800, 800)
  plot(one_period$t, delta, type = "l", xlab = "t",
       main = paste0("Difference of fixed point values when a=", a))
  dev.off()
}

####################################
# Moving for a continuous function #
####################################
# Instead on considering "prob_supp_of" a word w, we can define it directly
# from a CDF. The following CDF keeps continuity when moving to the right 
# modulo 1.

## CDF of interest involving sinus functions
x = seq(from = 0, to = 2*pi, length.out = 1000)
nb_move = 5 # recommanded = 5
plot(0, 0, xlim = c(0,1), ylim = c(0, 1), type = "n",
     xlab = "t", ylab = "f(t)", 
     main = "Moving for a sinus continuous function")
for(t in seq(from = 0, to = 2*pi, length.out = nb_move)[-1]) {
  lines(x/(2*pi), (sin(x+t)+x-sin(t))/(2*pi), type = "l")
}

## Related density
x = seq(from = 0, to = 2*pi, length.out = 100)
my_prob_supp = list(supp = (x/(2*pi)), prob = cos(x)+1)
my_prob_supp$prob = my_prob_supp$prob / sum(my_prob_supp$prob)

## Fixed point when moving the probability distribution during one period
a = 2; b = 0; c = 2; d = 1
m = 1; n = 1
png("outputs/fixed_moving_custom_prob.png", 800, 800)
plot(one_move_period(0.01, my_prob_supp, m, n, a, b, c, d), type = "l",
     ylab = "Fixed point",
     main = "Fixed point for a custom probability")
dev.off()
# We observe a smooth function (when length(x) --> +Inf)