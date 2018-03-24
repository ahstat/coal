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