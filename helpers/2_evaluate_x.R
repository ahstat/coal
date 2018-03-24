##################
# Redefine outer #
##################
# to make it working for bigq elements
outer = function(x, y) {
  return(x %*% t(y))
}

##
# Examples
##
## Main example
# Initial value for x (i.e. value for the word "") and
# Applying a multiplication with two slopes
x_init = c(as.bigq(1L, 1L), as.bigq(2L, 1L), as.bigq(3L, 1L), as.bigq(4L, 1L))
slopes = c(as.bigq(1L, 3L), as.bigq(1L, 4L))
outer(x_init, slopes)
# x1 x2 x3 x4 %outer% s1 s2 =
#  x1*s1 x1*s2
#  x2*s1 x2*s2
#  x3*s1 x3*s2
#  x4*s1 x4*s2
# i.e.:
# 1 2 3 4 %outer% 1/3 1/4 = 
#  1/3   1/4 
#  2/3   1/2 
#  1     3/4 
#  4/3   1

## Degenerate examples
x_init0 = NULL
x_init1 = as.bigq(1L, 1L)
# Applying a multiplication with a slope
slopes1 = as.bigq(1L, 3L)
outer(x_init0, slopes1)
outer(x_init1, slopes1) # the matrix 1/3
outer(x_init, slopes1) # the column t(1/3 2/3 1 4/3)
# Applying a multiplication with two slopes
outer(x_init0, slopes) # the NULL matrix bigq(0)
outer(x_init1, slopes) # the matrix 1/3 1/4
rm(x_init0, x_init1, x_init, slopes1, slopes)

##
# Unit tests
##
x_init = c(as.bigq(1L, 1L), as.bigq(2L, 1L), as.bigq(3L, 1L), as.bigq(4L, 1L))
slopes = c(as.bigq(1L, 3L), as.bigq(1L, 4L))
checkEquals(outer(NULL, as.bigq(1L, 3L)), as.bigq(NULL))
checkEquals(outer(as.bigq(1L, 1L), as.bigq(1L, 3L)), 1/3)
checkEquals(outer(x_init, as.bigq(1L, 3L)), x_init/3L)
checkEquals(outer(NULL, slopes), as.bigq(NULL))
checkEquals(outer(as.bigq(1L, 1L), slopes), slopes)
checkEquals(outer(x_init, slopes), 
            matrix(c(as.bigq(c(1,2,3,4))/3, as.bigq(c(1,2,3,4))/4), ncol = 2))
rm(x_init, slopes)

###########################################
# Linear coeffs repeated to form a matrix #
###########################################
# The coeffs are either the slopes or the intercepts of a linear function.
rep_mat = function(coeffs, nb_x) {
  out = outer(rep(1, nb_x), coeffs)
  return(out)
}

##
# Examples
##
# For example, f is related to slopes and intercepts
# This means that we selected f = x/3 - 1/3 and f = x/4 - 1/4 as possible
# evaluation of the function f.
slopes = c(as.bigq(1L, 3L), as.bigq(1L, 4L))
intercepts = c(-as.bigq(1L, 3L), -as.bigq(1L, 4L))

# In columns is each possible evaluation of f,
# In rows, each selection of x_init the initial value where to compute f
# (in this helper function it is a repetition of the coefficient)
rep_mat(slopes, 3)
# 1/3  1/4 
# 1/3  1/4 
# 1/3  1/4 

rep_mat(intercepts, 3)
# -1/3 -1/4
# -1/3 -1/4
# -1/3 -1/4

rm(slopes, intercepts)

##
# Unit tests
##
slopes = c(as.bigq(1L, 3L), as.bigq(1L, 4L))
intercepts = c(-as.bigq(1L, 3L), -as.bigq(1L, 4L))
checkEquals(nrow(rep_mat(slopes, 3)), 3)
checkEquals(ncol(rep_mat(slopes, 3)), 2)
checkEquals(rep_mat(slopes, 3)[2,], slopes)
checkEquals(rep_mat(intercepts, 3)[2,], intercepts)
rm(slopes, intercepts)

##############################################################
# Evaluate a word with given slopes and intercepts at some x #
##############################################################
# Given the function f = ax+b, we compute it where:
# - 'a' is related to some numeric slopes,
# - 'b' is related to some numeric intercepts,
# - 'x' is related to some numeric values
# The output is a matrix of size 'number of x' \times 'number of (a,b)'
#
# We evaluate words like this during initialization
# In other cases, we prefer to push a matrix from previous matrix 
# (see the function push_matrix_values).
compute_matrix_values = function(x_init, slopes, intercepts) {
  nb_x = length(x_init)
  mat = outer(x_init, slopes) + rep_mat(intercepts, nb_x) 
  # Note: rownames and colnames are not implemented in gmp
  # rownames(mat) = paste("x =", x_init)
  # colnames(mat) = paste("coeffs", 1:length(slopes))
  return(mat)
}

##
# Examples
##
## Main example
# Initial value for x (i.e. value for the word "") and
# Doing ax+b with two couples (a1,b1), (a2,b2)
x_init = c(as.bigq(1L, 1L), as.bigq(2L, 1L), as.bigq(3L, 1L), as.bigq(4L, 1L))
slopes = c(as.bigq(1L, 3L), as.bigq(1L, 4L))
intercepts = c(-as.bigq(1L, 3L), -as.bigq(1L, 4L))
compute_matrix_values(x_init, slopes, intercepts)
# ax+b = (1/3 1/4) %outer% (1, 2, 3, 4) - (1/3 1/4) =
# 1/3 1/4       1/3 1/4         0   0
# 2/3 2/4   -   1/3 1/4   =   1/3 1/4
# 3/3 3/4       1/3 1/4       2/3 2/4
# 4/3 4/4       1/3 1/4       3/3 3/4

## Degenerate examples
x_init0 = NULL
x_init1 = as.bigq(1L, 1L)
# Doing ax+b with one couple (a,b)
slopes1 = as.bigq(1L, 3L)
intercepts1 = -as.bigq(1L, 3L)
compute_matrix_values(x_init0, slopes1, intercepts1) # bigq(0)
compute_matrix_values(x_init1, slopes1, intercepts1) # 0 = 1/3 - 1/3
compute_matrix_values(x_init, slopes1, intercepts1) # t(0, 1/3, 2/3, 1)
# Doing ax+b with two couples (a1,b1), (a2,b2)
compute_matrix_values(x_init0, slopes, intercepts) # bigq(0)
compute_matrix_values(x_init1, slopes, intercepts) # (0, 0)
rm(x_init0, x_init1, x_init, slopes1, slopes, intercepts1, intercepts)

##
# Unit tests
##
# Doing ax+b with one couple (a,b)
x = NULL
a = as.bigq(1L, 3L)
b = -as.bigq(1L, 3L)
checkEquals(compute_matrix_values(x, a, b), a*x+b)

x = as.bigq(1L)
a = as.bigq(1L, 3L)
b = -as.bigq(1L, 3L)
checkEquals(compute_matrix_values(x, a, b), a*x+b)

x_init = c(as.bigq(1L, 1L), as.bigq(2L, 1L), as.bigq(3L, 1L), as.bigq(4L, 1L))
a = as.bigq(1L, 3L)
b = -as.bigq(1L, 3L)
checkEquals(compute_matrix_values(x_init, a, b), a*x_init+b)

# Doing ax+b with two couples (a1,b1), (a2,b2)
slopes = c(as.bigq(1L, 3L), as.bigq(1L, 4L))
intercepts = c(-as.bigq(1L, 3L), -as.bigq(1L, 4L))

x = NULL
checkEquals(compute_matrix_values(x, slopes, intercepts), as.bigq(NULL))

x = as.bigq(1L)
checkEquals(compute_matrix_values(x, slopes, intercepts), c(0,0))

x_init = c(as.bigq(1L, 1L), as.bigq(2L, 1L), as.bigq(3L, 1L), as.bigq(4L, 1L))
checkEquals(compute_matrix_values(x_init, slopes, intercepts),
            matrix(c(as.bigq(c(0,1,2,3))/3, as.bigq(c(0,1,2,3))/4), ncol = 2))

rm(x, a, b, x_init, slopes, intercepts)

#####################################################################
# Evaluate a pushed word with given slopes and intercepts at some x #
#####################################################################
push_matrix_values = function(matrix_values, slopes, intercepts) {
  nb_x = nrow(matrix_values)
  out = rep_mat(slopes, nb_x) * matrix_values + rep_mat(intercepts, nb_x)
  return(out)
}

##
# Examples
##
x_init = c(as.bigq(1L, 1L), as.bigq(2L, 1L), as.bigq(3L, 1L), as.bigq(4L, 1L))
slopes = c(as.bigq(1L, 3L), as.bigq(1L, 4L))
intercepts = c(-as.bigq(1L, 3L), -as.bigq(1L, 4L))

# Matrix values is the matrix for the word related with slopes and intercepts,
# say the word "f"
matrix_values_f = compute_matrix_values(x_init, slopes, intercepts)
# 0    0   
# 1/3  1/4 
# 2/3  1/2 
# 1    3/4 

# Slopes corresponding to another word (say "g")
slopes_g = c(as.bigq(2L, 1L), as.bigq(3L, 1L)) # multiply by 2 and 3
intercepts_g = c(as.bigq(0L, 1L), as.bigq(0L, 1L)) # 0

# We push f on the word "f" to obtain the matrix related to "ff"
push_matrix_values(matrix_values_f, slopes, intercepts)
# -1/3 -1/4       0/9 0/16     1/3 1/4
# -2/9 -3/16      1/9 1/16     1/3 1/4
# -1/9 -1/8   =   2/9 2/16  -  1/3 1/4
# 0    -1/16      3/9 3/16     1/3 1/4

# We push g on the word "f" to obtain the matrix related to "gf"
push_matrix_values(matrix_values_f, slopes_g, intercepts_g)
# 0    0          0/3 0/4        0   0
# 2/3  3/4        2/3 3/4        0   0
# 4/3  3/2    =   4/3 6/4   -    0   0
# 2    9/4        6/3 9/4        0   0

rm(x_init, slopes, intercepts, slopes_g, intercepts_g)

##
# Unit tests
##
x_init0 = NULL
x_init1 = as.bigq(1L, 1L)
x_init = c(as.bigq(1L, 1L), as.bigq(2L, 1L), as.bigq(3L, 1L), as.bigq(4L, 1L))
slopes1 = as.bigq(1L, 3L)
intercepts1 = -as.bigq(1L, 3L)
slopes = c(as.bigq(1L, 3L), as.bigq(1L, 4L))
intercepts = c(-as.bigq(1L, 3L), -as.bigq(1L, 4L))
slopes_g = c(as.bigq(2L, 1L), as.bigq(3L, 1L))
intercepts_g = c(as.bigq(0L, 1L), as.bigq(0L, 1L))
matrix_values01 = compute_matrix_values(x_init0, slopes1, intercepts1)
matrix_values02 = compute_matrix_values(x_init0, slopes, intercepts)
matrix_values11 = compute_matrix_values(x_init1, slopes1, intercepts1)
matrix_values12 = compute_matrix_values(x_init1, slopes, intercepts)
matrix_values21 = compute_matrix_values(x_init, slopes1, intercepts1)
matrix_values_f = compute_matrix_values(x_init, slopes, intercepts)

checkEquals(push_matrix_values(matrix_values01, slopes1, intercepts1),
            as.bigq(NULL))
checkEquals(push_matrix_values(matrix_values02, slopes, intercepts),
            as.bigq(NULL))
checkEquals(push_matrix_values(matrix_values11, slopes1, intercepts1),
            intercepts1)
checkEquals(push_matrix_values(matrix_values12, slopes, intercepts),
            intercepts)
checkEquals(push_matrix_values(matrix_values21, slopes1, intercepts1),
            as.bigq(1L, 3L)*matrix_values21 - as.bigq(1L, 3L))
checkEquals(push_matrix_values(matrix_values_f, slopes, intercepts),
            matrix(c(matrix_values_f[,1]/3, 
                     matrix_values_f[,2]/4), ncol = 2) -
            matrix(c(rep(as.bigq(1L, 3L), 4), 
                     rep(as.bigq(1L, 4L), 4)), ncol = 2))
checkEquals(push_matrix_values(matrix_values_f, slopes_g, intercepts_g),
            matrix(c(2*matrix_values_f[,1], 
                     3*matrix_values_f[,2]), ncol = 2))
rm(x_init0, x_init1, x_init, 
   slopes1, slopes, slopes_g,
   intercepts1, intercepts, intercepts_g,
   matrix_values01, matrix_values02, 
   matrix_values11, matrix_values12,
   matrix_values21, matrix_values_f)

##################################
# Get the related numeric matrix #
##################################
# This function is useful to get the rownames and colnames back.
matrix_values_numeric = function(matrix_values, x_init = NULL) {
  if(length(matrix_values) != 0) {
    mat = matrix(as.numeric(matrix_values), 
                 ncol = ncol(matrix_values), 
                 nrow = nrow(matrix_values))
    if(!is.null(x_init)) {
      rownames(mat) = paste0("x_init=", x_init, ":")
    }
    colnames(mat) = paste("coeffs", 1:ncol(mat))
    # each coeffs is by default a 4-uplet (a, b, c, d), for f: ax+b, g: cx+d
  } else {
    mat = NULL
  }
  return(mat)
}

##
# Examples
##
x_init = c(as.bigq(1L, 1L), as.bigq(2L, 1L), as.bigq(3L, 1L), as.bigq(4L, 1L))
slopes = c(as.bigq(1L, 3L), as.bigq(1L, 4L))
intercepts = c(-as.bigq(1L, 3L), -as.bigq(1L, 4L))
matrix_values_f = compute_matrix_values(x_init, slopes, intercepts)
matrix_values_numeric(matrix_values_f, x_init)
#            coeffs 1 coeffs 2
# x_init=1: 0.0000000     0.00
# x_init=2: 0.3333333     0.25
# x_init=3: 0.6666667     0.50
# x_init=4: 1.0000000     0.75
rm(x_init, slopes, intercepts)

##
# Unit tests
##
x_init0 = NULL
x_init1 = as.bigq(1L, 1L)
x_init = c(as.bigq(1L, 1L), as.bigq(2L, 1L), as.bigq(3L, 1L), as.bigq(4L, 1L))
slopes1 = as.bigq(1L, 3L)
intercepts1 = -as.bigq(1L, 3L)
slopes = c(as.bigq(1L, 3L), as.bigq(1L, 4L))
intercepts = c(-as.bigq(1L, 3L), -as.bigq(1L, 4L))
matrix_values01 = compute_matrix_values(x_init0, slopes1, intercepts1)
matrix_values02 = compute_matrix_values(x_init0, slopes, intercepts)
matrix_values11 = compute_matrix_values(x_init1, slopes1, intercepts1)
matrix_values12 = compute_matrix_values(x_init1, slopes, intercepts)
matrix_values21 = compute_matrix_values(x_init, slopes1, intercepts1)
matrix_values_f = compute_matrix_values(x_init, slopes, intercepts)

checkEquals(matrix_values_numeric(matrix_values01, x_init0), NULL)
checkEquals(matrix_values_numeric(matrix_values02, x_init0), NULL)
checkEquals(dimnames(matrix_values_numeric(matrix_values11, x_init1)),
            list("x_init=1:", "coeffs 1"))
checkEquals(dimnames(matrix_values_numeric(matrix_values12, x_init1)),
            list("x_init=1:", c("coeffs 1", "coeffs 2")))
checkEquals(dimnames(matrix_values_numeric(matrix_values21, x_init)),
            list(c("x_init=1:", "x_init=2:", "x_init=3:", "x_init=4:"),
                 "coeffs 1"))
checkEquals(dimnames(matrix_values_numeric(matrix_values_f, x_init)),
            list(c("x_init=1:", "x_init=2:", "x_init=3:", "x_init=4:"),
                 c("coeffs 1", "coeffs 2")))
rm(x_init0, x_init1, x_init, 
   slopes1, slopes,
   intercepts1, intercepts,
   matrix_values01, matrix_values02, 
   matrix_values11, matrix_values12,
   matrix_values21, matrix_values_f)

############################################
# Get (row/col) position from idx position #
############################################
# This function is needed because M[idx] is not implemented correctly in gmp,
# and this is explained in the documentation
# The index is counted column by column from 1
get_pos = function(idx, nrow, ncol) {
  if(is.null(nrow) | is.null(ncol)) {
    return(NA)
  } else if(nrow == 0 | ncol == 0 | idx <= 0) {
    return(NA)
  } 

  get_row = idx %% nrow
  if(get_row == 0) {
    get_row = nrow
  }
  get_col = 1 + (idx - get_row)/nrow
  return(c(get_row, get_col))
}

##
# Examples
##
# From a matrix 4 x 2, get position
# of index 1
get_pos(1, 4, 2) # Output: 1st row, 1st column
# of index 2
get_pos(2, 4, 2) # Output: 2nd row, 1st column
# of index 5
get_pos(5, 4, 2) # Output: 1st row, 2nd column
# of index 8
get_pos(8, 4, 2) # Output: 4th row, 2nd column
# Note: 1 5
#       2 6
#       3 7
#       4 8

# For a matrix 3 x 5, get position of index 10
get_pos(10, 3, 5) # Output: 1st row, 4th column
# Note: 1  4  7 '10' 13
#       2  5  8  11  14
#       3  6  9  12  15

##
# Unit tests
##
checkEquals(get_pos(1, 4, 2), c(1,1))
checkEquals(get_pos(2, 4, 2), c(2,1))
checkEquals(get_pos(5, 4, 2), c(1,2))
checkEquals(get_pos(8, 4, 2), c(4,2))
checkEquals(get_pos(10, 3, 5), c(1,4))
checkEquals(get_pos(1, 1, 1), c(1,1))
checkEquals(get_pos(0, 0, 0), NA)
checkEquals(get_pos(integer(0), NULL, NULL), NA)

####################################################
# Replace non integer values of the matrix with NA #
####################################################
to_integer_mat = function(matrix_values) {
  # Note: gmp:: to prevent conflict with distr package
  idx = which(!gmp::is.whole(matrix_values))
  row_col_idx = sapply(idx, get_pos, nrow(matrix_values), ncol(matrix_values))

  if(!is.null(ncol(row_col_idx))) {
    for(i in 1:ncol(row_col_idx)) {
      matrix_values[row_col_idx[1,i], row_col_idx[2,i]] = NA
    }
  }
  return(matrix_values)
}

##
# Examples
##
x_init = c(as.bigq(1L, 1L), as.bigq(2L, 1L), as.bigq(3L, 1L), as.bigq(4L, 1L))
slopes = c(as.bigq(1L, 3L), as.bigq(1L, 4L))
intercepts = c(-as.bigq(1L, 3L), -as.bigq(1L, 4L))
slopes_g = c(as.bigq(2L, 1L), as.bigq(3L, 1L))
intercepts_g = c(as.bigq(0L, 1L), as.bigq(0L, 1L))

matrix_values_f = compute_matrix_values(x_init, slopes, intercepts)
matrix_values_ff = push_matrix_values(matrix_values_f, slopes, intercepts)
matrix_values_gf = push_matrix_values(matrix_values_f, slopes_g, intercepts_g)

to_integer_mat(matrix_values_f)
#      0   0      0  0
# From 1/3 1/4 to NA NA
#      2/3 1/2    NA NA
#      1   3/4    1  NA

to_integer_mat(matrix_values_ff)
#      -1/3 -1/4     NA NA
# From -2/9 -3/16 to NA NA
#      -1/9 -1/8     NA NA
#         0 -1/16     0 NA

to_integer_mat(matrix_values_gf)
#      0   0      0  0
# From 2/3 3/4 to NA NA
#      4/3 3/2    NA NA
#      2   9/4    2  NA

##
# Unit tests
##
x_init0 = NULL
x_init1 = as.bigq(1L, 1L)
x_init = c(as.bigq(1L, 1L), as.bigq(2L, 1L), as.bigq(3L, 1L), as.bigq(4L, 1L))
slopes1 = as.bigq(1L, 3L)
intercepts1 = -as.bigq(1L, 3L)
slopes = c(as.bigq(1L, 3L), as.bigq(1L, 4L))
intercepts = c(-as.bigq(1L, 3L), -as.bigq(1L, 4L))
slopes_g = c(as.bigq(2L, 1L), as.bigq(3L, 1L))
intercepts_g = c(as.bigq(0L, 1L), as.bigq(0L, 1L))
matrix_values01 = compute_matrix_values(x_init0, slopes1, intercepts1)
matrix_values02 = compute_matrix_values(x_init0, slopes, intercepts)
matrix_values11 = compute_matrix_values(x_init1, slopes1, intercepts1)
matrix_values12 = compute_matrix_values(x_init1, slopes, intercepts)
matrix_values21 = compute_matrix_values(x_init, slopes1, intercepts1)
matrix_values_f = compute_matrix_values(x_init, slopes, intercepts)
matrix_values_ff = push_matrix_values(matrix_values_f, slopes, intercepts)
matrix_values_gf = push_matrix_values(matrix_values_f, slopes_g, intercepts_g)

checkEquals(to_integer_mat(matrix_values01), as.bigq(NULL))
checkEquals(to_integer_mat(matrix_values02), as.bigq(NULL))
checkEquals(to_integer_mat(matrix_values11), 0)
checkEquals(to_integer_mat(matrix_values12), c(0, 0))
checkEquals(to_integer_mat(matrix_values21), c(0, NA, NA, 1))
checkEquals(to_integer_mat(matrix_values_f), 
            matrix(c(0, NA, NA, 1, 0, NA, NA, NA), ncol = 2))
checkEquals(to_integer_mat(matrix_values_ff), 
            matrix(c(NA, NA, NA, 0, NA, NA, NA, NA), ncol = 2))
checkEquals(to_integer_mat(matrix_values_gf), 
            matrix(c(0, NA, NA, 2, 0, NA, NA, NA), ncol = 2))

rm(x_init0, x_init1, x_init, 
   slopes1, slopes, slopes_g,
   intercepts1, intercepts, intercepts_g,
   matrix_values01, matrix_values02, 
   matrix_values11, matrix_values12,
   matrix_values21, matrix_values_f,
   matrix_values_ff, matrix_values_gf)

##########################################
# Say if the matrix is full of NA or not #
##########################################
# If the matrix is NA, we may want to delete the related word
is_NA_mat = function(matrix_values) {
  return(all(is.na(matrix_values)))
}

##
# Examples
##
x_init = c(as.bigq(1L, 1L), as.bigq(2L, 1L), as.bigq(3L, 1L), as.bigq(4L, 1L))
slopes = c(as.bigq(1L, 3L), as.bigq(1L, 4L))
intercepts = c(-as.bigq(1L, 3L), -as.bigq(1L, 4L))
slopes_g = c(as.bigq(2L, 1L), as.bigq(3L, 1L))
intercepts_g = c(as.bigq(0L, 1L), as.bigq(0L, 1L))

matrix_values_f = compute_matrix_values(x_init, slopes, intercepts)
matrix_values_f = to_integer_mat(matrix_values_f)

matrix_values_ff = push_matrix_values(matrix_values_f, slopes, intercepts)
matrix_values_ff = to_integer_mat(matrix_values_ff)

matrix_values_gf = push_matrix_values(matrix_values_f, slopes_g, intercepts_g)
matrix_values_gf =to_integer_mat(matrix_values_gf)
  
matrix_values_fff = push_matrix_values(matrix_values_ff, slopes, intercepts)
matrix_values_fff =to_integer_mat(matrix_values_fff)

matrix_values_fgf = push_matrix_values(matrix_values_gf, slopes, intercepts)
matrix_values_fgf =to_integer_mat(matrix_values_fgf)

is_NA_mat(matrix_values_f) # FALSE
is_NA_mat(matrix_values_ff) # FALSE
is_NA_mat(matrix_values_gf) # FALSE
is_NA_mat(matrix_values_fff) # TRUE
is_NA_mat(matrix_values_fgf) # TRUE

rm(x_init, 
   slopes, intercepts,
   slopes_g, intercepts_g,
   matrix_values_f, 
   matrix_values_ff, matrix_values_gf, 
   matrix_values_fff, matrix_values_fgf)
##
# Unit tests
##
x_init = c(as.bigq(1L, 1L), as.bigq(2L, 1L), as.bigq(3L, 1L), as.bigq(4L, 1L))
slopes = c(as.bigq(1L, 3L), as.bigq(1L, 4L))
intercepts = c(-as.bigq(1L, 3L), -as.bigq(1L, 4L))
slopes_g = c(as.bigq(2L, 1L), as.bigq(3L, 1L))
intercepts_g = c(as.bigq(0L, 1L), as.bigq(0L, 1L))

matrix_values_f = compute_matrix_values(x_init, slopes, intercepts)
matrix_values_f = to_integer_mat(matrix_values_f)

matrix_values_ff = push_matrix_values(matrix_values_f, slopes, intercepts)
matrix_values_ff = to_integer_mat(matrix_values_ff)

matrix_values_gf = push_matrix_values(matrix_values_f, slopes_g, intercepts_g)
matrix_values_gf = to_integer_mat(matrix_values_gf)

matrix_values_fff = push_matrix_values(matrix_values_ff, slopes, intercepts)
matrix_values_fff = to_integer_mat(matrix_values_fff)

matrix_values_fgf = push_matrix_values(matrix_values_gf, slopes, intercepts)
matrix_values_fgf = to_integer_mat(matrix_values_fgf)

checkEquals(is_NA_mat(matrix_values_f), FALSE)
checkEquals(is_NA_mat(matrix_values_ff), FALSE)
checkEquals(is_NA_mat(matrix_values_gf), FALSE)
checkEquals(is_NA_mat(matrix_values_fff), TRUE)
checkEquals(is_NA_mat(matrix_values_fgf), TRUE)

rm(x_init, 
   slopes, intercepts,
   slopes_g, intercepts_g,
   matrix_values_f, 
   matrix_values_ff, matrix_values_gf, 
   matrix_values_fff, matrix_values_fgf)