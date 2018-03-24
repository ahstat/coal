####################################
# Linear function class definition #
####################################
# A linear function is: 
# - a character for the slope (for example "a"); and
# - a character for the intercept (for example "b").
# Those two define the formal function: ax + b.
# - a vector of biq values related with the slope character,
# - a vector of same length related with the intercept.
# Those two define the related numeric values.
setClass("Linear function", representation(coeff_x = "character", 
                                           coeff_1 = "character",
                                           seq_coeff_x = "bigq", 
                                           seq_coeff_1 = "bigq"))

# Recall: as.bigq(8000L, 21L) is the rational 8000/21
#         as.bigq(10L) is the rational 10

#########################################
# Linear function initialization method #
#########################################
setMethod("initialize", signature  = "Linear function",
          definition = function(.Object,
                                coeff_x = "a",
                                coeff_1 = "b",
                                seq_coeff_x = as.bigq(1L, 3L),
                                seq_coeff_1 = as.bigq(-1L, 3L)) {
            ##
            # Initialize slots (=attributes)
            ##
            .Object@coeff_x = coeff_x
            .Object@coeff_1 = coeff_1
            .Object@seq_coeff_x = seq_coeff_x
            .Object@seq_coeff_1 = seq_coeff_1

            ##
            # Check validity
            ##
            ## Check that seq_coeff_x and seq_coeff_1 have the same size
            if(length(seq_coeff_x) != length(seq_coeff_1)) {
              stop("Need same size for seq_coeff_x and seq_coeff_1. ",
                   "You can use expand.grid to get all combinations.")
            }
            
            return (.Object)
          }
)

##
# Examples
##
# The function ax + b = (1/3)x - 1/3
new("Linear function", "a", "b", as.bigq(1L, 3L), -as.bigq(1L, 3L))

# The function cx + d for three possible couples:
# (1/3)x + 2 ; x + 2/3 ; 5x - 1/4
seq_coeff_x = c(as.bigq(1L, 3L), as.bigq(1L, 1L), as.bigq(5L, 1L))
seq_coeff_1 = c(as.bigq(2L, 1L), as.bigq(2L, 3L), -as.bigq(1L, 4L))
new("Linear function", "c", "d", seq_coeff_x, seq_coeff_1)
rm(seq_coeff_x, seq_coeff_1)

##########
# Base f #
##########
# f and g as defined in Collatz conjecture (going upwards on the tree)
# f_base is the function (x - 1)/3 = (1/3)x - 1/3
f_base = new("Linear function", "a", "b", as.bigq(1L, 3L), -as.bigq(1L, 3L))

##########
# Base g #
##########
# f and g as defined in Collatz conjecture (going upwards on the tree)
# g_base is the function 2x = 2x + 0
g_base = new("Linear function", "c", "d", as.bigq(2L), as.bigq(0L))

#################
# id_n function #
#################
# Create the identify function x = 1x + 0
# The parameter n is the size of the related vectors, used to do composition
# with other linear functions.
id_n = function(n = 1) {
  id = new("Linear function", coeff_x = "1", coeff_1 = "0",
           seq_coeff_x = as.bigq(rep(1, n)), 
           seq_coeff_1 = as.bigq(rep(0, n)))
  return(id)
}

##
# Examples
##
# The identity function x
id_n(1)

# The identity function x three times:
# 1x + 0 ; 1x + 0 ; 1x + 0
id_n(3)

#############################
# is_defined_as_id function #
#############################
# Test if the linear function is *defined* as id or not
#
# Import remark: we just check if the linear function is defined as id, not
# if the linear function is a composition leading to id.
# If we need to check identity, we use is_id function instead.
is_defined_as_id = function(linfunc) {
  if(linfunc@coeff_x == "1" & linfunc@coeff_1 == "0") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

##
# Examples
##
is_defined_as_id(id_n(1)) # TRUE
is_defined_as_id(id_n(3)) # TRUE
is_defined_as_id(f_base) # FALSE
is_defined_as_id(g_base) # FALSE

# We just check that the linear function is *defined* as id.
# If the linear function has a vector of seq_coeff_x = 1 and of
# seq_coeff_1 = 0, is_defined_as_id will be FALSE unless the function has been
# explicitly defined as the identity function.
# So, for example:
# - The composition of f with its inverse F will be id, but is_defined_as_id 
# will be FALSE.
# - If we take f = 2 * x + 1 ; g = x / 4 - 3/4, we have
# gff = (2*(2*x+1)+1)/4 - 3/4 = (4x + 3)/4 - 3/4 = x + 3/4 - 3/4 = x,
# but is_defined_as_id of this composition will be FALSE.
test_lin = new("Linear function", "a", "b", as.bigq(1L, 1L), as.bigq(0L, 1L))
is_defined_as_id(test_lin) # FALSE
rm(test_lin)

##
# Unit tests
##
checkEqualsNumeric(is_defined_as_id(id_n(1)), TRUE)
checkEqualsNumeric(is_defined_as_id(id_n(3)), TRUE)
checkEqualsNumeric(is_defined_as_id(f_base), FALSE)
checkEqualsNumeric(is_defined_as_id(g_base), FALSE)
test_lin = new("Linear function", "a", "b", as.bigq(1L, 1L), as.bigq(0L, 1L))
checkEqualsNumeric(is_defined_as_id(test_lin), FALSE)
rm(test_lin)

########################
# get_inverse function #
########################
# Given a linear function, compute the inverse of this function.
# The inverse function is not defined when the slope is 0 (i.e. a = 0)
#
# By convention, we write upward operations on the tree with lowercase letters
# (such as f = ax+b), and the downward inverse operations are written with
# related uppercase letters (in the example F = Ax+B).
get_inverse = function(linfunc) {
  coeff_x_inverse = linfunc@coeff_x
  if(coeff_x_inverse %in% letters) {
    coeff_x_inverse = toupper(coeff_x_inverse)
  } else {
    coeff_x_inverse = tolower(coeff_x_inverse)
  }
  
  coeff_1_inverse = linfunc@coeff_1
  if(coeff_1_inverse %in% letters) {
    coeff_1_inverse = toupper(coeff_1_inverse)
  } else {
    coeff_1_inverse = tolower(coeff_1_inverse)
  }
  
  if(sum(as.bigq(0) == linfunc@seq_coeff_x) > 0) {
    stop("seq_coeff_x cannot be 0, because we need its inverse")
  }
  
  # If y = ax+b, then x = y/a - b/a
  # So coefficients are 1/seq_coeff_x and -seq_coeff_1/seq_coeff_x
  linfunc_out = new("Linear function", 
                    coeff_x_inverse, 
                    coeff_1_inverse,
                    1/linfunc@seq_coeff_x, 
                    -linfunc@seq_coeff_1/linfunc@seq_coeff_x)
  return(linfunc_out)
}

##########
# Base F #
##########
F_base = get_inverse(f_base) # Ax + B = 3x + 1

##########
# Base G #
##########
G_base = get_inverse(g_base) # Cx + D = (1/2)x + 0

##
# Examples
##
F_base
G_base
get_inverse(id_n(3)) # the inverse of id is id

##
# Unit tests
##
checkEqualsNumeric(F_base@coeff_x, "A")
checkEqualsNumeric(F_base@coeff_1, "B")
checkEqualsNumeric(F_base@seq_coeff_x, 1/f_base@seq_coeff_x)
checkEqualsNumeric(F_base@seq_coeff_1, as.bigq(1L))

checkEqualsNumeric(G_base@coeff_x, "C")
checkEqualsNumeric(G_base@coeff_1, "D")
checkEqualsNumeric(G_base@seq_coeff_x, 1/g_base@seq_coeff_x)
checkEqualsNumeric(G_base@seq_coeff_1, as.bigq(0L))

checkEqualsNumeric(get_inverse(F_base)@coeff_x, f_base@coeff_x)
checkEqualsNumeric(get_inverse(F_base)@coeff_1, f_base@coeff_1)
checkEqualsNumeric(get_inverse(F_base)@seq_coeff_x, f_base@seq_coeff_x)
checkEqualsNumeric(get_inverse(F_base)@seq_coeff_1, f_base@seq_coeff_1)

checkEqualsNumeric(get_inverse(G_base)@coeff_x, g_base@coeff_x)
checkEqualsNumeric(get_inverse(G_base)@coeff_1, g_base@coeff_1)
checkEqualsNumeric(get_inverse(G_base)@seq_coeff_x, g_base@seq_coeff_x)
checkEqualsNumeric(get_inverse(G_base)@seq_coeff_1, g_base@seq_coeff_1)

# The function cx + d for three possible couples:
# (1/3)x + 2 ; x + 2/3 ; 5x - 1/4
seq_coeff_x = c(as.bigq(1L, 3L), as.bigq(1L, 1L), as.bigq(5L, 1L))
seq_coeff_1 = c(as.bigq(2L, 1L), as.bigq(2L, 3L), -as.bigq(1L, 4L))
test_lin = new("Linear function", "c", "d", seq_coeff_x, seq_coeff_1)

# The inverse function Cx+D has also three couples:
# 3x - 6 ; x - 2/3 ; x/5 + 1/20
test_lin_inv = get_inverse(test_lin)
seq_coeff_x_inv = c(as.bigq(3L, 1L), as.bigq(1L, 1L), as.bigq(1L, 5L))
seq_coeff_1_inv = c(-as.bigq(6L, 1L), -as.bigq(2L, 3L), as.bigq(1L, 20L))

checkEqualsNumeric(test_lin_inv@coeff_x, "C")
checkEqualsNumeric(test_lin_inv@coeff_1, "D")
checkEqualsNumeric(test_lin_inv@seq_coeff_x, seq_coeff_x_inv)
checkEqualsNumeric(test_lin_inv@seq_coeff_1, seq_coeff_1_inv)

rm(test_lin, seq_coeff_x, seq_coeff_1, 
   test_lin_inv, seq_coeff_x_inv, seq_coeff_1_inv)

##########################
# Formal printing method #
##########################
# Print the formal linear function, on the form 'slope'x + 'intercept'
setGeneric("printf", function(object) {
  standardGeneric("printf")
})

setMethod("printf", "Linear function", function(object) {
  return(paste0(object@coeff_x, "x + ", object@coeff_1))
})

##
# Examples
##
printf(f_base) # "ax + b"
printf(g_base) # "cx + d"
printf(F_base) # "Ax + B"
printf(G_base) # "Cx + D"
printf(id_n(3)) # "1x + 0"

##
# Unit tests
##
checkEquals(printf(f_base), "ax + b")
checkEquals(printf(g_base), "cx + d")
checkEquals(printf(F_base), "Ax + B")
checkEquals(printf(G_base), "Cx + D")
checkEquals(printf(id_n(3)), "1x + 0")

#########################
# Splitting coefficient #
#########################
# Split a formal coefficient at each '+' element.
# Note: the similar elements are not grouped, we only split.
# Note: the minus sign is not managed, so we forbid using the minus sign.
split_coeff = function(coeff) {
  # Check that there is no any minus element.
  coeff_has_minus = grepl("-", coeff, fixed = TRUE)
  if(coeff_has_minus) {
    stop("Formal operations with minus sign not implemented. ",
         "To do so, one may change at least split_coeff and ",
         "poly_expansion functions in 1_linear_function.R.")
  }
  
  # Split each '+'
  out = strsplit(coeff, "+", fixed = TRUE)[[1]]
  return(out)
}

##
# Examples
##
split_coeff("b") # b
split_coeff("b+b") # b b
split_coeff("ac+bc+cd") # ac bc cd
split_coeff("a+a+ba+ab") # a a ba ab
# split_coeff("ba-ab") # Throw an error, because there is a minus sign.

##
# Unit tests
##
checkEquals(split_coeff("b"), "b")
checkEquals(split_coeff("b+b"), c("b", "b"))
checkEquals(split_coeff("ac+bc+cd"), c("ac", "bc", "cd"))
checkEquals(split_coeff("a+a+ba+ab"), c("a", "a", "ba", "ab"))

########################
# Polynomial expansion #
########################
# Perform the polynomial expansion of two coefficients, formally
# Note: the similar elements are not grouped.
# Note: the minus sign is not managed (throwing error if any)
poly_expansion = function(coeff1, coeff2) {
  split1 = split_coeff(coeff1)
  split2 = split_coeff(coeff2)
  out = sapply(split1, function(x) {paste0(x, split2, collapse = "+")})
  out = paste0(out, collapse = "+")
  return(out)
}

##
# Examples
##
poly_expansion("a", "c") # ac
poly_expansion("a", "c+d") # ac+ad
poly_expansion("a+b", "c") # ac+bc
poly_expansion("a+b", "c+d") # ac+ad+bc+bd
poly_expansion("a+b", "b+b") # ab+ab+bb+bb
poly_expansion("a+b+c", "d+e") # ad+ae+bd+be+cd+ce
poly_expansion("a+b+c", "d+e+f") # ad+ae+af+bd+be+bf+cd+ce+cf
# poly_expansion("a-b", "c-d") # Throw an error, because there is a minus sign.

##
# Unit tests
##
checkEquals(poly_expansion("a", "c"), "ac")
checkEquals(poly_expansion("a", "c+d"), "ac+ad")
checkEquals(poly_expansion("a+b", "c"), "ac+bc")
checkEquals(poly_expansion("a+b", "c+d"), "ac+ad+bc+bd")
checkEquals(poly_expansion("a+b", "b+b"), "ab+ab+bb+bb")
checkEquals(poly_expansion("a+b+c", "d+e"), "ad+ae+bd+be+cd+ce")
checkEquals(poly_expansion("a+b+c", "d+e+f"), "ad+ae+af+bd+be+bf+cd+ce+cf")

###################################
# Formal composition of functions #
###################################
# Composition g o f of f = ax+b with g = cx+d
# g o f = c(ax+b)+d = cax + cb + d
# Note: the similar elements are not grouped.
# Note: the minus sign is not managed (throwing error if any)
compo_formal = function(a, b, c, d) {
  # coeff_x is: "ca"
  coeff_x = poly_expansion(c, a)

  # coeff_1 is "cb + d"
  coeff_1_develop = poly_expansion(c, b)
  coeff_1 = paste(coeff_1_develop, d, sep = "+")

  return(c(coeff_x, coeff_1))
}

##
# Examples
##
compo_formal("a", "b", "c", "d") # "ca" "cb+d"
compo_formal("a+k", "b", "c", "d") # "ca+ck" "cb+d"
compo_formal("a", "b+k", "c", "d") # "ca" "cb+ck+d"
compo_formal("a", "b", "c+k", "d") # "ca+ka" "cb+kb+d"
compo_formal("a", "b", "c", "d+k") # "ca" "cb+d+k"
compo_formal("a+A", "b+B", "c+C", "d+D") # "ca+cA+Ca+CA" "cb+cB+Cb+CB+d+D"
# compo_formal("a", "b", "-c", "d") # Throw an error, because of minus sign.

##
# Unit tests
##
checkEquals(compo_formal("a", "b", "c", "d"), c("ca", "cb+d"))
checkEquals(compo_formal("a+k", "b", "c", "d"), c("ca+ck", "cb+d"))
checkEquals(compo_formal("a", "b+k", "c", "d"), c("ca", "cb+ck+d"))
checkEquals(compo_formal("a", "b", "c+k", "d"), c("ca+ka", "cb+kb+d"))
checkEquals(compo_formal("a", "b", "c", "d+k"), c("ca", "cb+d+k"))
checkEquals(compo_formal("a+A", "b+B", "c+C", "d+D"), 
            c("ca+cA+Ca+CA", "cb+cB+Cb+CB+d+D"))

####################################
# Numeric composition of functions #
####################################
# Composition g o f of f = ax+b with g = cx+d
# g o f = c(ax+b)+d = cax + cb + d
# All is numeric, so it is far easier compared to formal composition.
compo_numeric = function(a_num, b_num, c_num, d_num) {
  seq_coeff_x = c_num * a_num
  seq_coeff_1 = c_num * b_num + d_num
  return(list(seq_coeff_x, seq_coeff_1))
}

##
# Examples
##
## First define some triplets functions:
# The function cx + d for three possible couples:
# (1/3)x + 2 ; x + 2/3 ; 5x - 1/4
seq_coeff_x = c(as.bigq(1L, 3L), as.bigq(1L, 1L), as.bigq(5L, 1L))
seq_coeff_1 = c(as.bigq(2L, 1L), as.bigq(2L, 3L), -as.bigq(1L, 4L))

# The inverse function Cx+D has also three couples:
# 3x - 6 ; x - 2/3 ; x/5 + 1/20
seq_coeff_x_inv = c(as.bigq(3L, 1L), as.bigq(1L, 1L), as.bigq(1L, 5L))
seq_coeff_1_inv = c(-as.bigq(6L, 1L), -as.bigq(2L, 3L), as.bigq(1L, 20L))

# Other triplet of functions:
# (1/2)x - 1/5 ; 2x - 7/4 ; 7x + 1/2
seq_coeff_x_bis = c(as.bigq(1L, 2L), as.bigq(2L, 1L), as.bigq(7L, 1L))
seq_coeff_1_bis = c(-as.bigq(1L, 5L), -as.bigq(7L, 4L), as.bigq(1L, 2L))

## Then perform compositions:
# Composition of 3 linear functions with their inverses, output is id coeffs.
compo_numeric(seq_coeff_x, seq_coeff_1, seq_coeff_x_inv, seq_coeff_1_inv)

# Take f = (1/3)x + 2 ; x + 2/3 ; 5x - 1/4, 
# then g = (1/2)x - 1/5 ; 2x - 7/4 ; 7x + 1/2.
# g o f = (1/2)((1/3)x + 2) - 1/5 ; 2(x + 2/3) - 7/4 ; 7(5x - 1/4) + 1/2
#       = (1/6)x + 4/5 ; 2x - 5/12 ; 35x - 5/4
compo_numeric(seq_coeff_x, seq_coeff_1, seq_coeff_x_bis, seq_coeff_1_bis)

# Take g = (1/2)x - 1/5 ; 2x - 7/4 ; 7x + 1/2,
# then f = (1/3)x + 2 ; x + 2/3 ; 5x - 1/4.
# f o g = (1/3)((1/2)x - 1/5) + 2 ; 2x - 7/4 + 2/3 ; 5(7x + 1/2) - 1/4
#       = (1/6)x + 29/15 ; 2x - 13/12 ; 35x + 9/4
compo_numeric(seq_coeff_x_bis, seq_coeff_1_bis, seq_coeff_x, seq_coeff_1)

rm(seq_coeff_x, seq_coeff_1,
   seq_coeff_x_inv, seq_coeff_1_inv,
   seq_coeff_x_bis, seq_coeff_1_bis)

##
# Unit tests
##
# See examples for explanations
seq_coeff_x = c(as.bigq(1L, 3L), as.bigq(1L, 1L), as.bigq(5L, 1L))
seq_coeff_1 = c(as.bigq(2L, 1L), as.bigq(2L, 3L), -as.bigq(1L, 4L))
seq_coeff_x_inv = c(as.bigq(3L, 1L), as.bigq(1L, 1L), as.bigq(1L, 5L))
seq_coeff_1_inv = c(-as.bigq(6L, 1L), -as.bigq(2L, 3L), as.bigq(1L, 20L))
seq_coeff_x_bis = c(as.bigq(1L, 2L), as.bigq(2L, 1L), as.bigq(7L, 1L))
seq_coeff_1_bis = c(-as.bigq(1L, 5L), -as.bigq(7L, 4L), as.bigq(1L, 2L))

cp1 = compo_numeric(seq_coeff_x, seq_coeff_1, seq_coeff_x_inv, seq_coeff_1_inv)
cp2 = compo_numeric(seq_coeff_x, seq_coeff_1, seq_coeff_x_bis, seq_coeff_1_bis)
cp3 = compo_numeric(seq_coeff_x_bis, seq_coeff_1_bis, seq_coeff_x, seq_coeff_1)

checkEquals(cp1[[1]], c(as.bigq(1L, 1L), as.bigq(1L, 1L), as.bigq(1L, 1L)))
checkEquals(cp1[[2]], c(as.bigq(0L, 1L), as.bigq(0L, 1L), as.bigq(0L, 1L)))
checkEquals(cp2[[1]], c(as.bigq(1L, 6L), as.bigq(2L, 1L), as.bigq(35L, 1L)))
checkEquals(cp2[[2]], c(as.bigq(4L, 5L), -as.bigq(5L, 12L), -as.bigq(5L, 4L)))
checkEquals(cp3[[1]], c(as.bigq(1L, 6L), as.bigq(2L, 1L), as.bigq(35L, 1L)))
checkEquals(cp3[[2]], 
            c(as.bigq(29L, 15L), -as.bigq(13L, 12L), as.bigq(9L, 4L)))

rm(seq_coeff_x, seq_coeff_1, 
   seq_coeff_x_inv, seq_coeff_1_inv, 
   seq_coeff_x_bis, seq_coeff_1_bis,
   cp1, cp2, cp3)

###################################
# Composition of functions method #
###################################
# Perform composition of two linear functions.
# compo(f, g) means g o f, that is first apply f, then apply g.
# The composition is done formally on the letters (coeff_x and coeff_1),
# and also on the numeric values (seq_coeff_x and seq_coeff_1).
#
# To have standard notation, we define the operator %o% as the composition
# operator: g %o% f means g o f.
setGeneric("compo", function(f, g) {
  standardGeneric("compo")
})

# compo(f, g) = g o f (first apply f, then apply g)
setMethod("compo", signature("Linear function", "Linear function"), 
          function(f, g) {
  ## Formal composition
  # We first check if f or g is defined as the id function,
  # in that case we define id o f = f and g o id = g.
  # Otherwise, we extract the formal composition coefficients with compo_formal
  if(is_defined_as_id(f)) {
    coeff_x = g@coeff_x
    coeff_1 = g@coeff_1
  } else if(is_defined_as_id(g)) {
    coeff_x = f@coeff_x
    coeff_1 = f@coeff_1
  } else {
  
    a = f@coeff_x
    b = f@coeff_1
    c = g@coeff_x
    d = g@coeff_1
    compo_formal_out = compo_formal(a, b, c, d)
    coeff_x = compo_formal_out[1]
    coeff_1 = compo_formal_out[2]
  }
  
  ## Numeric composition
  a_num = f@seq_coeff_x
  b_num = f@seq_coeff_1
  c_num = g@seq_coeff_x
  d_num = g@seq_coeff_1
  compo_numeric_out = compo_numeric(a_num, b_num, c_num, d_num)
  seq_coeff_x = compo_numeric_out[[1]]
  seq_coeff_1 = compo_numeric_out[[2]]
  
  gof = new("Linear function", 
            coeff_x = coeff_x, coeff_1 = coeff_1,
            seq_coeff_x = seq_coeff_x, seq_coeff_1 = seq_coeff_1)
  return(gof)
})

################
# %o% function #
################
`%o%` = function(g, f) {
  return(compo(f, g))
}

##
# Examples
##
## Simple example
f = new("Linear function", coeff_x = "a", coeff_1 = "b",
        seq_coeff_x = as.bigq(c(3, 6)), seq_coeff_1 = as.bigq(c(4, 2)))
g = new("Linear function", coeff_x = "c", coeff_1 = "d",
        seq_coeff_x = as.bigq(c(5, 9)), seq_coeff_1 = as.bigq(c(7, 8)))

printf(f) # ax + b
printf(g) # cx + d
printf(f %o% f) # aax + ab+b
printf(g %o% f) # cax + cb+d
printf(f %o% g) # acx + ad+b
printf(g %o% g) # ccx + cd+d
printf(g %o% f %o% f) # caax + cab+cb+d (ok cf associativity of %o%)
(g %o% f %o% f)@seq_coeff_x # 45 and 324
(g %o% f %o% f)@seq_coeff_1 # 87 and 134
rm(f, g)

## Default value
new("Linear function")

## Example with expand.grid
seq_current = as.bigq(seq(from = 0, to = 2, by = 1))
coeffs = expand.grid(d = seq_current,
                     c = seq_current,
                     b = seq_current,
                     a = seq_current)
coeffs = coeffs[,c("a", "b", "c", "d")]

f = new("Linear function", coeff_x = "a", coeff_1 = "b",
        seq_coeff_x = coeffs$a, seq_coeff_1 = coeffs$b)
g = new("Linear function", coeff_x = "c", coeff_1 = "d",
        seq_coeff_x = coeffs$c, seq_coeff_1 = coeffs$d)
id = id_n(nrow(coeffs))

# You can observe linobj@coeff_x and linobj@coeff_1 for each following linobj
f
g
g %o% f
f %o% g
g %o% f %o% f
g %o% f %o% f %o% f %o% g %o% g %o% g %o% f

rm(seq_current, coeffs, f, g, id)

##
# Unit tests
##
f <- new("Linear function", coeff_x = "a", coeff_1 = "b",
         seq_coeff_x = as.bigq(c(3, 6)), seq_coeff_1 = as.bigq(c(4, 2)))
g <- new("Linear function", coeff_x = "c", coeff_1 = "d",
         seq_coeff_x = as.bigq(c(5, 9)), seq_coeff_1 = as.bigq(c(7, 8)))
id = id_n(2)

checkEquals(printf(id), "1x + 0")
checkEquals(printf(f), "ax + b")
checkEquals(printf(g), "cx + d")
checkEquals(printf(f %o% id), "ax + b")
checkEquals(printf(id %o% f), "ax + b")
checkEquals(printf(f %o% f), "aax + ab+b")
checkEquals(printf(g %o% f), "cax + cb+d")
checkEquals(printf(f %o% g), "acx + ad+b")
checkEquals(printf(g %o% g), "ccx + cd+d")
checkEquals(printf(g %o% f %o% f), "caax + cab+cb+d")

checkEquals((f %o% id)@seq_coeff_x, f@seq_coeff_x)
checkEquals((id %o% f)@seq_coeff_x, f@seq_coeff_x)
checkEquals((f %o% f)@seq_coeff_x, (f@seq_coeff_x)^2)
checkEquals((g %o% g)@seq_coeff_x, (g@seq_coeff_x)^2)
checkEquals((f %o% g)@seq_coeff_x, f@seq_coeff_x * g@seq_coeff_x)
checkEquals((g %o% f)@seq_coeff_x, f@seq_coeff_x * g@seq_coeff_x)
checkEquals((g %o% f %o% f)@seq_coeff_x, g@seq_coeff_x*(f@seq_coeff_x)^2)

a = c(3, 6)
b = c(4, 2)
c = c(5, 9)
d = c(7, 8)
checkEquals((f %o% id)@seq_coeff_1, f@seq_coeff_1)
checkEquals((id %o% f)@seq_coeff_1, f@seq_coeff_1)
checkEquals((f %o% f)@seq_coeff_1, a*b+b)
checkEquals((g %o% f)@seq_coeff_1, c*b+d)
checkEquals((f %o% g)@seq_coeff_1, a*d+b)
checkEquals((g %o% g)@seq_coeff_1, c*d+d)
checkEquals((g %o% f %o% f)@seq_coeff_1, c*a*b+c*b+d)
rm(a, b, c, d, f, g, id)

########################################
# Convert a word into a numeric vector #
########################################
# Convert a word (such as "ffgf") into a numeric vector of R^k, 
# where k is the length of each word. 
# For example, "ffgf" is converted to 1 2 1 1. This can be read as follows:
# the first coordinate is 1 (to apply f), the second is 2 (to apply g), etc.
# Note again this is not a number but a vector of R^k.
#
# The vocabulary is mapped from `voc` to 1:length(voc)
# This is the good way to map the vocabulary because:
# - We want to use 0 to complete vector from R^i to R^n with n>i,
# - Using minus such as -1 only works for a vocabulary of size 2, also we
# may want to keep minus sign for the inverse like F = f^{-1} or G = g^{-1}.
word2num = function(word, voc = c("f", "g")) {
  word_num = rep(NA, nchar(word))
  word_cut = strsplit(word, "")[[1]]
  for(i in 1:length(voc)) {
    word_num[which(word_cut == voc[i])] = i
  }
  return(rev(word_num))
}

##
# Examples
##
word2num("fffgg") # 2 2 1 1 1
word2num("") # integer(0)
word2num("g") # 2
word2num("hhf", voc = c("f", "g", "h")) # 1 3 3

##
# Unit tests
##
checkEquals(word2num(""), numeric(0))
checkEquals(word2num("g"), 2)
checkEquals(word2num("ffg"), c(2, 1, 1))
checkEquals(word2num("hhf", voc = c("f", "g", "h")), c(1, 3, 3))

############################################
# Linear function defined as a composition #
############################################
# Given some linear functions (such as the letter "f" related to the linear
# function f_base; and "g" related to g_base), we get the linear function
# related to a word such as "ffg".
# 
# This function is useful to initialize the functions in the Word Tree
# class (see 3_word_tree.R). 
# Note that when pushing letters (so after initialization), we prefer to use
# a vectorized function (see add_linfunc in 3_word_tree.R).
linfun_init = function(word, 
                       voc = c("f", "g"), 
                       linfunc = list(f_base, g_base)) {
  cut_word = word2num(word, voc)
  if(length(cut_word) == 0) { # For an empty word i.e. word == ""
    out = id_n(length(linfunc[[1]]@seq_coeff_1))
  } else if(length(cut_word) == 1) { # For a word of size 1
    out = linfunc[[cut_word]]
  } else { # for a larger word
    out = linfunc[[cut_word[1]]]
    for(k in 2:length(cut_word)) {
      out = linfunc[[cut_word[k]]] %o% out
    }
  }
  return(out)
}

##
# Examples
##
# With default values
linfun_init("") # x
linfun_init("f") # ax + b
linfun_init("ff") # aax + ab+b
linfun_init("ffg") # a(a(cx+d)+b)+b = a(acx+ad+b)+b = aacx + aad + ab + b
linfun_init("gffgf") # c(a(a(c(ax+b)+d)+b)+b)+d = caacax+caacb+caad+cab+cb+d

# With 3 linear functions
a = c(3, 6)
b = c(4, 2)
c = c(5, 9)
d = c(7, 8)
p = c(-1, -2)
q = c(-3, -4)
f = new("Linear function", coeff_x = "a", coeff_1 = "b",
        seq_coeff_x = as.bigq(a), seq_coeff_1 = as.bigq(b))
g = new("Linear function", coeff_x = "c", coeff_1 = "d",
        seq_coeff_x = as.bigq(c), seq_coeff_1 = as.bigq(d))
h = new("Linear function", coeff_x = "p", coeff_1 = "q",
        seq_coeff_x = as.bigq(p), seq_coeff_1 = as.bigq(q))

linfun_init("ggfg", voc = c("f", "g"), list(f, g))
# ggfg = c(c(a(cx+d)+b)+d)+d = ccacx + ccad + ccb + cd + d
# c*c*a*c # 375 4374
# c*c*a*d + c*c*b + c*d + d # 667 4130

linfun_init("ghfg", voc = c("f", "g", "h"), list(f, g, h))
# ghfg = c(p(a(cx+d)+b)+q)+d = cpacx + cpad+cpb+cq+d
# c*p*a*c # -75 -972
# c*p*a*d+c*p*b+c*q+d # -133 -928
rm(a,b,c,d,p,q,f,g,h)

##
# Unit tests
##
checkEquals(printf(linfun_init("")), "1x + 0")
checkEquals(printf(linfun_init("f")), "ax + b")
checkEquals(printf(linfun_init("ff")), "aax + ab+b")
checkEquals(printf(linfun_init("ffg")), "aacx + aad+ab+b")
checkEquals(printf(linfun_init("gffgf")), "caacax + caacb+caad+cab+cb+d")

a = c(3, 6)
b = c(4, 2)
c = c(5, 9)
d = c(7, 8)
p = c(-1, -2)
q = c(-3, -4)
f = new("Linear function", coeff_x = "a", coeff_1 = "b",
        seq_coeff_x = as.bigq(a), seq_coeff_1 = as.bigq(b))
g = new("Linear function", coeff_x = "c", coeff_1 = "d",
        seq_coeff_x = as.bigq(c), seq_coeff_1 = as.bigq(d))
h = new("Linear function", coeff_x = "p", coeff_1 = "q",
        seq_coeff_x = as.bigq(p), seq_coeff_1 = as.bigq(q))

test1 = linfun_init("ggfg", voc = c("f", "g"), list(f, g))
test2 = linfun_init("ghfg", voc = c("f", "g", "h"), list(f, g, h))

checkEquals(printf(test1), "ccacx + ccad+ccb+cd+d")
checkEquals(printf(test2), "cpacx + cpad+cpb+cq+d")

checkEquals(test1@seq_coeff_x, c(375, 4374))
checkEquals(test1@seq_coeff_1, c(667, 4130))
checkEquals(test2@seq_coeff_x, c(-75, -972))
checkEquals(test2@seq_coeff_1, c(-133, -928))
rm(a,b,c,d,p,q,f,g,h,test1,test2)