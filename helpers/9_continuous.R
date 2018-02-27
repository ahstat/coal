# Summary:
# - get_nb_without (function)
# - get_pos_letter (function)
# - prob_of (function)
# - supp_of (function)
# - get_eps (function)
# - prob_supp_of (function)
# - diff_gmp (function)
# - prob_supp_inv_of (function)
# - plot_distribution (function)
# - iota_vector (function)
# - puiss (function)
# - bigterms1 (function)
# - bigterms2 (function)
# - sigma (function)
# - bigterms_singular_left (function)
# - bigterms_singular_right (function)
# - sigma_singular (function)
# - alpha (function)
# - beta (function)
# - x0 (function)
# - reordering (function)
# - move (function)

############################################################
# Number of letters of the word, without counting "letter" #
############################################################
get_nb_without = function(letter = "f", w) {
  w_split = strsplit(w, "")[[1]]
  out = sum(w_split != letter)
  return(out)
}

##
# Examples
##
get_nb_without("f", "ggffgfgggff") # 6, because there are 6 "g"
get_nb_without("g", "ggffgfgggff") # 5, because there are 5 "f"

##
# Unit tests
##
checkEquals(get_nb_without("f", ""), 0)
checkEquals(get_nb_without("f", "f"), 0)
checkEquals(get_nb_without("g", "f"), 1)
checkEquals(get_nb_without("f", "ggffgfgggff"), 6)
checkEquals(get_nb_without("g", "ggffgfgggff"), 5)

#######################################################
# Positions of a letter compared to others, in a word #
#######################################################
# Positions of a letter in a word compared to others, see examples
# For all outputs, we also add extremes positions
get_pos_letter = function(letter = "f", w) {
  ## Split the word
  w_split = strsplit(w, "")[[1]]
  
  ## Number of non "letter" elements
  n = get_nb_without(letter, w)
  
  ## Let 1 if the position is "letter", 0 otherwise
  w_which_not_letter = ifelse(w_split != letter, 1, 0)
  
  ## Get the position of "letter" compared to other elements
  pos_letter = cumsum(w_which_not_letter)[which(w_split == letter)]
  
  ## Add extremes positions
  pos_letter = c(0, pos_letter, n)
  
  return(pos_letter)
}

##
# Examples
##
get_pos_letter("f", "ggffgfgggff") # 0 2 2 3 6 6 6
# In the following, the positions of "f" are the . and the positions
# of "g" are | :
# ||..|.|||..
# The . are in positions: 2 2 3 6 6
# We add the extremes positions 0 and 6 to get the output.

get_pos_letter("g", "ggffgfgggff") # 0 0 0 2 3 3 3 5
# In the following, the positions of "f" are the | and the positions
# of "f" are . :
# ..||.|...||
# The . are in positions: 0 0 2 3 3 3
# We add the extremes positions 0 and 5 to get the output.

##
# Unit tests
##
checkEquals(get_pos_letter("f", ""), c(0, 0))
checkEquals(get_pos_letter("f", "f"), c(0, 0, 0))
checkEquals(get_pos_letter("f", "g"), c(0, 1))
checkEquals(get_pos_letter("f", "ggffgfgggff"), c(0, 2, 2, 3, 6, 6, 6))
checkEquals(get_pos_letter("g", "ggffgfgggff"), c(0, 0, 0, 2, 3, 3, 3, 5))

##########################
# Raw probability vector #
##########################
# Raw probability vector related to a word, see examples 
prob_of = function(letter = "f", w, gmp = TRUE) {
  pos_letter = get_pos_letter(letter, w)
  n = max(pos_letter)
  if(n == 0) {
    stop("The probability is not defined for word with only 1 distinct letter")
  }
  out = diff(rev(n - pos_letter))
  if(gmp) {
    out = as.bigq(out)
  }
  out = out/n
  return(out)
}

##
# Examples
##
## The word representation of "ggffgfgggff".
# We set "f" = right, and "g" = up. We do compositions from the right.
# First it is a non continuous function in [0, 5] x [0, 6]
# Then, it can be normalized as a cumulative probability distribution in [0,1].
#
# 1          |
#         _ _|
#       _|
#      |
#      |
#  _ _ |
# 0           1
#
# Related distribution for "ggffgfgggff" (with Diracs)
#
#     1/2
#      |    1/3
#      |1/6  |
#  _ _ | | _ |
#  0 1 2 3 4 5  / 5 (support)
# 
# prob_of outputs the raw probability (0, 0, 1/2, 1/6, 0, 1/3)
prob_of("f", "ggffgfgggff") # 0 0 1/2 1/6 0 1/3
# We have the position of "f" as 0 2 2 3 6 6 6
# In the reverse order, it is 6 6 6 3 2 2 0
# Taking n-. with n = nb_g, we have 0 0 0 3 4 4 6
# By taking the difference, we have  0 0 3 1 0 2
# Dividing by n, we obtain the output.
prob_of("f", "ggffgfgggff", gmp = FALSE) # 0.00 0.00 0.50 0.17 0.00 0.33

## The word representation of "ffggfgfffgg".
# We set "f" = right, and "g" = up. We do compositions from the right.
# First it is a non continuous function in [0, 6] x [0, 5]
# Then, it can be normalized as a cumulative probability distribution in [0,1].
#
# 1         _ _
#          |
#         _|
#   _ _ _|
#  |
#  |
# 0            1
#
# Related distribution for "ffggfgfffgg" (with Diracs)
#
# 2/5     2/5
#  |    1/5|
#  | _ _ | | _ _ 
#  0 1 2 3 4 5 6 / 6 (support)
# 
# prob_of outputs the raw probability (2/5, 0, 0, 1/5, 2/5, 0, 0)
prob_of("f", "ffggfgfffgg") # 2/5 0 0 1/5 2/5 0 0

## Note the symmetry between "f" and "g":
all(prob_of("f", "ggffgfgggff") == prob_of("g", "ffggfgfffgg"))
all(prob_of("g", "ggffgfgggff") == prob_of("f", "ffggfgfffgg"))

## Example with word beginning and ending with the same letter
# "fffffgfffffffggfffffffffggggf"
#
# 1                                  _ _ _ _ _
#                      _ _ _ _ _ _ _|
#                     |
#    _ _ _ _ _ _ _ _ _|
#   |
#   |
#   |
#  _|
# 0                                           1
#
# Related distribution for "fffffgfffffffggfffffffffggggf" (with Diracs)
#
#   4/7
#    |
#    |                2/7
#    |                 |            1/7
#  _ | _ _ _ _ _ _ _ _ | _ _ _ _ _ _ | _ _ _ _ _
#  0 1 2 3 4 5 6 7 8 9 1 1 1 1 1 1 1 1 1 1 2 2 2 / 22 (support)
#                      0 1 2 3 4 5 6 7 8 9 0 1 2
#
prob_of("f", "fffffgfffffffggfffffffffggggf")
#
# Positions of "fffffgfffffffggfffffffffggggf"
# Pos f: (0) 0 0 0 0 0 1 1 1 1 1 1 1 3 3 3 3 3 3 3 3 3 7 (7)
# Pos g: (0) 5 12 12 21 21 21 21 (22)

## Inverse "gggggfgggggggffgggggggggffffg"
#
# 1                |
#                  |
#                  |
#                  |
#                _ |
#               |
#               |
#               |
#               |
#               |
#               |
#           _ _ |
#          |
#          |
#          |
#          |
#          |
#          |
#          |
#          |
#   _ _ _ _|
#  |
# 0                1
#
# Related distribution for "gggggfgggggggffgggggggggffffg" (with Diracs)
#
#         9/22
#          |
#          |  7/22
#          |   |
#          |   |5/22
#          |   | |
#          |   | |
#          |   | |
# 1/22     |   | |
#  | _ _ _ | _ | |
#  0 1 2 3 4 5 6 7 / 7 (support)
prob_of("f", "gggggfgggggggffgggggggggffffg")

##
# Unit tests
##
checkEquals(prob_of("f", "ggffgfgggff"), as.bigq(c(0, 0, 3, 1, 0, 2))/6)
checkEquals(prob_of("f", "ggffgfgggff", gmp = FALSE), c(0, 0, 3, 1, 0, 2)/6)
checkEquals(prob_of("f", "ffggfgfffgg"), as.bigq(c(2, 0, 0, 1, 2, 0, 0))/5)
checkEquals(prob_of("f", "ggffgfgggff"), prob_of("g", "ffggfgfffgg"))
checkEquals(prob_of("g", "ggffgfgggff"), prob_of("f", "ffggfgfffgg"))
checkEquals(prob_of("f", "fffffgfffffffggfffffffffggggf"),
            as.bigq(c(0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 1,
                      0, 0, 0, 0, 0)/7))
checkEquals(prob_of("f", "gggggfgggggggffgggggggggffffg"),
            as.bigq(c(1, 0, 0, 0, 9, 0, 7, 5)/22))

######################
# Raw support vector #
######################
# Obtained from the raw probability vector "prob"
supp_of = function(prob) {
  m = length(prob) - 1
  # Check if we have a gmp raw object or a float vector
  if(typeof(prob) == "raw") {
    supp = as.bigq(0:m) / m
  } else {
    supp = (0:m)/m
  }
  return(supp)
}

##
# Examples
##
supp_of(prob_of("f", "ggffgfgggff")) # from 0 to 1 by 1/5
supp_of(prob_of("f", "ggffgfgggff", gmp = FALSE))  # from 0 to 1 by 0.2

##
# Unit tests
##
checkEquals(supp_of(prob_of("f", "ggffgfgggff")), as.bigq(0:5)/5)
checkEquals(supp_of(prob_of("f", "ggffgfgggff", gmp = FALSE)), 0:5/5)
checkEquals(supp_of(prob_of("f", "ffggfgfffgg")), as.bigq(0:6)/6)
checkEquals(supp_of(prob_of("f", "ggffgfgggff")), 
            supp_of(prob_of("g", "ffggfgfffgg")))
checkEquals(supp_of(prob_of("g", "ggffgfgggff")), 
            supp_of(prob_of("f", "ffggfgfffgg")))
checkEquals(supp_of(prob_of("f", "fffffgfffffffggfffffffffggggf")), 
            as.bigq(0:22)/22)
checkEquals(supp_of(prob_of("f", "gggggfgggggggffgggggggggffffg")), 
            as.bigq(0:7)/7)

#################################
# Helper function to define eps #
#################################
get_eps = function(eps, gmp) {
  if(is.na(eps)) {
    if(gmp) {
      # Exact form, so no approximation problem, only 0 elements are removed
      eps = 0
    } else {
      # Approximation, so each probability lower than eps is removed
      eps = 1e-10
    }
  }
  return(eps)
}

#######################################
# Support-Probability minimum mapping #
#######################################
# Minimum mapping in the sense that we remove any 0 probability
# w which must contain at least one "f" and one "g"
prob_supp_of = function(letter = "f", w = "ggffgfgggff", 
                        gmp = TRUE, eps = NA) {
  
  ## Raw probability (with 0 elements)
  prob = prob_of(letter, w, gmp)
  
  ## Define eps such as keep only elements greater than eps
  eps = get_eps(eps, gmp)
  
  ## Index to keep
  idx = which(prob > eps)
  
  ## Output support
  supp = supp_of(prob)[idx]
  
  ## Output probability
  prob = prob[idx]
  
  ## Renormalization in the case of approximation, 
  # to keep a standard probability
  if(!gmp) {
    prob = prob / sum(prob)
  }
  
  ## Output as a list 
  # (because we want to avoid problem between data.table and gmp)
  return(list(supp = supp, prob = prob))
}

##
# Examples
##
prob_supp_of("f", "ggffgfgggff")
prob_supp_of("f", "ggffgfgggff", gmp = FALSE)
# The result is as this graph:
#     1/2
#      |    1/3
#      |1/6  |
#  _ _ | | _ |
#  0 1 2 3 4 5  / 5 (support)

##
# Unit tests
##
checkEquals(prob_supp_of("f", "ggffgfgggff"), 
            list(supp = as.bigq(c(2, 3, 5))/5,
                 prob = as.bigq(c(3, 1, 2))/6))
checkEquals(prob_supp_of("f", "ggffgfgggff", gmp = FALSE),
            list(supp = c(2, 3, 5)/5,
                 prob = c(3, 1, 2)/6))
checkEquals(prob_supp_of("f", "ffggfgfffgg"),
            list(supp = as.bigq(c(0, 3, 4))/6,
                 prob = as.bigq(c(2, 1, 2))/5))
checkEquals(prob_supp_of("f", "ggffgfgggff"), prob_supp_of("g", "ffggfgfffgg"))
checkEquals(prob_supp_of("g", "ggffgfgggff"), prob_supp_of("f", "ffggfgfffgg"))
checkEquals(prob_supp_of("f", "fffffgfffffffggfffffffffggggf"), 
            list(supp = as.bigq(c(1, 10, 17))/22,
                 prob = as.bigq(c(4, 2, 1))/7))
checkEquals(prob_supp_of("f", "gggggfgggggggffgggggggggffffg"), 
            list(supp = as.bigq(c(0, 4, 6, 7))/7,
                 prob = as.bigq(c(1, 9, 7, 5))/22))

################################################
# Helper function to take diff of a gmp vector # 
################################################
diff_gmp = function(vect) {
  return(vect[2:length(vect)] - vect[1:(length(vect)-1)])
}

##
# Examples
##
# For (1, 2), the difference is 1
diff_gmp(as.bigq(c(1L, 2L)))
# For (1, 2, 5, 5+1/2), the differences are (1, 3, 1/2).
diff_gmp(c(as.bigq(c(1L, 2L, 5L)), 5 + as.bigq(1L, 2L)))

#######################################################
# Inverse of the [0,1] -> [0,1] nondecreasing mapping #
#######################################################
# We get the inverse of a prob/supp list
# (note that we do not use/need any word anymore from this step)
prob_supp_inv_of = function(prob_supp, eps = NA) {
  gmp = ifelse(typeof(prob_supp$supp) == "raw", TRUE, FALSE)
  eps = get_eps(eps, gmp)
  
  if(gmp) {
    zero = as.bigq(0)
    one = as.bigq(1)
    diff_fn = diff_gmp
  } else {
    zero = 0
    one = 1
    diff_fn = diff
  }
  
  supp = c(zero, cumsum(prob_supp$prob))
  prob = diff_fn(c(zero, prob_supp$supp, one))
  
  ## Take only positive probability
  idx = prob > eps
  supp = supp[idx]
  prob = prob[idx]
  
  prob_supp_inv = list(supp = supp, prob = prob)
  return(prob_supp_inv)
}

##
# Examples
##
prob_supp = prob_supp_of("f", "ggffgfgggff")
prob_supp_inv_of(prob_supp) # taking the inverse...
prob_supp_of("g", "ggffgfgggff") # ...is like to take the other letter
prob_supp_of("f", "ffggfgfffgg") # ...and (recall) the same as the inverse word
# We do not need to define any words after prob_supp has been defined when
# using prob_supp_inv_of.
rm(prob_supp)

##
# Unit tests
##
prob_supp = prob_supp_of("f", "ggffgfgggff")
checkEquals(prob_supp_inv_of(prob_supp)$supp,
            prob_supp_of("g", "ggffgfgggff")$supp)
checkEquals(prob_supp_inv_of(prob_supp)$prob,
            prob_supp_of("f", "ffggfgfffgg")$prob)
rm(prob_supp)

prob_supp = prob_supp_of("f", "ggffgfgggff", gmp = FALSE)
checkEquals(prob_supp_inv_of(prob_supp)$supp,
            prob_supp_of("g", "ggffgfgggff", gmp = FALSE)$supp)
checkEquals(prob_supp_inv_of(prob_supp)$prob,
            prob_supp_of("f", "ffggfgfffgg", gmp = FALSE)$prob)
rm(prob_supp)

prob_supp = prob_supp_of("f", "ffggfgfffgg")
checkEquals(prob_supp_inv_of(prob_supp)$supp,
            prob_supp_of("g", "ffggfgfffgg")$supp)
checkEquals(prob_supp_inv_of(prob_supp)$prob,
            prob_supp_of("f", "ggffgfgggff")$prob)
rm(prob_supp)

prob_supp = prob_supp_of("f", "fffffgfffffffggfffffffffggggf")
checkEquals(prob_supp_inv_of(prob_supp)$supp,
            prob_supp_of("g", "fffffgfffffffggfffffffffggggf")$supp)
checkEquals(prob_supp_inv_of(prob_supp)$prob,
            prob_supp_of("f", "gggggfgggggggffgggggggggffffg")$prob)
rm(prob_supp)

prob_supp = prob_supp_of("f", "gggggfgggggggffgggggggggffffg")
checkEquals(prob_supp_inv_of(prob_supp)$supp,
            prob_supp_of("g", "gggggfgggggggffgggggggggffffg")$supp)
checkEquals(prob_supp_inv_of(prob_supp)$prob,
            prob_supp_of("f", "fffffgfffffffggfffffffffggggf")$prob)
rm(prob_supp)

## Check inverse of inverse
prob_supp = prob_supp_of("f", "fffffgfffffffggfffffffffggggf")
prob_supp_inv = prob_supp_inv_of(prob_supp)
prob_supp_inv_inv = prob_supp_inv_of(prob_supp_inv)
prob_supp_inv_inv_inv = prob_supp_inv_of(prob_supp_inv_inv)

checkEquals(prob_supp$supp, prob_supp_inv_inv$supp)
checkEquals(prob_supp$prob, prob_supp_inv_inv$prob)
checkEquals(prob_supp_inv$supp, prob_supp_inv_inv_inv$supp)
checkEquals(prob_supp_inv$prob, prob_supp_inv_inv_inv$prob)

rm(prob_supp, prob_supp_inv, prob_supp_inv_inv, prob_supp_inv_inv_inv)

####################################################
# Plotting distribution, CDF and quantile function #
####################################################
plot_distribution = function(prob_supp) {
  Df = DiscreteDistribution(supp = as.numeric(prob_supp$supp), 
                            prob = as.numeric(prob_supp$prob))
  plot(Df)
}

##
# Examples
##
prob_supp = prob_supp_of("f", "fffffgfffffffggfffffffffggggf")
plot_distribution(prob_supp)
dev.off()

prob_supp_inv = prob_supp_inv_of(prob_supp)
plot_distribution(prob_supp_inv)
dev.off()

rm(prob_supp, prob_supp_inv)

## Difference between fgfgfgfg... and gfgfgfgf...
N = 11
prob_supp = seq(from = as.bigq(0), to = as.bigq(1), length.out = N)
prob_supp = list(supp = prob_supp, 
                 prob = rep(1/as.bigq(length(prob_supp)), length(prob_supp)))
prob_supp_inv = prob_supp_inv_of(prob_supp)
#plot_distribution(prob_supp)
#plot_distribution(prob_supp_inv)
rm(N, prob_supp, prob_supp_inv)

#################
# iota function #
#################
# Function to obtain the 3 (normalized) elements in the summation.
# All those functions are necessary to compute alpha and beta for 
# fractional words
iota_vector = function(prob_supp, m = 1, n = 1) {
  # prob_supp_inv = prob_supp_inv_of(prob_supp)
  supp = prob_supp$supp
  prob = prob_supp$prob
  
  # check if formal or numeric
  gmp = ifelse(typeof(prob) == "raw", TRUE, FALSE)
  if(gmp) {
    zero = as.bigq(0)
  } else {
    zero = 0
  }
  
  # Coefficients
  poly_coeff = m * rev(1 - supp)
  
  # iota_{k} - iota_{k-1}
  iota_k__minus__iota_k_minus_1 = n * rev(prob)
  
  # iota_{k-1}
  iota_k_minus_1 = cumsum(c(zero, iota_k__minus__iota_k_minus_1))
  iota_k_minus_1 = iota_k_minus_1[-length(iota_k_minus_1)]
  
  # Return the list:
  return(list(poly_coeff = poly_coeff,
              iota_k_minus_1 = iota_k_minus_1,
              iota_k__minus__iota_k_minus_1 = iota_k__minus__iota_k_minus_1))
}

##
# Examples
##
## Example for "ffg"
w = "ffg"
m = get_nb_without("g", w) # m = 2
n = get_nb_without("f", w) # n = 1
prob_supp = prob_supp_of("f", w)
iota_out = iota_vector(prob_supp)
m * iota_out$poly_coeff
n * iota_out$iota_k_minus_1
n * iota_out$iota_k__minus__iota_k_minus_1
# iota' for the position of "f":
# coeff  iota'_k_minus_1   iota'_k__minus__iota'_k_minus_1
#-------------------------------------------------------
#     0  iota'_-1  0      iota'_0-iota'_-1  0
#     1  iota'_0   0      iota'_1-iota'_0   0
#     2  iota'_1   0      iota'_m-iota'_1   1
#     3  iota'_m   1 (=n)   
#
# We take only the non-null differences, so we keep:
# poly_coeff = 2, iota_k_minus_1 = 0 and iota_k__minus__iota_k_minus_1 = 1

## Example for "ffg" inverse
iota_out_inv = iota_vector(prob_supp_inv_of(prob_supp))
n * iota_out_inv$poly_coeff
m * iota_out_inv$iota_k_minus_1
m * iota_out_inv$iota_k__minus__iota_k_minus_1
# iota for the position of "g" (i.e. of the inverse):
# coeff  iota_k_minus_1   iota_k__minus__iota_k_minus_1
#-------------------------------------------------------
#     0  iota_-1  0       iota_0-iota_-1  2
#     1  iota_0   2       iota_n-iota_0   0    
#     2  iota_n   2 (=m)
#
# We take only the non-null differences, so we keep:
# poly_coeff = 0, iota_k_minus_1 = 0 and iota_k__minus__iota_k_minus_1 = 2
rm(w, m, n, prob_supp, iota_out, iota_out_inv)

## Example for "fffffgfffffffggfffffffffggggf"
w = "fffffgfffffffggfffffffffggggf"
m = get_nb_without("g", w) # m = 22
n = get_nb_without("f", w) # n = 7
prob_supp = prob_supp_of("f", w)
iota_out = iota_vector(prob_supp)
m * iota_out$poly_coeff
n * iota_out$iota_k_minus_1
n * iota_out$iota_k__minus__iota_k_minus_1
# iota' for the position of "f":
# coeff  iota'_k_minus_1   iota'_k__minus__iota'_k_minus_1
#-------------------------------------------------------
#     0  iota'_-1  0      iota'_0-iota'_-1   0
#     1  iota'_0   0      iota'_1-iota'_0    0
#     2  iota'_1   0      iota'_2-iota'_1    0
#     3  iota'_2   0      iota'_3-iota'_2    0
#     4  iota'_3   0      iota'_4-iota'_3    0
#     5  iota'_4   0      iota'_5-iota'_4    1
#     6  iota'_5   1      iota'_6-iota'_5    0
#     7  iota'_6   1      iota'_7-iota'_6    0
#     8  iota'_7   1      iota'_8-iota'_7    0
#     9  iota'_8   1      iota'_9-iota'_8    0
#    10  iota'_9   1      iota'_10-iota'_9   0
#    11  iota'_10  1      iota'_11-iota'_10  0
#    12  iota'_11  1      iota'_12-iota'_11  2
#    13  iota'_12  3      iota'_13-iota'_12  0
#    14  iota'_13  3      iota'_14-iota'_13  0
#    15  iota'_14  3      iota'_15-iota'_14  0
#    16  iota'_15  3      iota'_16-iota'_15  0
#    17  iota'_16  3      iota'_17-iota'_16  0
#    18  iota'_17  3      iota'_18-iota'_17  0
#    19  iota'_18  3      iota'_19-iota'_18  0
#    20  iota'_19  3      iota'_20-iota'_19  0
#    21  iota'_20  3      iota'_21-iota'_20  4
#    22  iota'_21  7      iota'_m-iota'_21   0
#    23  iota'_m   7 (=n)
# We take only the non-null differences, so we keep:
# poly_coeff = c(5, 12, 21), 
# iota_k_minus_1 = c(0, 1, 3)
# iota_k__minus__iota_k_minus_1 = c(1, 2, 4)

## Example for "fffffgfffffffggfffffffffggggf" inverse
iota_out_inv = iota_vector(prob_supp_inv_of(prob_supp))
n * iota_out_inv$poly_coeff
m * iota_out_inv$iota_k_minus_1
m * iota_out_inv$iota_k__minus__iota_k_minus_1
# iota for the position of "g" (i.e. of the inverse):
# coeff  iota_k_minus_1   iota_k__minus__iota_k_minus_1
#-------------------------------------------------------
#     0  iota_-1  0       iota_0-iota_-1   5
#     1  iota_0   5       iota_1-iota_0    7
#     2  iota_1  12       iota_2-iota_1    0
#     3  iota_2  12       iota_3-iota_2    9
#     4  iota_3  21       iota_4-iota_3    0
#     5  iota_4  21       iota_5-iota_4    0
#     6  iota_5  21       iota_6-iota_5    0
#     7  iota_6  21       iota_n-iota_6    1
#     8  iota_n  22

# We take only the non-null differences, so we keep:
# poly_coeff = c(0, 1, 3, 7), 
# iota_k_minus_1 = c(0, 5, 12, 21)
# iota_k__minus__iota_k_minus_1 = c(5, 7, 9, 1)
rm(w, m, n, prob_supp, iota_out, iota_out_inv)

##
# Unit tests
##
## Example for "ffg"
w = "ffg"
m = get_nb_without("g", w)
n = get_nb_without("f", w)
prob_supp = prob_supp_of("f", w)
iota_out = iota_vector(prob_supp)
checkEquals(m * iota_out$poly_coeff, as.bigq(2))
checkEquals(n * iota_out$iota_k_minus_1, as.bigq(0))
checkEquals(n * iota_out$iota_k__minus__iota_k_minus_1, as.bigq(1))
## Example for "ffg" inverse
iota_out_inv = iota_vector(prob_supp_inv_of(prob_supp))
checkEquals(n * iota_out_inv$poly_coeff, as.bigq(0))
checkEquals(m * iota_out_inv$iota_k_minus_1, as.bigq(0))
checkEquals(m * iota_out_inv$iota_k__minus__iota_k_minus_1, as.bigq(2))
#
rm(w, m, n, prob_supp, iota_out, iota_out_inv)

## Example for "fffffgfffffffggfffffffffggggf"
w = "fffffgfffffffggfffffffffggggf"
m = get_nb_without("g", w)
n = get_nb_without("f", w)
prob_supp = prob_supp_of("f", w)
iota_out = iota_vector(prob_supp)
checkEquals(m * iota_out$poly_coeff, as.bigq(c(5, 12, 21)))
checkEquals(n * iota_out$iota_k_minus_1, as.bigq(c(0, 1, 3)))
checkEquals(n * iota_out$iota_k__minus__iota_k_minus_1, as.bigq(c(1, 2, 4)))
## Example for "fffffgfffffffggfffffffffggggf" inverse
iota_out_inv = iota_vector(prob_supp_inv_of(prob_supp))
checkEquals(n * iota_out_inv$poly_coeff, as.bigq(c(0, 1, 3, 7)))
checkEquals(m * iota_out_inv$iota_k_minus_1, as.bigq(c(0, 5, 12, 21)))
checkEquals(m * iota_out_inv$iota_k__minus__iota_k_minus_1, 
            as.bigq(c(5, 7, 9, 1)))
#
rm(w, m, n, prob_supp, iota_out, iota_out_inv)

##########################
# Power function for gmp #
##########################
# value is a bigq element
# vector is a vector of bigq elements
# The output is value^vector
puiss = function(value, vector) {
  puiss_out = sapply(vector, function(x) {value^x}, simplify = FALSE)
  puiss_out = do.call(base::c, puiss_out)
  return(puiss_out)
}

##
# Examples
##
puiss(as.bigq(3L), as.bigq(c(1,2,3))) # 3^(1, 2, 3) = (3, 9, 27)
puiss(as.bigq(1L, 3L), as.bigq(c(1,2,3))) # (1/3)^(1, 2, 3) = (1/3, 1/9, 1/27)
puiss(3, c(1,2,3))
puiss(1/3, c(1,2,3))

#########################
# First terms of Sigma| #
#########################
# Note: prob_supp by 'default' is for "f" position, which corresponds
# to iota'.
# In the function, all is defined for iota'
bigterms1 = function(prob_supp, m, n, a = as.bigq(1L, 3L), c = as.bigq(2L)) {
  iota_out = iota_vector(prob_supp)
  
  # "c" is used for the variable name... 
  # So we use base::c for the concatenation function c()...
  
  ## coefficients
  a_k = puiss(a, m * iota_out$poly_coeff)
  
  ## iota factorized term
  c_k = puiss(c, n * iota_out$iota_k_minus_1)
    
  ## iota difference term
  c_diff = puiss(c, n * iota_out$iota_k__minus__iota_k_minus_1)

  ## Each term of the sum sigma|_{1}
  sigma_term_out = a_k * c_k * (c_diff - 1)
  return(sigma_term_out)
}

##
# Example
##
## For "ffg"
# By calculus, the term is: a^2 * (c-1)
w = "ffg"
m = get_nb_without("g", w)
n = get_nb_without("f", w)
prob_supp = prob_supp_of("f", w)
a = as.bigq(1L, 2713L)
c = as.bigq(7L)
bigterms1(prob_supp, m, n, a, c) # same as a^2 * (c - 1)
a^2 * (c - 1)

##
# Unit test
##
checkEquals(bigterms1(prob_supp, m, n, a, c), a^2 * (c - 1))
rm(w, m, n, prob_supp, a, c)

##########################
# Second terms of Sigma| #
##########################
# Note: prob_supp_inv by 'default' is for "g" position, which corresponds
# to iota.
bigterms2 = function(prob_supp, m, n, a = as.bigq(1L, 3L), c = as.bigq(2L)) {
  prob_supp_inv = prob_supp_inv_of(prob_supp)
  sigma_term_out = bigterms1(prob_supp_inv, n, m, c, a)
  return(sigma_term_out)
}

##
# Example
##
## For "ffg"
# By calculus, the term is: c^0 * (a^2 - 1)
w = "ffg"
m = get_nb_without("g", w)
n = get_nb_without("f", w)
prob_supp = prob_supp_of("f", w)
a = as.bigq(1L, 2713L)
c = as.bigq(7L)
bigterms2(prob_supp, m, n, a, c) # same as c^0 * (a^2 - 1)
c^0 * (a^2 - 1)

##
# Unit test
##
checkEquals(bigterms2(prob_supp, m, n, a, c), c^0 * (a^2 - 1))
rm(w, m, n, prob_supp, a, c)

##########
# Sigma| #
##########
# Sigma| = bigterm_iota' - bigterm_iota = sum(bigterms1) - sum(bigterms2)
sigma = function(prob_supp, m, n, a = as.bigq(1L, 3L), c = as.bigq(2L)) {
  out1 = bigterms1(prob_supp, m, n, a, c)
  out2 = bigterms2(prob_supp, m, n, a, c)
  sigma_out = sum(out1) - sum(out2)
  return(sigma_out)
}

##
# Example
##
## For "ffg"
# By calculus, Sigma| = 1 - 2 * a^2 + a^2 * c
w = "ffg"
m = get_nb_without("g", w)
n = get_nb_without("f", w)
prob_supp = prob_supp_of("f", w)
a = as.bigq(1L, 2713L)
c = as.bigq(7L)
sigma(prob_supp, m, n, a, c) # same as 1 - 2 * a^2 + a^2 * c
1 - 2 * a^2 + a^2 * c

##
# Unit test
##
checkEquals(sigma(prob_supp, m, n, a, c), 1 - 2 * a^2 + a^2 * c)
rm(w, m, n, prob_supp, a, c)

##################################################
# Case a == 1 or c == 1 : bigterms_singular_left #
##################################################
bigterms_singular_left = function(prob_supp, m, n, c) {
  iota_out = iota_vector(prob_supp)
  
  # alternative coefficient
  a_k = m * iota_out$poly_coeff 
  
  ## iota factorized term
  c_k = puiss(c, n * iota_out$iota_k_minus_1)
  
  ## iota difference term
  c_diff = puiss(c, n * iota_out$iota_k__minus__iota_k_minus_1)
  
  ## Each term of the sum sigma|_{1}
  sigma_term_out = a_k * c_k * (c_diff - 1)
  
  return(sigma_term_out)
}

###################################################
# Case a == 1 or c == 1 : bigterms_singular_right #
###################################################
bigterms_singular_right = function(prob_supp, m, n, c) {
  iota_out = iota_vector(prob_supp)
  
  ## coefficients
  c_k = puiss(c, m * iota_out$poly_coeff)
  
  # i_diff
  i_diff = n * iota_out$iota_k__minus__iota_k_minus_1
  
  ## Each term of the sum sigma|_{2}
  sigma_term_out = c_k * i_diff
  
  return(sigma_term_out)
}

###################
# Sigma| singular #
###################
sigma_singular = function(prob_supp, m, n, c) {
  prob_supp_inv = prob_supp_inv_of(prob_supp)
  
  out1 = bigterms_singular_left(prob_supp, m, n, c)
  out2 = bigterms_singular_right(prob_supp_inv, n, m, c)
  
  sigma_out = sum(out1) - sum(out2)
  return(sigma_out)
}

#####################
# Coefficient alpha #
#####################
alpha = function(m, n, a, c) {
  return(puiss(a, m) * puiss(c, n))
}

####################
# Coefficient beta #
####################
beta = function(prob_supp, m, n, a, b, c, d) {
  if(as.numeric(abs(a-1)) == 0 & as.numeric(abs(c-1)) == 0) {
    beta_out = b*m + d*n
  } else if(as.numeric(abs(c-1)) == 0) {
    prob_supp_inv = prob_supp_inv_of(prob_supp)
    beta_out = beta(prob_supp_inv, n, m, c, d, a, b)
  } else if(as.numeric(abs(a-1)) == 0) {
    beta_left = 2 * d * (1 - alpha(m, n, a, c)) + 
      b * (1-c) * m * alpha(m, n, a, c)
    beta_right = - b * (1-c) * sigma_singular(prob_supp, m, n, c)
    beta_quotient = 2 * (1-c)
    beta_out = (beta_left + beta_right) / beta_quotient
  } else {
    beta_left = (b - b * c + d - a * d) * (1 - alpha(m, n, a, c))
    beta_right = (b - b * c - d + a * d) * sigma(prob_supp, m, n, a, c)
    beta_quotient = 2 * (1 - a) * (1 - c)
    beta_out = (beta_left + beta_right) / beta_quotient
  }
  return(beta_out)
}

##
# Examples
##
## For "ffg"
# By simple calculus, alpha = a^2 * c and beta = a^2 * d + a*b + b
w = "ffg"
m = get_nb_without("g", w)
n = get_nb_without("f", w)
prob_supp = prob_supp_of("f", w)
a = as.bigq(1L, 2713L)
c = as.bigq(7L)
b = as.bigq(-1L, 41L)
d = as.bigq(3L)
alpha(m, n, a, c) # same as a^2 * c
a^2 * c
beta(prob_supp, m, n, a, b, c, d) # same as a^2 * d + a*b + b
a^2 * d + a*b + b

## Example with a = 1
w = "ffg"
m = get_nb_without("g", w)
n = get_nb_without("f", w)
prob_supp = prob_supp_of("f", w)
a = as.bigq(1L)
c = as.bigq(7L)
b = as.bigq(-1L, 41L)
d = as.bigq(3L)
beta(prob_supp, m, n, a, b, c, d)
a^2 * d + a*b + b

## Visual verification
out = sapply(as.bigq(1L, 2^(1:15)), 
       function(k) {as.numeric(beta(prob_supp, m, n, a + k, b, c, d))})
out_true = as.numeric(beta(prob_supp, m, n, a, b, c, d))
out_true2 = a^2 * d + a*b + b
plot(out)
abline(h = as.numeric(out_true2), col = "blue")
abline(h = out_true, col = "red")
plot(log(out - out_true))
dev.off()

## Example with c = 1
w = "ffg"
m = get_nb_without("g", w)
n = get_nb_without("f", w)
prob_supp = prob_supp_of("f", w)
a = as.bigq(7L)
c = as.bigq(1L)
b = as.bigq(-1L, 41L)
d = as.bigq(3L)
beta(prob_supp, m, n, a, b, c, d)
a^2 * d + a*b + b

## Example with a = c = 1
w = "ffg"
m = get_nb_without("g", w)
n = get_nb_without("f", w)
prob_supp = prob_supp_of("f", w)
a = as.bigq(1L)
c = as.bigq(1L)
b = as.bigq(-1L, 41L)
d = as.bigq(3L)
beta(prob_supp, m, n, a, b, c, d)
a^2 * d + a*b + b

## Big big word
w = "ffgfgfgfgffffffgffgfggfggffgfffggfgfgggfgfgfgfgfgfgggfgffggfgffgggffgfgff"
m = get_nb_without("g", w)
n = get_nb_without("f", w)
prob_supp = prob_supp_of("f", w)

## Go to a = 1
a = as.bigq(1L)
c = as.bigq(1L, 7L)
b = as.bigq(-1L, 41L)
d = as.bigq(3L)

beta(prob_supp, m, n, a, b, c, d)
out = sapply(as.bigq(1L, 2^(5:20)), 
             function(k) {as.numeric(beta(prob_supp, m, n, a + k, b, c, d))})
out_true = as.numeric(beta(prob_supp, m, n, a, b, c, d))
plot(out)
abline(h = out_true, col = "red")
plot(log(out - out_true))
dev.off()

## Go to c = 1
a = as.bigq(1L, 7L)
c = as.bigq(1L)
b = as.bigq(-1L, 41L)
d = as.bigq(3L)

beta(prob_supp, m, n, a, b, c, d)
out = sapply(as.bigq(1L, 2^(5:20)), 
             function(k) {as.numeric(beta(prob_supp, m, n, a, b, c + k, d))})
out_true = as.numeric(beta(prob_supp, m, n, a, b, c, d))
plot(out)
abline(h = out_true, col = "red")
plot(log(out - out_true))
dev.off()

## Go to a = 1, c = 1
a = as.bigq(1L)
c = as.bigq(1L)
b = as.bigq(-1L, 41L)
d = as.bigq(3L)

beta(prob_supp, m, n, a, b, c, d)
out = sapply(as.bigq(1L, 2^(5:20)), 
             function(k) {as.numeric(beta(prob_supp, m, n, a, b, c + k, d))})
out_true = as.numeric(beta(prob_supp, m, n, a, b, c, d))
plot(out)
abline(h = out_true, col = "red")
plot(log(out - out_true))
dev.off()

## Go to a = 1, c = 1 bis
a = as.bigq(1L)
c = as.bigq(1L)
b = as.bigq(-1L, 41L)
d = as.bigq(3L)

beta(prob_supp, m, n, a, b, c, d)
out = sapply(as.bigq(1L, 2^(5:20)), 
             function(k) {as.numeric(beta(prob_supp, m, n, a+k, b, c+k, d))})
out_true = as.numeric(beta(prob_supp, m, n, a, b, c, d))
plot(out)
abline(h = out_true, col = "red")
plot(log(out - out_true))
dev.off()
rm(out, out_true, out_true2)

##
# Unit test
##
## For "ffg"
# By simple calculus, alpha = a^2 * c and beta = a^2 * d + a*b + b
w = "ffg"
m = get_nb_without("g", w)
n = get_nb_without("f", w)
prob_supp = prob_supp_of("f", w)
a = as.bigq(1L, 2713L)
c = as.bigq(7L)
b = as.bigq(-1L, 41L)
d = as.bigq(3L)
checkEquals(alpha(m, n, a, c), a^2 * c)
checkEquals(beta(prob_supp, m, n, a, b, c, d), a^2 * d + a*b + b)
rm(w, m, n, prob_supp, a, b, c, d)

## Many tests:
a_vect = list(as.bigq(1L, 2713L), as.bigq(1L))
c_vect = list(as.bigq(11L), as.bigq(1L))
b = as.bigq(-1L, 41L)
d = as.bigq(13L)
for(idx_a in 1:length(a_vect)) {
  for(idx_c in 1:length(c_vect)) {
    # print(idx_a)
    # print(idx_c)
    a = a_vect[[idx_a]]
    c = c_vect[[idx_c]]

    # gf
    w = "gf"
    m = get_nb_without("g", w)
    n = get_nb_without("f", w)
    prob_supp = prob_supp_of("f", w)
    checkEquals(alpha(m, n, a, c), a*c)
    checkEquals(beta(prob_supp, m, n, a, b, c, d), b*c+d)
    
    # fg
    w = "fg"
    m = get_nb_without("g", w)
    n = get_nb_without("f", w)
    prob_supp = prob_supp_of("f", w)
    checkEquals(alpha(m, n, a, c), a*c)
    checkEquals(beta(prob_supp, m, n, a, b, c, d), a*d+b)
    
    # gff
    w = "gff"
    m = get_nb_without("g", w)
    n = get_nb_without("f", w)
    prob_supp = prob_supp_of("f", w)
    checkEquals(beta(prob_supp, m, n, a, b, c, d), a*b*c+b*c+d)
    
    # fgf
    w = "fgf"
    m = get_nb_without("g", w)
    n = get_nb_without("f", w)
    prob_supp = prob_supp_of("f", w)
    checkEquals(beta(prob_supp, m, n, a, b, c, d), a*b*c+a*d+b)
    
    # fgff
    w = "fgff"
    m = get_nb_without("g", w)
    n = get_nb_without("f", w)
    prob_supp = prob_supp_of("f", w)
    checkEquals(beta(prob_supp, m, n, a, b, c, d), a^2*b*c+a*b*c+a*d+b)
    
    # gfgf
    w = "gfgf"
    m = get_nb_without("g", w)
    n = get_nb_without("f", w)
    prob_supp = prob_supp_of("f", w)
    checkEquals(beta(prob_supp, m, n, a, b, c, d), a*b*c^2+a*c*d+b*c+d)
  
    # gfgg
    w = "gfgg"
    m = get_nb_without("g", w)
    n = get_nb_without("f", w)
    prob_supp = prob_supp_of("f", w)
    checkEquals(beta(prob_supp, m, n, a, b, c, d), a*c^2*d+a*c*d+b*c+d)
  }
}
rm(w, m, n, prob_supp, a, c, b, d, a_vect, c_vect, idx_a, idx_c)

###############
# Fixed point #
###############
x0 = function(prob_supp, m, n, a, b, c, d) {
  alpha_val = alpha(m, n, a, c)
  if(as.numeric(abs(alpha_val - 1)) == 0) {
    return(NA)
  } else {
    return(beta(prob_supp, m, n, a, b, c, d) / (1 - alpha_val))
  }
}

##
# Examples
##
w = "ffg"
m = get_nb_without("g", w)
n = get_nb_without("f", w)
prob_supp = prob_supp_of("f", w)
a = as.bigq(1L, 3L)
b = as.bigq(-1L, 3L)
c = as.bigq(2L)
d = as.bigq(0)
x0(prob_supp, m, n, a, b, c, d)
# ffg(-4/7) = ff(-8/7) = f(-5/7) = -4/7

## Example without gmp
prob_supp2 = prob_supp_of("f", w, gmp = FALSE)
a2 = 1/3
b2 = -1/3
c2 = 2
d2 = 0
x0(prob_supp2, m, n, a2, b2, c2, d2)

##
# Unit tests
##
checkEquals(x0(prob_supp, m, n, a, b, c, d), as.bigq(-4, 7))
checkEquals(x0(prob_supp2, m, n, a2, b2, c2, d2), -4/7)
rm(w, m, n, prob_supp, a, b, c, d, prob_supp2, a2, b2, c2, d2)

####################################################################
# Helper function to reorder the list prob_supp, for move function #
####################################################################
# From a prob_supp, removing duplicate 0 support, remove 0 probability,
# and reorder the support.
reordering = function(prob_supp) {
  ## Get unique supp and reorder
  # The order may have problem with very close gmp numbers
  ord = order(as.numeric(prob_supp$supp))
  prob_supp$supp = prob_supp$supp[ord]
  prob_supp$prob = prob_supp$prob[ord]
  
  ## Check if there are two 0's support to merge them
  if(length(prob_supp$supp) > 1) {
    if(prob_supp$supp[1] == prob_supp$supp[2]) {
      prob_supp$prob[2] = prob_supp$prob[1] + prob_supp$prob[2]
      prob_supp$prob = prob_supp$prob[-1]
      prob_supp$supp = prob_supp$supp[-1]
    }
  }
  
  idx_remove = which(prob_supp$prob == 0)
  if(length(idx_remove) > 0) {
    prob_supp$prob = prob_supp$prob[-idx_remove]
    prob_supp$supp = prob_supp$supp[-idx_remove]
  }
  return(prob_supp)
}

##
# Examples
##
prob_supp1 = list()
prob_supp1$supp = c(0, 1, 3, 5, 7, 0)/7
prob_supp1$prob = c(1, 0, 6, 3, 3, 4)/17
reordering(prob_supp1) 
# supp: 0.0000000 0.4285714 0.7142857 1.0000000
# prob: 0.2941176 0.3529412 0.1764706 0.1764706

prob_supp2 = list()
prob_supp2$supp = as.bigq(c(0, 1, 3, 5, 7, 0))/7
prob_supp2$prob = as.bigq(c(1, 0, 6, 3, 3, 4))/17
reordering(prob_supp2)
# supp: 0    3/7  5/7  1  
# prob: 5/17 6/17 3/17 3/17

prob_supp3 = list()
prob_supp3$supp = as.bigq(c(0, 1))
prob_supp3$prob = as.bigq(c(1, 0))
reordering(prob_supp3)
# supp: 0
# prob: 1

prob_supp4 = list()
prob_supp4$supp = as.bigq(c(1))
prob_supp4$prob = as.bigq(c(1))
reordering(prob_supp4)
# supp: 1
# prob: 1

##
# Unit tests
##
# We have initially:
# 0    0.14 0.42 0.71 1    0
# 0.05 0    0.35 0.17 0.17 0.23
# So we get:
# 0    0.42 0.71 1   
# 0.28 0.29 0.35 0.17
checkEquals(reordering(prob_supp1)$supp, c(0, 3, 5, 7)/7)
checkEquals(reordering(prob_supp1)$prob, c(1+4, 6, 3, 3)/17)
checkEquals(reordering(prob_supp2)$supp, as.bigq(c(0, 3, 5, 7))/7)
checkEquals(reordering(prob_supp2)$prob, as.bigq(c(1+4, 6, 3, 3))/17)
checkEquals(reordering(prob_supp3)$supp, as.bigq(0))
checkEquals(reordering(prob_supp3)$prob, as.bigq(1))
checkEquals(reordering(prob_supp4)$supp, as.bigq(1))
checkEquals(reordering(prob_supp4)$prob, as.bigq(1))
rm(prob_supp1, prob_supp2, prob_supp3, prob_supp4)

###################################
# Moving the support to the right #
###################################
# Move the probability support to the right by adding t
# When reaching 1, transferring the remaining t to 0
# Then, move(prob_supp, 2) will be prob_supp
move = function(prob_supp, t, eps = NA) {
  ## check if formal or numeric to define eps
  if(is.na(eps)) {
    gmp = ifelse(typeof(prob_supp$prob) == "raw", TRUE, FALSE)
    if(gmp) {
      eps = 0
    } else {
      eps = 1e-10
    }
  }
  if(gmp) {
    one = as.bigq(1)
  } else {
    one = 1
  }
  
  len = length(prob_supp$prob)
  if(t <= eps) {
    return(prob_supp)
  } else {
    max_support = prob_supp$supp[len] # max because prob_supp$supp is sorted
    
    if(max_support != one) {
      ## Go to the right until moving distance t or reaching 1
      t_right_ok = min(t, one - max_support)
      prob_supp$supp = prob_supp$supp + t_right_ok
      
      ## Remaining t (can be 0 or > 0)
      t = t - t_right_ok
      return(move(prob_supp, t))
    } else {
      ## Transfering t from 1 to 0 
      # until transferring mass t or reaching proba_in_one
      # (here max_support == 1)
      proba_in_one = prob_supp$prob[len]
      
      ## We transfer probability, then we do
      proba_remaining = max(proba_in_one - t, 0)
      proba_transfer = proba_in_one - proba_remaining
      t = t - proba_transfer
      prob_supp$prob[len] = proba_remaining
      prob_supp$prob[len + 1] = proba_transfer
      prob_supp$supp[len + 1] = 0
      
      ## Reorder
      prob_supp = reordering(prob_supp)
      return(move(prob_supp, t))
    }
  }
}

##
# Examples
##
## Without gmp
w = "fg"
prob_supp = prob_supp_of("f", w, gmp = FALSE)
move(prob_supp, 0) # all mass in 0 (probability 1)
move(prob_supp, 0.4) # all mass in 0.4
move(prob_supp, 0.8) # all mass in 0.8
move(prob_supp, 0.999) # all mass in 0.999
move(prob_supp, 1) # all mass in 1, this is "gf"
move(prob_supp, 1.1) # 0.1 in 0; 0.9 in 1
move(prob_supp, 1.4) # 0.4 in 0; 0.6 in 1
move(prob_supp, 1.8) # 0.8 in 0; 0.2 in 1
move(prob_supp, 1.999) # 0.999 in 0; 0.001 in 1
move(prob_supp, 2) # like t=0, all mass in 0
move(prob_supp, 2.5) # like t=0.5, all mass in 0.5
move(prob_supp, 3.5) # like t=1.5, 0.5 in 0; 0.5 in 1

## With gmp
w = "fg"
prob_supp = prob_supp_of("f", w, gmp = TRUE)
move(prob_supp, 0) # all mass in 0 (probability 1)
move(prob_supp, as.bigq(4, 10)) # all mass in 2/5
move(prob_supp, as.bigq(8, 10)) # all mass in 4/5
move(prob_supp, as.bigq(1)) # all mass in 1, this is "gf"
move(prob_supp, as.bigq(3, 2)) # like t=1.5, 1/2 in 0; 1/2 in 1

## More complex word
w = "fffffgfffffffggfffffffffggggf"
prob_supp = prob_supp_of("f", w, gmp = FALSE)
move(prob_supp, 0)
move(prob_supp, 0.1)
move(prob_supp, 0.2)
move(prob_supp, 0.3)
move(prob_supp, 0.4)
move(prob_supp, 0.5)
move(prob_supp, 1)
move(prob_supp, 2) # same as move(prob_supp, 0)

w = "ffggfgffgffgfgf"
prob_supp = prob_supp_of("f", w, gmp = FALSE)
move(prob_supp, 0.1)

##
# Unit tests
##
checkEquals(prob_supp_of("f", "fg", gmp = FALSE),
            move(prob_supp_of("f", "gf", gmp = FALSE), 1))
checkEquals(prob_supp_of("f", "fg", gmp = TRUE),
            move(prob_supp_of("f", "gf", gmp = TRUE), as.bigq(1)))
checkEquals(prob_supp_of("f", "fggfgffgffgfgf", gmp = FALSE),
            move(prob_supp_of("f", "gffgfgffggfgff", gmp = FALSE), 1))