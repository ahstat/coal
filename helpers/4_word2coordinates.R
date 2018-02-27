# Summary:
# - voc_unique_func (function)
# - voc_lower_func (function)
# - voc_upper_func (function)
# - voc2steps (function)
# - walk_word (function)
# - coord_representation (function)
# - coord_representation_n (function)
# - compute_coord (method)

# We assume that the vocabulary is as follows:
# - Each element of `voc` is a one letter character,
# - Lower case is to go forward, and related upper case is the inverse.
# For example, with voc = c("f", "g", "F"), we have 2 functions f, g, and 
# F = f^{-1}.
#
# There is a nice bijection between a b-ary tree and all forward unit walk
# in \mathbb{N}^b.
# For example "ffg" can be to go up once and then right twice, amd "gf" is to
# go right then up, whereas "fg" is to go up then right.
# Note that *IF* the vocabulary commutes, then each word can be simplified to 
# any representation with the same signature. In this case, "fgffg" can be 
# written as "ggfff" (*only if* the vocabulary commutes).

####################################################
# Non inverse function letters that possibly exist #
####################################################
# For example, if the vocabulary contains f and G only, we know that G is the
# inverse of some function g, so the vocabulary of non inverse functions is
# c("f", "g").
voc_unique_func = function(voc) {
  unique(tolower(voc))
}

##
# Examples
##
voc_unique_func(c("f", "g")) # c("f", "g")
voc_unique_func(c("f", "G")) # c("f", "g")
voc_unique_func(c("F")) # "f"
voc_unique_func(c("f", "F", "G")) # c("f", "g")

##
# Unit tests
##
checkEquals(voc_unique_func(c("f", "g")), c("f", "g"))
checkEquals(voc_unique_func(c("f", "G")), c("f", "g"))
checkEquals(voc_unique_func(c("F")), "f")
checkEquals(voc_unique_func(c("f", "F", "G")), c("f", "g"))

############################
# Forward function letters #
############################
# Get the list of non inverse functions.
voc_lower_func = function(voc) {
  voc[which(unlist(gregexpr("[a-z]", voc)) == 1)]
}

##
# Examples
##
voc_lower_func(c("f", "g")) # c("f", "g")
voc_lower_func(c("f", "G")) # "f"
voc_lower_func(c("F")) # character(0)
voc_lower_func(c("f", "F", "G")) # "f"

##
# Unit tests
##
checkEquals(voc_lower_func(c("f", "g")), c("f", "g"))
checkEquals(voc_lower_func(c("f", "G")), "f")
checkEquals(voc_lower_func(c("F")), character(0))
checkEquals(voc_lower_func(c("f", "F", "G")), "f")

############################
# Inverse function letters #
############################
# Get the list of inverse functions.
voc_upper_func = function(voc) {
  voc[which(unlist(gregexpr("[A-Z]", voc)) == 1)]
}

##
# Examples
##
voc_upper_func(c("f", "g")) # character(0)
voc_upper_func(c("f", "G")) # "G"
voc_upper_func(c("F")) # "F"
voc_upper_func(c("f", "F", "G")) # c("F", "G")

##
# Unit tests
##
checkEquals(voc_upper_func(c("f", "g")), character(0))
checkEquals(voc_upper_func(c("f", "G")), "G")
checkEquals(voc_upper_func(c("F")), "F")
checkEquals(voc_upper_func(c("f", "F", "G")), c("F", "G"))

######################
# voc2steps function #
######################
# Get the contrast function for a vocabulary
# For a vocabulary with n forward functions,
# - the letter in position k of the (forward) function is defined as the
# coordinate k i.e. (0, ..., 0, 1, 0, ... 0) where 1 is in position k.
# - the inverse function related to the forward function in position k is
# (0, ..., 0, -1, 0, ... 0) where -1 is in position k.
# Examples make this clear.
voc2steps = function(voc) {
  voc_unique = voc_unique_func(voc)
  
  # Get the contrast in the case where all upper and lower letters are present
  contrast_lower = contrasts(as.factor(voc_unique), contrasts = FALSE)
  colnames(contrast_lower) = paste0("coord_", 1:ncol(contrast_lower))
  contrast_upper = -contrast_lower
  rownames(contrast_upper) = toupper(rownames(contrast_lower))
  contrast = rbind(contrast_lower, contrast_upper)
  
  # Keep only present letters
  M_voc = matrix(FALSE, ncol = 2, nrow = length(voc_unique))
  colnames(M_voc) = c("lower", "upper")
  rownames(M_voc) = voc_unique
  M_voc[rownames(M_voc) %in% voc_lower_func(voc), "lower"] = TRUE
  M_voc[rownames(M_voc) %in% tolower(voc_upper_func(voc)), "upper"] = TRUE
  
  to_keep = which(rbind(M_voc[,"lower",drop=FALSE], 
                        M_voc[,"upper",drop=FALSE]))
  contrast = contrast[to_keep,]
  
  return(contrast)
}

##
# Examples
##
voc2steps(c("f", "g"))
# f is represented as (1,0) of R^2, and g as (0,1)
#   coord_1 coord_2
# f       1       0
# g       0       1

voc2steps(c("f", "g", "F"))
# F=f^{-1} is represented as (-1,0)
#   coord_1 coord_2
# f       1       0
# g       0       1
# F      -1       0

voc2steps(c("f", "g", "G"))
#   coord_1 coord_2
# f       1       0
# g       0       1
# G       0      -1

voc2steps(c("f", "g", "F", "G"))
#   coord_1 coord_2
# f       1       0
# g       0       1
# F      -1       0
# G       0      -1

voc2steps(c("f", "g", "F", "G", "H"))
# There are 3 potential forward letters, so all live in R^3
#   coord_1 coord_2 coord_3
# f       1       0       0
# g       0       1       0
# F      -1       0       0
# G       0      -1       0
# H       0       0      -1

voc2steps(c("f", "G", "H"))
#   coord_1 coord_2 coord_3
# f       1       0       0
# G       0      -1       0
# H       0       0      -1

##
# Unit tests
##
checkEqualsNumeric(voc2steps(c("f", "g"))[1,], c(1,0))
checkEqualsNumeric(voc2steps(c("f", "g", "F"))[3,], c(-1,0))
checkEqualsNumeric(voc2steps(c("f", "g", "G"))[3,], c(0,-1))
checkEqualsNumeric(voc2steps(c("f", "g", "F", "G", "H"))[5,], c(0,0,-1))
checkEqualsNumeric(voc2steps(c("f", "G", "H"))[1,], c(1,0,0))

######################
# walk_word function #
######################
# Define the path related to a word
walk_word = function(word, voc = c("f", "g")) {
  # Correspondance between each letter and its move on the plane
  possibleSteps = voc2steps(voc)
  
  # The move at each step for the word
  # word2num is defined in 1_linear_function.R, and for example
  # word2num("ffg") is c(2, 1, 1).
  walk = possibleSteps[word2num(word, voc),,drop=FALSE]
  
  return(walk)
}

##
# Examples
##
walk_word("ffg") 
# Apply g, then f, then f:
# Each row is an move, and each column is a coordinate.
#   coord_1 coord_2
# g       0       1
# f       1       0
# f       1       0

walk_word("ffgFF", voc = c("f", "g", "F"))
# We need to specify "F" in the vocabulary when we add it inside words
#   coord_1 coord_2
# F      -1       0
# F      -1       0
# g       0       1
# f       1       0
# f       1       0

walk_word("fGGhf", voc = c("f", "g", "h", "F", "G"))
#   coord_1 coord_2 coord_3
# f       1       0       0
# h       0       0       1
# G       0      -1       0
# G       0      -1       0
# f       1       0       0

##
# Unit tests
##
checkEqualsNumeric(walk_word(""), numeric(0))
checkEqualsNumeric(walk_word("f"), c(1,0))
checkEqualsNumeric(walk_word("fg")[1,], c(0,1))
checkEqualsNumeric(walk_word("fg")[2,], c(1,0))
checkEqualsNumeric(walk_word("fgG", voc = c("f", "g", "G"))[1,], c(0,-1))
checkEqualsNumeric(walk_word("hhf", voc = c("f", "g", "h"))[3,], c(0,0,1))

#################################
# coord_representation function #
#################################
# Represent the word as a vector
# With b (forward) functions, a word of length l is in \mathbb{R}^{b \times l}
# This representation is harder to read by a human (compared to walk_word),
# but it can be added as features for the df of words (as dummy variables)
coord_representation = function(word, voc = c("f", "g")) {
  walk_word_current = walk_word(word, voc)
  out = as.vector(t(walk_word_current))
  
  if(length(out) > 0) {
    colnames_new = expand.grid(colnames(walk_word_current),
                               paste0("step_", 1:nrow(walk_word_current)))
    colnames_new = apply(colnames_new, 1, paste, collapse = "-")
  
    names(out) = colnames_new
  }
  
  return(out)
}

##
# Examples
##
coord_representation("ffg", voc = c("f", "g"))
# It is, in coordinates, (0,1) then (1,0) then (1,0), so the vector is
# (0,1,1,0,1,0).

coord_representation("g", voc = c("f", "g")) # (0, 1)

coord_representation("gfFf", voc = c("f", "g", "F", "G")) # (1,0,-1,0,1,0,0,1)

coord_representation("hf", voc=c("f", "g", "h")) # (1, 0, 0, 0, 0, 1)
# Note: we cannot compare representation with different vocabularies.

##
# Unit tests
##
checkEqualsNumeric(coord_representation("ffg"), c(0,1,1,0,1,0))
checkEqualsNumeric(coord_representation("g"), c(0,1))
checkEqualsNumeric(coord_representation(""), numeric(0))
checkEqualsNumeric(coord_representation("hGf", voc=c("f", "g", "h", "G")),
                   c(1,0,0,0,-1,0,0,0,1))

#######################################
# Convert words to vectors of R^(b*n) #
#######################################
# When we want to compare words of different size, we can add zeros to have
# the same length representation. We will select n as the larger word size.
coord_representation_n = function(word, n, voc = c("f", "g")) {
  out = coord_representation(word, voc)
  len_voc = length(voc_unique_func(voc))
  out = c(out, rep(0, len_voc * (n - nchar(word))))
  
  colnames_new = expand.grid(paste0("coord_", 1:len_voc),
                             paste0("step_", 1:n))
  colnames_new = apply(colnames_new, 1, paste, collapse = "-")
  names(out) = colnames_new
  return(out)
}

##
# Examples
##
coord_representation_n("", 3) #(0, 0, 0, 0, 0, 0)
coord_representation_n("f", 3) # (1, 0, 0, 0, 0, 0)

# Note: When adding possible (0,0) in the representation, it is exactly
# like dummy variables: to represent b+1 different factors, we need b features.
# For example, with "f", "g", three actions are possible: "f", "g" or "" (the
# word already ended in this last case), and there are three related coords:
# (1,0), (0,1) or (0,0).

##
# Unit tests
##
checkEqualsNumeric(coord_representation_n("ffg", 5), 
                   c(0,1, 1,0, 1,0, 0,0, 0,0))
checkEqualsNumeric(coord_representation_n("g", 3),
                   c(0,1, 0,0, 0,0))
checkEqualsNumeric(coord_representation_n("", 7),
                   rep(0, 7*2))
checkEqualsNumeric(coord_representation_n("hGf", 5, voc=c("f", "g", "h", "G")),
                   c(1,0,0, 0,-1,0, 0,0,1, 0,0,0, 0,0,0))

########################
# compute_coord method #
########################
# Add the coordinates for each word in another dataframe of the word tree
# object.
# This dataframe can be merged with the main dataframe if needed with:
# merge(object@df, object@coord, by = "word")
setGeneric("compute_coord", function(object) {
  standardGeneric("compute_coord")
})

setMethod("compute_coord", "Word tree", 
          function(object) {
  current_words = object@df$word
  max_nchar = max(nchar(current_words))
  voc = object@voc
  out = sapply(current_words, coord_representation_n, max_nchar, voc)
  out = t(out)
  out = data.table(word = rownames(out), out)
  
  object@coord = out
  return(object)
})

##
# Examples
##
compute_coord(pushn(new("Word tree", words_init = c("f")), 2))
compute_coord(pushn(new("Word tree"), 5))

##
# Unit tests
##
wt = compute_coord(pushn(new("Word tree", words_init = c("f")), 2))
checkEqualsNumeric(as.numeric(wt@coord[5,-1]), c(1,0,1,0,0,1))
checkEqualsNumeric(unlist(wt@coord[5,1]), "gff")
rm(wt)

wt = new("Word tree", voc = c("f", "g", "h"), words_init = c(""),
         linfunc = list(f_base, g_base, g_base))
wt = pushn(wt, 2)
wt = compute_coord(wt)
checkEqualsNumeric(as.numeric(wt@coord[5,-1]), c(1,0,0, 1,0,0))
checkEqualsNumeric(unlist(wt@coord[5,1]), "ff")
rm(wt)