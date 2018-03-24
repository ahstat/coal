#######################
# word2index function #
#######################
# Convert a word into an integer number.
# This is the 'common' index on a b-ary tree (b the length of the voc).
# For example for b = 3:
# id=1, 
# f=2, g=3, h=4, 
# ff=5, gf=6, hf=7, fg=8, gg=9, hg=10, fh=11, gh=12, hh=13,
# fff=14, etc.
# (For base 2 the formula is straighforward, less easy for all bases b)
word2index = function(word, voc = c("f", "g")) {
  b = length(voc)
  # word2num is defined in 1_linear_function.R, and for example
  # word2num("ffg") is c(2, 1, 1).
  w2num = word2num(word, voc)
  out = 0
  
  len = length(w2num)
  if(len > 0) {
    # Maybe a formal formula exists
    for(i in 1:len) {
      out = b*out + w2num[i]
    }
  }
  return(out + 1) # +1 to have index beginning with 1
}

##
# Examples
##
word2index("") # the null word is 1
word2index("f") # the first letter of the voc is 2
word2index("g") # the second letter of the voc is 3
word2index("ff") # the first word of two letter is length(voc)+2
word2index("hg", voc = c("f", "g", "h")) # 10 as expected in the intro

##
# Unit tests
##
checkEquals(word2index(""), 1)
checkEquals(word2index("fff"), 8)
checkEquals(word2index("ffff"), 16)
checkEquals(word2index("fff", voc = c("f", "g", "h")), 14)
checkEquals(word2index("hg", voc = c("f", "g", "h")), 10)
checkEquals(word2index("fffff", voc = c("f", "g", "h", "i", "j")), 782)

###########################
# add_common_index method #
###########################
# Method to add common index provided by word2index as a column of df
setGeneric("add_common_index", function(object) {
  standardGeneric("add_common_index")
})

setMethod("add_common_index", "Word tree", 
  function(object) {
    current_words = object@df$word
    voc = object@voc
    out = sapply(current_words, word2index, voc)
    object@df$common_index = out
    return(object)
})

##
# Examples
##
add_common_index(pushn(new("Word tree"), 5))
wt = add_common_index(pushn(new("Word tree", voc = c("f", "g", "h"),
                                linfunc = list(f_base, g_base, g_base)), 5))
# print(wt@df, topn = 100) # visual checking
rm(wt)

# with a different init, the rows and common_index are different
add_common_index(pushn(new("Word tree", words_init = c("f")), 2))
add_common_index(pushn(new("Word tree", 
                           words_init = c("f"),
                           voc = c("f", "g", "h"),
                           linfunc = list(f_base, g_base, g_base)), 2))

##
# Unit tests
##
wt = add_common_index(pushn(new("Word tree", words_init = c("f")), 2))
checkEqualsNumeric(wt@df$common_index, c(2,4:5,8:11))
rm(wt)

wt = add_common_index(pushn(new("Word tree", 
                                words_init = c("f"),
                                voc = c("f", "g", "h"),
                                linfunc = list(f_base, g_base, g_base)), 2))
checkEqualsNumeric(wt@df$common_index, c(2, 5:7, 14:22))
rm(wt)

########################
# word2index_signature #
########################
# Now, there is no indexing linked to the signature.
# This is because there is more than one possible ordering.
# First, we can order with the level and the signature:
# f < g < ff < fg < gg < fff < etc.
# Then, we need to order the elements with the same signature.
# Each word is a trajectory, so it is like to compare two trajectories.
# But the operation 'trajectory V < trajectory W' does not define a total order
# For example, fggf and gffg cannot be compared.
# (the partial order is interesting, showing line, squares and cubes...)

#######################################
# Partial order on the words function #
#######################################
# Given the words with a given signature, we describe the partial ordering of
# those words.

###################################
# Partial order plotting function #
###################################
# Plotting as a graph the partial ordering (more or less a Hasse diagram)
