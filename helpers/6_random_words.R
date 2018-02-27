# Summary:
# - random_word (function)
# - random_words (function)

########################
# Create a random word #
########################
random_word = function(nSteps = 100, voc = c("f", "g")) {
  indexes = sample(1:length(voc), nSteps, TRUE)
  # Note: we do sample without repetition.
  # When nSteps is large (100 or more), it is unlikely to sample twice the same
  # element.
  # When nSteps is small, we do not need this function because we can pushn
  # all the words.
  word = paste(voc[indexes], collapse = "")
  return(word)
}

##
# Examples
##
random_word(0) # ""
random_word(1) # sometimes "f", sometimes "g"
random_word(50) # may be "fgfgffgfgfgfgggffffggffggggggfgggffffffffggfgggggg"
random_word(20, voc = c("f", "g", "F", "G")) # may be "gGGfGgggfGggfGgfggFG"
random_word(20, voc = c("f", "g", "h")) # may be "hggfffghgggfhfgggfgg"

##
# Unit tests
##
checkEqualsNumeric(random_word(0), "")
checkEqualsNumeric(random_word(1) %in% c("f", "g"), TRUE)

set.seed(2713)
random_1000 = random_word(1000, voc = c("f", "g", "F", "G"))
rle_out = rle(sort(strsplit(random_1000, "")[[1]]))
checkEqualsNumeric(all(rle_out[[1]] > 200), TRUE)
checkEqualsNumeric(rle_out[[2]], c("f", "F", "g", "G"))
rm(random_1000)
rm(rle_out)

#######################
# Create random words #
#######################
# Repetition are allowed when creating random words.
random_words = function(nb_words = 3, nSteps = 100, voc = c("f", "g")) {
  words = c()
  for(i in 1:nb_words) {
    words = c(words, random_word(nSteps, voc))
  }
  return(words)
}

##
# Examples
##
random_words(5, 0) # "" "" "" "" ""
random_words(1, 10) # maybe "ffgfgfgfgg"
random_words(3, 5) # maybe "fgfgg" "gggff" "ffffg"

##
# Unit tests
##
checkEqualsNumeric(random_words(5, 0), rep("", 5))
checkEqualsNumeric(length(random_words(5, 3)), 5)
set.seed(2713)
checkEqualsNumeric(length(unique(random_words(30, 50))), 30)