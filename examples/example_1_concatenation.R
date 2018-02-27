##########################
# Concatenation of words #
##########################
# We concatenate all words together from "" to "gggggggggg" in the common order
# We have: "" "f" "g" "ff" "gf" "fg" "gg" "fff" "gff" "fgf" "ggf" "ffg"...
# So we get: "fgffgffgggfffgfffgfggfffggfgfggggg"
# We count how many we have the same letter: 1 f, then 1 g, then 2 f, etc.:
# 1 1 2 1 2 3 3 1 3 1 1 2 3 2 1 1 1 5
wt = new("Word tree")
wt = pushn(wt, 10)
word2num_wt = sapply(wt@df$word, word2num)
word2num_wt = sapply(word2num_wt, rev)
converted = as.vector(unlist(word2num_wt))
diffseq = rle(converted)[[1]]
plot(diffseq)
cat(diffseq[1:200])
# 200 first terms of diffseq (not in OEIS):
# 1 1 2 1 2 3 3 1 3 1 1 2 3 2 1 1 1 5 4 1 4 1 2 2
# 4 1 1 1 1 1 2 2 1 3 4 2 2 1 1 1 1 3 1 1 2 3 1 2
# 1 7 5 1 5 1 3 2 5 1 2 1 1 1 3 2 2 3 5 1 1 1 2 1
# 2 1 1 1 1 2 1 1 3 2 1 1 1 2 2 3 1 4 5 2 3 1 1 1
# 2 3 2 1 2 1 1 2 1 1 1 1 1 2 1 4 1 1 3 3 2 2 1 1
# 1 4 1 2 2 4 1 3 1 9 6 1 6 1 4 2 6 1 3 1 1 1 4 2
# 3 3 6 1 2 1 2 1 3 1 1 1 2 2 1 1 4 2 2 1 1 2 3 3
# 2 4 6 1 1 1 3 1 2 1 2 1 1 2 2 1 3 1 1 1 1 1 1 1
# 1 1 2 2 1 1 1 3
#
############################
# Concatenation of words 2 #
############################
# Since we apply from right to left, we can reverse each word before
# concatenation. The reverse words are:
# We have: "" "f" "g" "ff" "fg" "gf" "gg" "fff" "ffg" "fgf" "fgg" "gff"...
# So we get: "fgfffggfggfffffgfgffgggff"
# We get a different sequence (but similar)
# 1 1 3 2 1 2 5 1 1 1 2 3 2
word2num_wt = sapply(wt@df$word, word2num)
converted = as.vector(unlist(word2num_wt))
diffseq = rle(converted)[[1]]
plot(diffseq)
cat(diffseq[1:200])
# 200 first terms of diffseq (not in OEIS):
# 1 1 3 2 1 2 5 1 1 1 2 3 2 1 1 3 1 3 7 1 2 1 3 2
# 1 1 3 1 1 1 1 2 2 4 3 1 2 2 1 1 1 1 1 4 2 2 1 4
# 1 4 9 1 3 1 4 2 2 1 4 1 1 1 2 2 3 3 1 1 4 1 2 1
# 1 1 1 1 2 1 1 2 1 2 3 2 1 1 1 3 2 5 4 1 3 2 2 1
# 1 1 2 3 1 1 2 1 1 1 1 2 1 2 1 1 1 5 3 2 2 3 1 1
# 1 2 1 5 2 3 1 5 1 5 11 1 4 1 5 2 3 1 5 1 1 1 3 2
# 4 3 2 1 5 1 2 1 2 1 1 1 3 1 1 2 2 2 4 2 1 1 2 3
# 3 4 1 1 5 1 3 1 1 1 2 1 2 1 2 2 1 1 1 1 3 1 1 1
# 1 1 1 1 1 2 2 1