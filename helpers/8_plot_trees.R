###############################
# Remove first/last character #
###############################
remove_first_charact = function(x) {
  ifelse(x == "", NA, gsub("^.", "", x))
}

remove_last_charact = function(x) {
  ifelse(x == "", NA, gsub(".$", "", x))
}

##
# Examples
##
remove_first_charact("") # NA
remove_first_charact("a") # ""
remove_first_charact("abd") # "bd"
remove_first_charact(" aabx ") # "aabx "

remove_last_charact("") # NA
remove_last_charact("a") # ""
remove_last_charact("abd") # "ab"
remove_last_charact(" aabx ") # " aabx"

##
# Unit tests
##
checkEquals(remove_first_charact(""), NA)
checkEquals(remove_first_charact("a"), "")
checkEquals(remove_first_charact("abd"), "bd")
checkEquals(remove_first_charact(" aabx "), "aabx ")
checkEquals(remove_last_charact(""), NA)
checkEquals(remove_last_charact("a"), "")
checkEquals(remove_last_charact("abd"), "ab")
checkEquals(remove_last_charact(" aabx "), " aabx")

################################
# Get the parents of the words #
################################
# Get the parents, and iterates until the root (or until specific number of
# iterations)
get_parents = function(words, nb_iterations_up = NA) {
  # Iterations up to the empty word when nb = NA
  max_nchar = max(nchar(words))
  if(is.na(nb_iterations_up) | nb_iterations_up > max_nchar) {
    nb_iterations_up = max_nchar
  }
  
  if(nb_iterations_up > 0) {
    for(i in 1:nb_iterations_up) {
      words = c(words, remove_first_charact(words))
      words = unique(words)
    }
  }
  
  idx_to_remove = which(is.na(words) | words == "")
  if(length(idx_to_remove) > 0) {
    words = words[-idx_to_remove]
  }
  
  edges = matrix(c(remove_first_charact(words), words), ncol = 2)
  colnames(edges) = c("parent", "child")
  
  return(edges)
}

##
# Examples
##
get_parents("ffg") # all the tree from "" to "ffg"
# parent child
# "fg"   "ffg"
# "g"    "fg" 
# ""     "g"  

get_parents(c("ffg", "ggg")) # all the tree from "" to "ffg" and "ggg"
# parent child
# "fg"   "ffg"
# "gg"   "ggg"
# "g"    "fg" 
# "g"    "gg" 
# ""     "g"

get_parents(c("ffg", "ggg"), nb_iterations_up = 1) # only one step back
# parent child
# "fg"   "ffg"
# "gg"   "ggg"
# "g"    "fg" 
# "g"    "gg" 

# A larger example
words = c(random_words(3, 5), random_words(20, 20))
get_parents(words)
rm(words)

##
# Unit tests
##
checkEquals(get_parents("ffg")[,"parent"], c("fg", "g", ""))
checkEquals(get_parents("ffg")[,"child"], c("ffg", "fg", "g"))
checkEqualsNumeric(get_parents("ffg")[1,], c("fg", "ffg"))
checkEqualsNumeric(get_parents("ffg", nb_iterations_up = 0)[,"parent"], 
                   "fg")
checkEqualsNumeric(get_parents("ffg", nb_iterations_up = 1)[,"parent"], 
                   c("fg", "g"))
checkEquals(get_parents(c("ffg", "ggfg"))[,"parent"], 
                        c("fg", "gfg", "g", "fg", ""))

##########################
# Plot the tree of words #
##########################
plot_tree = function(edges) {
  idx_to_change = which(edges == "")
  if(length(idx_to_change) > 0) {
    edges[idx_to_change] = "id"
  }
  
  G = graph_from_edgelist(edges)
  
  co <- layout.reingold.tilford(G, params=list(root=1)) 
  co <- layout_as_tree(G) 
  plot(G, layout=co, ylim=c(1,-1))
}

##
# Examples
##
set.seed(2713)
words = random_words(10, 5)
plot_tree(get_parents(words))
dev.off()
rm(words)

set.seed(2713)
words = c(random_words(3, 5), random_words(2, 20))
plot_tree(get_parents(words))
dev.off()
rm(words)

set.seed(2713)
words = random_words(10, 5)
plot_tree(get_parents(words, nb_iterations_up = 3))
dev.off()
rm(words)

# A large one
set.seed(2713)
words = c(random_words(3, 5), random_words(10, 10))
plot_tree(get_parents(words))
dev.off()
rm(words)