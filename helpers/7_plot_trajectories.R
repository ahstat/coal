###############################
# Trajectory of a word on R^b #
###############################
trajectory_word = function(word, voc = c("f", "g"), init = NA) {
  # The move at each step for the word
  walk = walk_word(word, voc)
  
  # Default value for init is 0
  if(is.na(init[1])) {
    init = rep(0, ncol(walk))
  }
  
  # The trajectory from init according to each move
  traj = rbind(t(init), walk)
  traj = apply(traj, 2, cumsum)
  
  return(traj)
}

##
# Examples
##
trajectory_word("ffg")
# From (0,0), move up, then right, then right
#   coord_1 coord_2
#         0       0
# g       0       1
# f       1       1
# f       2       1

trajectory_word("GFgf", voc = c("f", "g", "F", "G"))
# From (0,0), move right, then up, then left, then down
#   coord_1 coord_2
#         0       0
# f       1       0
# g       1       1
# F       0       1
# G       0       0

trajectory_word("GFgfh", voc = c("f", "g", "h", "F", "G"), init = c(1, -1, 0))
#   coord_1 coord_2 coord_3
#         1      -1       0
# h       1      -1       1
# f       2      -1       1
# g       2       0       1
# F       1       0       1
# G       1      -1       1

##
# Unit tests
##
checkEqualsNumeric(trajectory_word("ffg")[4,], c(2,1))
checkEqualsNumeric(trajectory_word("GFgf", voc = c("f", "g", "F", "G"))[4,],
                   c(0,1))
checkEqualsNumeric(trajectory_word("GFgfh", voc = c("f", "g", "h", "F", "G"), 
                                   init = c(1, -1, 0))[6,],
                   c(1,-1,1))

########################################################
# Helper function to have the trajectory ready to plot #
########################################################
# To know the lines to plot and the related colors from a trajectory
# Only works in dimension 2, but allows inverse in the vocabulary.
trajectory_word_ready_to_plot = function(traj) {
  if(ncol(traj) != 2) {
    stop("Need a 2-dimensional trajectory.")
  }
  
  # We want to find duplicate moves (to bold them).
  # Each segment is represented by 4 coordinates: 
  # (from_x, from_y) --- (to_x, to_y)
  from_to = cbind(traj[-nrow(traj),], traj[-1,])
  colnames(from_to) = c("from_x", "from_y", "to_x", "to_y")
  rownames(from_to) = NULL
  from_to = data.frame(from_to)
  
  # A segment has no direction on the plot, so we change column according 
  # to an order on N^2.
  # For example segment (2, 0) -- (1, 0) can be converted to the segment
  # (1, 0) -- (2, 0).
  # This is done in order to find duplicates.
  from_is_higher = which((from_to$from_x > from_to$to_x) | 
                           (from_to$from_x == from_to$to_x &
                            from_to$from_y > from_to$to_y))
  from_to[from_is_higher,] = from_to[from_is_higher,
                                     c("to_x", "to_y", "from_x", "from_y")]
  
  # Add a color ramp for the trajectory
  len = nrow(from_to)
  colfunc = colorRampPalette(c("black", "red"))
  from_to$col = colfunc(len)
  
  # Count the duplicates segment
  from_to = from_to %>% 
    group_by(from_x, from_y, to_x, to_y) %>% 
    summarize(count = n(), col = max(col)) %>%
    ungroup
  
  return(from_to)
}

##
# Examples
##
traj = trajectory_word("ffg")
trajectory_word_ready_to_plot(traj)
rm(traj)

traj = trajectory_word("ffFFffF", voc = c("f", "g", "F", "G"))
trajectory_word_ready_to_plot(traj)
rm(traj)

traj = trajectory_word("gfGFgf", voc = c("f", "g", "F", "G"))
trajectory_word_ready_to_plot(traj)
rm(traj)

# # error, need a 2-dimensional trajectory
# traj = trajectory_word("hgf", voc = c("f", "g", "h"))
# trajectory_word_ready_to_plot(traj) 

##
# Unit tests
##
# difficult to test this easily
traj = trajectory_word("ffg")
traj_ready = trajectory_word_ready_to_plot(traj)
checkEqualsNumeric(traj_ready$count, c(1,1,1))
rm(traj)
rm(traj_ready)

traj = trajectory_word("ffFFffF", voc = c("f", "g", "F", "G"))
traj_ready = trajectory_word_ready_to_plot(traj)
checkEqualsNumeric(traj_ready$count, c(4,3))
rm(traj)
rm(traj_ready)

traj = trajectory_word("gfGFgf", voc = c("f", "g", "F", "G"))
traj_ready = trajectory_word_ready_to_plot(traj)
checkEqualsNumeric(traj_ready$count, c(1,2,1,2))
rm(traj)
rm(traj_ready)

##########################
# Plotting a unique word #
##########################
plot_path_word = function(word, voc = c("f", "g"), ...) {
  traj = trajectory_word(word, voc)
  from_to = trajectory_word_ready_to_plot(traj)
  
  # Plotting each segment
  plot(traj[,"coord_1"], traj[,"coord_2"], type = "n", asp = 1,
       xlab = voc[1], ylab = voc[2], ...)
  for(i in 1:nrow(from_to)) {
    lines(from_to[i,c("from_x", "to_x")], 
          from_to[i,c("from_y", "to_y")], 
          lwd = from_to$count[i],
          col = from_to$col[i])
  }
  lines(traj[1,"coord_1"], traj[1,"coord_2"], type = "p", pch = 19, lwd = 5)
}

##
# Examples
##
# The color indicates the time from beginning (black) to end (red)
set.seed(2713)
voc = c("f", "g")
word = random_word(nSteps = 15, voc)
plot_path_word(word, voc, main = "A forward trajectory")
dev.off()
rm(voc)
rm(word)

set.seed(2713)
voc = c("f", "g", "F", "G")
word = random_word(nSteps = 15, voc)
plot_path_word(word, voc, main = "Random walk")
dev.off()
rm(voc)
rm(word)

set.seed(2713)
voc = c("f", "g", "F")
word = random_word(nSteps = 15, voc)
plot_path_word(word, voc, main = "Walk without going down")
dev.off()
rm(voc)
rm(word)

###########################
# Plotting multiple words #
###########################
plot_path_multiple_words = function(words, voc, ...) {
  trajs = lapply(words, trajectory_word, voc = voc)
  from_tos = lapply(trajs, trajectory_word_ready_to_plot)
  for(i in 1:length(from_tos)) {
    from_tos[[i]]$word_nb = i
  }
  from_to = bind_rows(from_tos)
  
  from_to = from_to %>% 
    group_by(from_x, from_y, to_x, to_y) %>% 
    summarize(count = n(), word_nb = max(word_nb))
  
  len = length(words)
  colramp <- colorRampPalette(c("blue", "black"))(len)
  from_to$col = colramp[from_to$word_nb]
  
  # Plotting each segment
  plot(1:2, 1:2, type = "n", asp = 1,
       xlab = voc[1], ylab = voc[2],
       xlim = range(from_to[,c("from_x", "to_x")]),
       ylim = range(from_to[,c("from_y", "to_y")]), ...)
  
  for(i in 1:nrow(from_to)) {
    lines(from_to[i,c("from_x", "to_x")], 
          from_to[i,c("from_y", "to_y")], 
          lwd = from_to$count[i],
          col = from_to$col[i])
  }
  lines(trajs[[1]][1,"coord_1"], trajs[[1]][1,"coord_2"], 
        type = "p", pch = 19, lwd = 5)
}

##
# Examples
##
# The color indicates the trajectory (one color for one trajectory)
set.seed(2713)
voc = c("f", "g")
words = random_words(4, 11, voc)
plot_path_multiple_words(words, voc, main = "Four forward walks")
dev.off()
rm(voc)
rm(words)

set.seed(2713)
voc = c("f", "g")
words = random_words(20, 4, voc)
plot_path_multiple_words(words, voc, main = "Twenty forward walks")
dev.off()
rm(voc)
rm(words)

set.seed(2713)
voc = c("f", "g", "F", "G")
words = random_words(3, 20, voc)
plot_path_multiple_words(words, voc, main = "Three random walks")
dev.off()
rm(voc)
rm(words)

set.seed(2713)
voc = c("f", "g", "F", "G")
words = random_words(20, 4, voc)
plot_path_multiple_words(words, voc, main = "Twenty random walks")
dev.off()
rm(voc)
rm(words)