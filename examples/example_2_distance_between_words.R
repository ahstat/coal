##########################
# Distance between words #
##########################
# We take the words each in R^(b*n) and compute the distance between each words
# We obtain something like 2 Gaussian together
wt = new("Word tree")
wt = pushn(wt, 10)
wt_coord = compute_coord(wt)@coord[,-"word"]
dist_fgn = dist(wt_coord)
rle_dist_fgn = rle(sort(dist_fgn))
plot(unique(dist_fgn))
plot(rle_dist_fgn[[2]], rle_dist_fgn[[1]])

############################
# Distance between words 2 #
############################
# With 2 coords for each letter (voc of size 2), we can make as follows:
# (0,0) --> 0
# (1,0) --> 1
# (0,1) --> -1
# Note: there is no (1,1).
# This means that vocablary {f,g} is encoded into {-1, 1}
# In that case, we observe something like 3 Gaussian together
idx1 = seq(from = 1, to = ncol(wt_coord), by = 2)
idx2 = seq(from = 2, to = ncol(wt_coord), by = 2)
in_minus1_1 = wt_coord[,idx1, with=FALSE] - wt_coord[,idx2, with=FALSE]
dist_fgn = dist(in_minus1_1)
rle_dist_fgn = rle(sort(dist_fgn))
plot(unique(dist_fgn))
plot(rle_dist_fgn[[2]], rle_dist_fgn[[1]])