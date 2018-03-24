##############################
# Word tree class definition #
##############################
# A word tree is:
# - a vocabulary (for example letters "f" and "g"),
# - initialization words, all of the same length (can be empty "", but can also
#   be for example c("ffgg", "gffg")),
# - linear functions related to the vocabulary (for example f_base and g_base).
#
# Optionally, it is possible to specify:
# - x_init are the values related to the word "". It can be NULL; of size 1; or
#   a vector of bigq. It is never a matrix.
# - x_integer_only is a boolean turning non integer values to NA.
#
# The class will compute itself:
# - level, i.e. the length of the larger word,
# - df, the dataframe giving information about each word,
# - coord, the coordinates of each word, which is not computed by default (can
#   computed with compute_coord in 4_word2coordinates.R).
setClass("Word tree",
         # List of attributes
         representation(voc = "character",
                        level = "integer",
                        df = "data.table",
                        coord = "data.table",
                        linfunc = "list",
                        x_init = "bigq",
                        x_integer_only = "logical")
         # Here, initialize method is called automatically,
         # Then prototype(...) is not needed anymore.
         # Also, validity = ... seems not be called, so all included
         # in initialize method.
)

#################################################################
# Restrict data.table to matrix values with at least one non-NA #
#################################################################
# When x_integer_only == TRUE, we delete all rows of the dataframe with
# matrix values containing only NA values.
# We compute this function during:
# - initialization, 
# - push.
# The function is a little complex because we want to keep a list containing
# values at each row of the dataframe (why?: because of gmp with data.table,
# see the initialization method for more details).
df_to_integer_df = function(df) {
  # Replace non integer values with NA
  out_values = lapply(df$values, 
                      function(x){
                        list(to_integer_mat(x[[1]]))})
  if(length(out_values) == 1) {
    # a strange data.table feature simplifying a list
    # of length 1 to deal with...
    df$values[[1]] = out_values
  } else {
    df$values = out_values
  }
  
  # Delete matrix containing only NA
  idx_na = sapply(df$values, 
                  function(x){is_NA_mat(x[[1]])})
  idx_to_del = which(idx_na)
  if(length(idx_to_del) > 0) {
    df = df[-idx_to_del]
  }
  return(df)
}

##
# Examples
##
## keep the matrix if non NA
df = data.table(values = list(list(matrix(as.bigq(1)))))
df_to_integer_df(df) 
## delete the matrix if NA
df = data.table(values = list(list(matrix(as.bigq(NA)))))
df_to_integer_df(df)

##
# Unit tests
##
## keep the matrix if non NA
df = data.table(values = list(list(matrix(as.bigq(1)))))
checkEquals(df_to_integer_df(df)$values[[1]][[1]], 1)
## delete the matrix if NA
df = data.table(values = list(list(matrix(as.bigq(NA)))))
checkEquals(df_to_integer_df(df)$values, list())
## delete the matrix if all non integer
df = data.table(values = list(list(matrix(as.bigq(1L, 3L)))))
checkEquals(df_to_integer_df(df)$values, list())

## test with 2x2 matrices
df = data.table(values = list(list(matrix(as.bigq(c(1, 2, 3, 4)), ncol = 2))))
checkEquals(df_to_integer_df(df)$values[[1]][[1]],
            matrix(as.bigq(c(1, 2, 3, 4)), ncol = 2))

df = data.table(values = list(list(matrix(c(as.bigq(1L, 1L),
                                            as.bigq(1L, 3L),
                                            as.bigq(4L, 3L),
                                            as.bigq(4L, 1L)), 
                                          ncol = 2))))
checkEquals(df_to_integer_df(df)$values[[1]][[1]],
            matrix(as.bigq(c(1, NA, NA, 4)), ncol = 2))

df = data.table(values = list(list(matrix(c(as.bigq(1L, 3L),
                                            as.bigq(1L, 3L),
                                            as.bigq(4L, 3L),
                                            as.bigq(4L, 8L)), 
                                          ncol = 2))))
checkEquals(df_to_integer_df(df)$values, list())

###################################
# Word tree initialization method #
###################################
setMethod("initialize", signature  = "Word tree",
          # Constructor ( see stackoverflow.com/questions/38275161 )
          definition = function(.Object,
                                voc = c("f", "g"),
                                words_init = "",
                                linfunc = c(f_base, g_base),
                                x_init = as.bigq(NULL),
                                x_integer_only = FALSE) {
            ##
            # Initialize slots (=attributes)
            ##
            .Object@voc = voc
            .Object@x_init = x_init
            .Object@coord = data.table()
            .Object@linfunc = linfunc
            .Object@x_integer_only = x_integer_only
            
            linfun = sapply(words_init, linfun_init, voc, linfunc)
            .Object@df = data.table(word = words_init,
                                    level = nchar(words_init),
                                    linfun = linfun)
            .Object@level = max((.Object@df)$level)
            
            ## Initialization for values
            # R crashes if printing as.bigq(NULL) in a data.table
            # This is because gmp implementation of matrices does not
            # fit with data.table implementation
            if(length(x_init) != 0) {
              .Object@df$values = lapply(linfun, function(func) {
                a = func@seq_coeff_x
                b = func@seq_coeff_1
                mat = compute_matrix_values(x_init, a, b)
                # Convert to list to prevent error when mat has dimension 2
                # or more (problem between data.table and gmp)
                mat = list(mat)
                
                # Prevent some unlist when only one element in words_init
                if(length(words_init) == 1) {
                  mat = list(mat)
                }
                
                return(mat)
              })
              
              if(x_integer_only) {
                .Object@df = df_to_integer_df(.Object@df)
              }
            }

            ##
            # Check validity
            ##
            ## Check that all init words have the same level
            init_level = unique((.Object@df)$level)
            if(length(init_level) > 1) {
              stop("All init words should have same size initially, ",
                   "now some have length ", init_level[1], " ",
                   "and some have length ", init_level[2], ".")
            }
            
            return (.Object)
          }
)

##
# Examples
##
new("Word tree")
new("Word tree", voc = c("f", "g"), words_init = "")
new("Word tree", words_init = c("gff", "fgf", "ffg"))
# new("Word tree", words_init = c("f", "gf")) # Throws an error
new("Word tree", words_init = c("gff", "fgf", "ffg"), x_init = as.bigq(1L, 1L))
new("Word tree", words_init = c(""), 
    x_init = as.bigq(1L, 1L), x_integer_only = TRUE)
new("Word tree", words_init = c("ggf", "gfg", "fgg"), 
    x_init = as.bigq(1L, 1L), x_integer_only = TRUE)

##
# Unit tests
##
## 1. Default values
voc = c("f", "g")
words_init = ""
linfunc = c(f_base, g_base)
x_init = as.bigq(NULL)
wt = new("Word tree", voc = voc,
         words_init = words_init, linfunc = linfunc,
         x_init = x_init)
df = wt@df

# common to all
checkEquals(wt@voc, voc)
checkEquals(wt@level, nchar(words_init[1]))
checkEquals(dim(wt@coord), c(0,0))
checkEquals(wt@x_init, x_init)
checkEquals(wt@linfunc, linfunc)
checkEquals(nrow(df), length(words_init))
checkEquals(df$word, words_init)

# specific
checkEquals(df$linfun[[1]], id_n(1))
checkEquals(is.null(df$values), TRUE)

## 2. x_init of size 1
voc = c("f", "g")
words_init = ""
linfunc = c(f_base, g_base)
x_init = as.bigq(1L, 1L)
wt = new("Word tree", voc = voc,
         words_init = words_init, linfunc = linfunc,
         x_init = x_init)
df = wt@df

# common to all
checkEquals(wt@voc, voc)
checkEquals(wt@level, nchar(words_init[1]))
checkEquals(dim(wt@coord), c(0,0))
checkEquals(wt@x_init, x_init)
checkEquals(wt@linfunc, linfunc)
checkEquals(nrow(df), length(words_init))
checkEquals(df$word, words_init)

# specific
checkEquals(df$linfun[[1]], id_n(1))
checkEquals(dim(df$values[[1]][[1]]), c(1,1))
checkEquals(df$values[[1]][[1]], x_init)

## 3. x_init of size greater than 1
voc = c("f", "g")
words_init = ""
linfunc = c(f_base, g_base)
x_init = c(as.bigq(1L, 1L), as.bigq(1L, 3L))
wt = new("Word tree", voc = voc,
         words_init = words_init, linfunc = linfunc,
         x_init = x_init)
df = wt@df

# common to all
checkEquals(wt@voc, voc)
checkEquals(wt@level, nchar(words_init[1]))
checkEquals(dim(wt@coord), c(0,0))
checkEquals(wt@x_init, x_init)
checkEquals(wt@linfunc, linfunc)
checkEquals(nrow(df), length(words_init))
checkEquals(df$word, words_init)

# specific
checkEquals(df$linfun[[1]], id_n(1))
checkEquals(dim(df$values[[1]][[1]]), c(2,1)) # x in rows, (a,b,c,d) in col
checkEquals(df$values[[1]][[1]], x_init)

## 4. x_init of size 1 with one init word
voc = c("f", "g")
words_init = c("f")
linfunc = c(f_base, g_base)
x_init = as.bigq(1L, 1L)
wt = new("Word tree", voc = voc,
         words_init = words_init, linfunc = linfunc,
         x_init = x_init)
df = wt@df

# common to all
checkEquals(wt@voc, voc)
checkEquals(wt@level, nchar(words_init[1]))
checkEquals(dim(wt@coord), c(0,0))
checkEquals(wt@x_init, x_init)
checkEquals(wt@linfunc, linfunc)
checkEquals(nrow(df), length(words_init))
checkEquals(df$word, words_init)

# specific
checkEquals(df$linfun[[1]], f_base)
checkEquals(dim(df$values[[1]][[1]]), c(1,1))
checkEquals(df$values[[1]][[1]], (1-1)/3)

## 5. x_init of size greater than 1 with one init word
voc = c("f", "g")
words_init = c("f")
linfunc = c(f_base, g_base)
x_init = c(as.bigq(1L, 1L), as.bigq(1L, 3L))
wt = new("Word tree", voc = voc,
         words_init = words_init, linfunc = linfunc,
         x_init = x_init)
df = wt@df

# common to all
checkEquals(wt@voc, voc)
checkEquals(wt@level, nchar(words_init[1]))
checkEquals(dim(wt@coord), c(0,0))
checkEquals(wt@x_init, x_init)
checkEquals(wt@linfunc, linfunc)
checkEquals(nrow(df), length(words_init))
checkEquals(df$word, words_init)

# specific
checkEquals(df$linfun[[1]], f_base)
checkEquals(dim(df$values[[1]][[1]]), c(2,1)) # x in rows, (a,b,c,d) in col
checkEquals(df$values[[1]][[1]], (x_init - 1) * as.bigq(1L, 3L))

## 6. x_init of size 1 with some init words
voc = c("f", "g")
words_init = c("gff", "fgf", "ffg")
linfunc = c(f_base, g_base)
x_init = as.bigq(1L, 1L)
wt = new("Word tree", voc = voc,
         words_init = words_init, linfunc = linfunc,
         x_init = x_init)
df = wt@df

# common to all
checkEquals(wt@voc, voc)
checkEquals(wt@level, nchar(words_init[1]))
checkEquals(dim(wt@coord), c(0,0))
checkEquals(wt@x_init, x_init)
checkEquals(wt@linfunc, linfunc)
checkEquals(nrow(df), length(words_init))
checkEquals(df$word, words_init)

# specific
checkEquals(df$linfun[[1]], sapply(words_init, linfun_init, voc, linfunc)[[1]])
checkEquals(dim(df$values[[1]][[1]]), c(1,1))
# f(1) = (1-1)/3 = 0 ; ff(1) = -1/3 ; gff(1) = -2/3
# gf(1) = 0 ; fgf(1) = -1/3
# g(1) = 2; fg(1) = 1/3 ; ffg(1) = -2/9
checkEquals(df$values[[1]][[1]], -as.bigq(2L, 3L))
checkEquals(df$values[[2]][[1]], -as.bigq(1L, 3L))
checkEquals(df$values[[3]][[1]], -as.bigq(2L, 9L))

## 7. x_init of size greater than 1 with some init words
voc = c("f", "g")
words_init = c("gff", "fgf", "ffg")
linfunc = c(f_base, g_base)
x_init = c(as.bigq(1L, 1L), as.bigq(1L, 3L))
wt = new("Word tree", voc = voc,
         words_init = words_init, linfunc = linfunc,
         x_init = x_init)
df = wt@df

# common to all
checkEquals(wt@voc, voc)
checkEquals(wt@level, nchar(words_init[1]))
checkEquals(dim(wt@coord), c(0,0))
checkEquals(wt@x_init, x_init)
checkEquals(wt@linfunc, linfunc)
checkEquals(nrow(df), length(words_init))
checkEquals(df$word, words_init)

# specific
checkEquals(df$linfun[[1]], sapply(words_init, linfun_init, voc, linfunc)[[1]])
checkEquals(dim(df$values[[1]][[1]]), c(2,1)) # x in rows, (a,b,c,d) in col
# f(1/3) = -2/9 ; ff(1/3) = -11/27 ; gff(1/3) = -22/27
# gf(1/3) = -4/9 ; fgf(1/3) = -13/27
# g(1/3) = 2/3 ; fg(1/3) = -1/9 ; ffg(1/3) = -10/27
checkEquals(df$values[[1]][[1]], c(-as.bigq(2L, 3L), -as.bigq(22, 27)))
checkEquals(df$values[[2]][[1]], c(-as.bigq(1L, 3L), -as.bigq(13, 27)))
checkEquals(df$values[[3]][[1]], c(-as.bigq(2L, 9L), -as.bigq(10, 27)))

## 8. voc with one letter
voc = c("f")
words_init = c("")
linfunc = c(f_base)
x_init = c(as.bigq(1L, 1L))
wt = new("Word tree", voc = voc, words_init = words_init,
         x_init = x_init, linfunc = linfunc)
df = wt@df

# common to all
checkEquals(wt@voc, voc)
checkEquals(wt@level, nchar(words_init[1]))
checkEquals(dim(wt@coord), c(0,0))
checkEquals(wt@x_init, x_init)
checkEquals(wt@linfunc, linfunc)
checkEquals(nrow(df), length(words_init))
checkEquals(df$word, words_init)

# specific
checkEquals(df$linfun[[1]], id_n(1))
checkEquals(dim(df$values[[1]][[1]]), c(1,1))
checkEquals(df$values[[1]][[1]], 1)

## 9. voc with one letter bis
voc = c("f")
words_init = c("ff")
linfunc = c(f_base)
x_init = c(as.bigq(1L, 1L), as.bigq(1L, 3L))
wt = new("Word tree", voc = voc, words_init = words_init,
         x_init = x_init, linfunc = linfunc)
df = wt@df

# common to all
checkEquals(wt@voc, voc)
checkEquals(wt@level, nchar(words_init[1]))
checkEquals(dim(wt@coord), c(0,0))
checkEquals(wt@x_init, x_init)
checkEquals(wt@linfunc, linfunc)
checkEquals(nrow(df), length(words_init))
checkEquals(df$word, words_init)

# specific
checkEquals(df$linfun[[1]], sapply(words_init, linfun_init, voc, linfunc)[[1]])
checkEquals(dim(df$values[[1]][[1]]), c(2,1)) # x in rows, (a,b,c,d) in col
checkEquals(df$values[[1]][[1]], c(-as.bigq(1L, 3L), -as.bigq(11, 27)))

## 10. voc with four letters
voc = c("f", "g", "F", "G")
words_init = c("")
linfunc = c(f_base, g_base, F_base, G_base)
x_init = c(as.bigq(1L, 1L), as.bigq(1L, 3L))
wt = new("Word tree", voc = voc, words_init = words_init,
         x_init = x_init, linfunc = linfunc)
df = wt@df

# common to all
checkEquals(wt@voc, voc)
checkEquals(wt@level, nchar(words_init[1]))
checkEquals(dim(wt@coord), c(0,0))
checkEquals(wt@x_init, x_init)
checkEquals(wt@linfunc, linfunc)
checkEquals(nrow(df), length(words_init))
checkEquals(df$word, words_init)

# specific
checkEquals(df$linfun[[1]], id_n(1))
checkEquals(dim(df$values[[1]][[1]]), c(2,1)) # x in rows, (a,b,c,d) in col
checkEquals(df$values[[1]][[1]], x_init)

## 11. voc with four letters bis
voc = c("f", "g", "F", "G")
words_init = c("ffG")
linfunc = c(f_base, g_base, F_base, G_base)
x_init = c(as.bigq(1L, 1L))
wt = new("Word tree", voc = voc, words_init = words_init,
         x_init = x_init, linfunc = linfunc)
df = wt@df

# common to all
checkEquals(wt@voc, voc)
checkEquals(wt@level, nchar(words_init[1]))
checkEquals(dim(wt@coord), c(0,0))
checkEquals(wt@x_init, x_init)
checkEquals(wt@linfunc, linfunc)
checkEquals(nrow(df), length(words_init))
checkEquals(df$word, words_init)

# specific
checkEquals(df$linfun[[1]], sapply(words_init, linfun_init, voc, linfunc)[[1]])
checkEquals(dim(df$values[[1]][[1]]), c(1,1))
# G(1) = 1/2 ; fG(1) = -1/6 ; ffG(1) = -7/18
checkEquals(df$values[[1]][[1]], -as.bigq(7L, 18L))

## 12. linfunc containing more than one (a,b) couple
# ax+b is (1/3)x-(1/3) or (1/4)x-(1/4)
# cx+d is 2x+0 or 3x+0
f_test = new("Linear function", 
             "a", "b", 
             c(as.bigq(1L, 3L), as.bigq(1L, 4L)),
             c(-as.bigq(1L, 3L), -as.bigq(1L, 4L)))
g_test = new("Linear function", 
             "c", "d",
             c(as.bigq(2L), as.bigq(3L)), 
             c(as.bigq(0L), as.bigq(0L)))
voc = c("f", "g")
words_init = c("")
linfunc = c(f_test, g_test)
x_init = c(as.bigq(5L, 4L))
wt = new("Word tree", voc = voc, words_init = words_init,
         x_init = x_init, linfunc = linfunc)
df = wt@df

# common to all
checkEquals(wt@voc, voc)
checkEquals(wt@level, nchar(words_init[1]))
checkEquals(dim(wt@coord), c(0,0))
checkEquals(wt@x_init, x_init)
checkEquals(wt@linfunc, linfunc)
checkEquals(nrow(df), length(words_init))
checkEquals(df$word, words_init)

# specific
checkEquals(df$linfun[[1]], id_n(2))
checkEquals(dim(df$values[[1]][[1]]), c(1,2)) # x in rows, (a,b,c,d) in col
# G(1) = 1/2 ; fG(1) = -1/6 ; ffG(1) = -7/18
checkEquals(df$values[[1]][[1]], c(x_init, x_init))

## 13. linfunc containing more than one (a,b) couple bis
# ax+b is (1/3)x-(1/3) or (1/4)x-(1/4)
# cx+d is 2x+0 or 3x+0
f_test = new("Linear function", 
             "a", "b", 
             c(as.bigq(1L, 3L), as.bigq(1L, 4L)),
             c(-as.bigq(1L, 3L), -as.bigq(1L, 4L)))
g_test = new("Linear function", 
             "c", "d",
             c(as.bigq(2L), as.bigq(3L)), 
             c(as.bigq(0L), as.bigq(0L)))
voc = c("f", "g")
words_init = c("ffg", "ggf")
linfunc = c(f_test, g_test)
x_init = c(-as.bigq(1L, 1L), as.bigq(0L, 1L), as.bigq(1L, 1L))
wt = new("Word tree", voc = voc, words_init = words_init,
         x_init = x_init, linfunc = linfunc)
df = wt@df

# common to all
checkEquals(wt@voc, voc)
checkEquals(wt@level, nchar(words_init[1]))
checkEquals(dim(wt@coord), c(0,0))
checkEquals(wt@x_init, x_init)
checkEquals(wt@linfunc, linfunc)
checkEquals(nrow(df), length(words_init))
checkEquals(df$word, words_init)

# specific
checkEquals(df$linfun[[1]], sapply(words_init, linfun_init, voc, linfunc)[[1]])
checkEquals(dim(df$values[[1]][[1]]), c(3,2)) # x in rows, (a,b,c,d) in col
# ax+b = (1/3)x-(1/3) & cx+d = 2x+0
# ffg:
# g(-1)=-2 fg(-1)=-1 ffg(-1)=-2/3
# g(0)=0 fg(0)=-1/3 ffg(0)=-4/9
# g(1)=2 fg(1)=1/3 ffg(1)=-2/9
# ggf:
# f(-1)=-2/3 gf(-1)=-4/3 ggf(-1)=-8/3
# f(0)=-1/3 gf(0)=-2/3 ggf(0)=-4/3
# f(1)=0 gf(1)=0 ggf(1)=0
#
# ax+b = (1/4)x-(1/4) & cx+d = 3x+0
# ffg:
# g(-1)=-3 fg(-1)=-1 ffg(-1)=-1/2
# g(0)=0 fg(0)=-1/4 ffg(0)=-5/16
# g(1)=3 fg(1)=1/2 ffg(1)=-1/8
#
# ggf:
# f(-1)=-1/2 gf(-1)=-3/2 ggf(-1)=-9/2
# f(0)=-1/4 gf(0)=-3/4 ggf(0)=-9/4
# f(1)=0 gf(1)=0 ggf(1)=0
#
# So for ffg:
# -2/3 -1/2
# -4/9 -5/16
# -2/9 -1/8
#
# and for ggf:
# -8/3 -9/2
# -4/3 -9/4
#    0    0
checkEquals(df$values[[1]][[1]],
            matrix(-c(c(6,4,2) / as.bigq(9L), 
                      c(8,5,2) / as.bigq(16L)), ncol = 2))
checkEquals(df$values[[2]][[1]],
            matrix(-c(c(8,4,0) / as.bigq(3L), 
                      c(18,9,0) / as.bigq(4L)), ncol = 2))
rm(f_test, g_test, voc, words_init, linfunc, x_init, wt, df)

## 14. One example with x_integer_only
voc = c("f", "g")
words_init = c("ggf", "gfg", "fgg")
linfunc = c(f_base, g_base)
x_init = as.bigq(1L, 1L)
wt = new("Word tree", voc = voc,
         words_init = words_init, linfunc = linfunc,
         x_init = x_init, x_integer_only = TRUE)
df = wt@df

# only "ggf", "fgg" are integer, and fgf(1) = -1/3
checkEquals(df$word, c("ggf", "fgg"))
checkEquals(df$values[[1]][[1]], 0) # ggf(1) = 0
checkEquals(df$values[[2]][[1]], 1) # fgg(1) = 1
rm(voc, words_init, linfunc, x_init, wt, df)

## 15. One example with only one word_init
voc = c("f", "g")
words_init = c("")
linfunc = c(f_base, g_base)
x_init = as.bigq(1L, 1L)
wt = new("Word tree", voc = voc,
         words_init = words_init, linfunc = linfunc,
         x_init = x_init, x_integer_only = TRUE)
df = wt@df

checkEquals(nrow(df), 1)
checkEquals(df$values[[1]][[1]], 1)
rm(voc, words_init, linfunc, x_init, wt, df)

########################
# add_letters function #
########################
# From a vector of words, add each letter of the vocabulary `voc` on the left.
add_letters = function(words, voc = c("f", "g")) {
  out = as.vector(sapply(words, function(x){c(paste0(voc, x))}))
  return(out)
}

##
# Examples
##
add_letters("f") # add letters of the voc to the left: ff, gf
add_letters(add_letters("")) # ff, gf, fg, gg

##
# Unit tests
##
checkEquals(add_letters(""), c("f", "g"))
checkEquals(add_letters("f"), c("ff", "gf"))
checkEquals(add_letters(c("ff", "ggfff")), c("fff", "gff", "fggfff", "gggfff"))
checkEquals(add_letters(c("", "bb"), c("c", "d")), c("c", "d", "cbb", "dbb"))

########################
# add_linfunc function #
########################
# From a list of linear functions, add each base linear function `linfunc_base`
# on the left.
add_linfunc = function(linfuncs, linfunc_base = list(f_base, g_base)) {
  out = sapply(linfuncs, function(x){
    sapply(linfunc_base, function(y) {y %o% x})
  })
  return(unlist(out))
}

##
# Examples
##
add_linfunc(list(f_base)) # compose to the left to get: ff, gf
add_linfunc(add_linfunc(list(id_n(1)))) # ff, gf, fg, gg

##
# Unit tests
##
## Tests with f_base and g_base
checkEquals(add_linfunc(list(id_n(1))), list(f_base, g_base))
checkEquals(add_linfunc(list(f_base)), 
            list(f_base %o% f_base, g_base %o% f_base))
checkEquals(add_linfunc(list(f_base %o% f_base,
                             g_base %o% g_base %o% f_base %o% f_base)), 
            list(f_base %o% f_base %o% f_base,
                 g_base %o% f_base %o% f_base,
                 f_base %o% g_base %o% g_base %o% f_base %o% f_base,
                 g_base %o% g_base %o% g_base %o% f_base %o% f_base))

## Test with other linear functions
# (1/3)x + 2 ; x + 2/3 ; 5x - 1/4
seq_coeff_x = c(as.bigq(1L, 3L), as.bigq(1L, 1L), as.bigq(5L, 1L))
seq_coeff_1 = c(as.bigq(2L, 1L), as.bigq(2L, 3L), -as.bigq(1L, 4L))
lin1 = new("Linear function", "c", "d", seq_coeff_x, seq_coeff_1)

# 3x - 6 ; x - 2/3 ; x/5 + 1/20
seq_coeff_x_inv = c(as.bigq(3L, 1L), as.bigq(1L, 1L), as.bigq(1L, 5L))
seq_coeff_1_inv = c(-as.bigq(6L, 1L), -as.bigq(2L, 3L), as.bigq(1L, 20L))
lin2 = new("Linear function", "C", "D", seq_coeff_x_inv, seq_coeff_1_inv)

# (1/2)x - 1/5 ; 2x - 7/4 ; 7x + 1/2
seq_coeff_x_bis = c(as.bigq(1L, 2L), as.bigq(2L, 1L), as.bigq(7L, 1L))
seq_coeff_1_bis = c(-as.bigq(1L, 5L), -as.bigq(7L, 4L), as.bigq(1L, 2L))
lin3 = new("Linear function", "p", "q", seq_coeff_x_bis, seq_coeff_1_bis)

iter1 = add_linfunc(list(id_n(3)), list(lin1, lin2))
iter2 = add_linfunc(iter1, list(lin1, lin2))
iter3 = add_linfunc(iter1, list(lin1, lin2, lin3))

checkEquals(iter2, 
            list(lin1 %o% lin1, lin2 %o% lin1, lin1 %o% lin2, lin2 %o% lin2))

checkEquals(iter3, 
            list(lin1 %o% lin1, lin2 %o% lin1, lin3 %o% lin1, 
                 lin1 %o% lin2, lin2 %o% lin2, lin3 %o% lin2))

rm(seq_coeff_x, seq_coeff_1, lin1,
   seq_coeff_x_inv, seq_coeff_1_inv, lin2,
   seq_coeff_x_bis, seq_coeff_1_bis, lin3,
   iter1, iter2, iter3)

#######################
# add_values function #
#######################
# From a list of matrix values, add each base linear function `linfunc_base`
# on the left.
# Function to push in the object
add_values = function(values, linfunc_base = list(f_base, g_base)) {
  if(typeof(values[[1]]) != "list") {
    stop("'values' needs a list of list of bigq matrix! ",
         "Inner list is necessary to prevent a bug between gmp and data.table",
         ". Outer list is the different matrices where to apply linfunc_base.")
  }
  
  if(ncol(values[[1]][[1]]) != length(linfunc_base[[1]]@seq_coeff_x)) {
    stop("Each column of each matrix of values represents a choice for ",
         "linear coefficients. Those coeffs are those contained in ",
         "linfunc_base, so they must coincide.")
    # Ex: With f_base, it contains 1 choice for (a,b) = (1/3, -1/3)
    #     (and g_base also: (c,d) = (2, 0)).
    #     So the matrix 'values' must have only 1 column, corresponding to
    #     (a,b,c,d).
  }

  out = lapply(values, function(x){
    sapply(linfunc_base, function(y) {
      temp = push_matrix_values(x[[1]], y@seq_coeff_x, y@seq_coeff_1)
      # To prevent convert back from list to raw when there is only
      # one bigq element
      if(length(temp) == 1) {
        temp = list(temp)
      }
      return(temp)
    })
  })
  out = unlist(out, recursive=FALSE)
  # enclose with a list to prevent problem between gmp and data.table
  out = lapply(out, list)
  return(out)
}

##
# Examples
##
## Basic example
x_init = c(as.bigq(1L, 1L), as.bigq(2L, 1L), as.bigq(3L, 1L), as.bigq(4L, 1L))
matrix_values_f = compute_matrix_values(x_init, 
                                        f_base@seq_coeff_x, 
                                        f_base@seq_coeff_1)
matrix_values_g = compute_matrix_values(x_init, 
                                        g_base@seq_coeff_x, 
                                        g_base@seq_coeff_1)

# add_values(matrix_values_f) # error because shoud be a list of list
# add_values(list(matrix_values_f)) # error because should be a list of list

# Output values for: ff and gf:
# From t(0, 1/3, 2/3, 1), apply (x-1)/3 and 2x:
add_values(list(list(matrix_values_f)))

# Output values for: ff, gf, fg, gg
add_values(list(list(matrix_values_f), list(matrix_values_g)))
rm(x_init, matrix_values_f, matrix_values_g)

## Example with 2 coeffs (a,b) and (c,d)
slopes_f = c(as.bigq(1L, 3L), as.bigq(1L, 4L))
intercepts_f = c(-as.bigq(1L, 3L), -as.bigq(1L, 4L))
f_test = new("Linear function", "a", "b", slopes_f, intercepts_f)

slopes_g = c(as.bigq(2L, 1L), as.bigq(3L, 1L))
intercepts_g = c(as.bigq(0L, 1L), as.bigq(0L, 1L))
g_test = new("Linear function", "c", "d", slopes_g, intercepts_g)

x_init = c(as.bigq(1L, 1L), as.bigq(2L, 1L), as.bigq(3L, 1L), as.bigq(4L, 1L))
matrix_values_f = compute_matrix_values(x_init, slopes_f, intercepts_f)

add_values(list(list(matrix_values_f)), linfunc_base = list(f_test, g_test))
# From:
# 0    0   
# 1/3  1/4 
# 2/3  1/2 
# 1    3/4
# we apply f and g:
# With f:          and with g:
# -1/3   -1/4              0     0
# -2/9  -3/16              2/3   3/4
# -1/9   -1/8              4/3   3/2
# 0     -1/16              2     9/4

rm(slopes_f, intercepts_f, f_test, slopes_g, intercepts_g, g_test,
   x_init, matrix_values_f)

##
# Unit tests
##
## 1 x_init, 1 set of coeffs
M0 = matrix(as.bigq(1))
M0bis = matrix(as.bigq(-1))
M0ter = matrix(as.bigq(2))
# Apply f: (1-1)/3
checkEquals(add_values(list(list(M0)))[[1]][[1]], 0)
# Apply g: 2*1
checkEquals(add_values(list(list(M0)))[[2]][[1]], 2)
# Add values with 3 elements in the list
add_values_M0 = add_values(list(list(M0), list(M0bis), list(M0ter)))
checkEquals(add_values_M0[[1]][[1]], (1-1)/3)
checkEquals(add_values_M0[[2]][[1]], 2*1)
checkEquals(add_values_M0[[3]][[1]], (-1-1)/3)
checkEquals(add_values_M0[[4]][[1]], 2*(-1))
checkEquals(add_values_M0[[5]][[1]], (2-1)/3)
checkEquals(add_values_M0[[6]][[1]], 2*2)

## 6 x_init, 1 set of coeffs
M1 = matrix(c(as.bigq(-3:2)), nrow = 6, ncol = 1)
checkEquals(add_values(list(list(M1)))[[1]][[1]], as.bigq(-4:1)/3)
checkEquals(add_values(list(list(M1)))[[2]][[1]], as.bigq(-3:2)*2)

## 2 x_init, 2 sets of coeffs
# matrix choices
M2 = matrix(c(as.bigq(1:4)), ncol = 2)
M2bis = matrix(c(as.bigq(rep(0,4))), ncol = 2)
M2ter = matrix(c(as.bigq(c(-1,1,1,-1))), ncol = 2)
# linfunc_base choice
slopes_f = c(as.bigq(1L, 3L), as.bigq(1L, 4L))
intercepts_f = c(-as.bigq(1L, 3L), -as.bigq(1L, 4L))
f_test = new("Linear function", "a", "b", slopes_f, intercepts_f)
slopes_g = c(as.bigq(2L, 1L), as.bigq(3L, 1L))
intercepts_g = c(as.bigq(0L, 1L), as.bigq(0L, 1L))
g_test = new("Linear function", "c", "d", slopes_g, intercepts_g)
# 
add_values_M2 = add_values(list(list(M2), list(M2bis), list(M2ter)),
                           linfunc_base = list(f_test, g_test))
checkEquals(add_values_M2[[1]][[1]],
            matrix(c(as.bigq(0), as.bigq(1L, 3L),
                     as.bigq(1L, 2L), as.bigq(3L, 4L)), ncol = 2))
checkEquals(add_values_M2[[2]][[1]],
            matrix(c(as.bigq(2), as.bigq(4L, 1L),
                     as.bigq(9L, 1L), as.bigq(12L, 1L)), ncol = 2))
checkEquals(add_values_M2[[3]][[1]],
            matrix(c(-as.bigq(1L, 3L), -as.bigq(1L, 3L),
                     -as.bigq(1L, 4L), -as.bigq(1L, 4L)), ncol = 2))
checkEquals(add_values_M2[[4]][[1]],
            matrix(c(as.bigq(rep(0,4))), ncol = 2))
checkEquals(add_values_M2[[5]][[1]],
            matrix(c(-as.bigq(2L, 3L), as.bigq(0),
                     as.bigq(0), -as.bigq(1L, 2L)), ncol = 2))
checkEquals(add_values_M2[[6]][[1]],
            matrix(as.bigq(c(-2, 2, 3, -3)), ncol = 2))

## 1 x_init, 3 sets of coeffs
# matrix choice
M3 = matrix(c(as.bigq(-3:-1)), nrow = 1, ncol = 3)
# linfunc_base choice
slopes_f_ter = c(as.bigq(1L, 3L), as.bigq(1L, 4L), as.bigq(1L, 5L))
intercepts_f_ter = c(-as.bigq(1L, 3L), -as.bigq(1L, 4L), -as.bigq(1L, 5L))
f_test_ter = new("Linear function", "a", "b", slopes_f_ter, intercepts_f_ter)
slopes_g_ter = c(as.bigq(2L, 1L), as.bigq(3L, 1L), as.bigq(4L, 1L))
intercepts_g_ter = c(as.bigq(0L, 1L), as.bigq(0L, 1L), as.bigq(0L, 1L))
g_test_ter = new("Linear function", "c", "d", slopes_g_ter, intercepts_g_ter)
slopes_h_ter = c(-as.bigq(1L, 1L), as.bigq(1L, 2L), as.bigq(1L, 1L))
intercepts_h_ter = c(as.bigq(1L, 2L), as.bigq(1L, 2L), as.bigq(1L, 2L))
h_test_ter = new("Linear function", "e", "f", slopes_h_ter, intercepts_h_ter)
#
add_values_M3 = add_values(list(list(M3)),
                           linfunc_base = list(f_test_ter, 
                                               g_test_ter, 
                                               h_test_ter))
# apply f:
checkEquals(add_values_M3[[1]][[1]],
            c(-as.bigq(4L, 3L), -as.bigq(3L, 4L), -as.bigq(2L, 5L)))
# apply g:
checkEquals(add_values_M3[[2]][[1]],
            c(-as.bigq(6L, 1L), -as.bigq(6L, 1L), -as.bigq(4L, 1L)))
# apply h:
checkEquals(add_values_M3[[3]][[1]],
            c(as.bigq(7L, 2L), -as.bigq(1L, 2L), -as.bigq(1L, 2L)))

## 4 x_init, 3 sets of coeffs
# matrix choice
M4 = matrix(c(as.bigq(1:4)/3, as.bigq(1:4)/4, as.bigq(3 * 1:4)-1), ncol = 3)
# linfunc_base choice with slopes_f_ter and slopes_g_ter
#
add_values_M4 = add_values(list(list(M4)),
                           linfunc_base = list(f_test_ter, 
                                               g_test_ter))
# apply f:
checkEquals(add_values_M4[[1]][[1]][,1], (M4[,1]-1)/3)
checkEquals(add_values_M4[[1]][[1]][,2], (M4[,2]-1)/4)
checkEquals(add_values_M4[[1]][[1]][,3], (M4[,3]-1)/5)
# apply g:
checkEquals(add_values_M4[[2]][[1]][,1], (M4[,1]*2))
checkEquals(add_values_M4[[2]][[1]][,2], (M4[,2]*3))
checkEquals(add_values_M4[[2]][[1]][,3], (M4[,3]*4))

rm(M0, M0bis, M0ter, add_values_M0, M1, M2, M2bis, M2ter, slopes_f, 
   intercepts_f, f_test, slopes_g, intercepts_g, g_test, add_values_M2,
   M3, slopes_f_ter, intercepts_f_ter, f_test_ter, slopes_g_ter, 
   intercepts_g_ter, g_test_ter, slopes_h_ter, intercepts_h_ter, h_test_ter,
   add_values_M3, M4, add_values_M4)

###############
# push method #
###############
# Take all words of the current level, and push the `voc` to the left:
# On a word tree object, we add each letter of `voc` on the left of each word
# of `words`; and do the related composition by each linear function of 
# `linfunc`.
setGeneric("push", function(object) {
  standardGeneric("push")
})

setMethod("push", "Word tree", function(object) {
  # Get the words of the current level
  level = object@level
  df = object@df
  words = df$word[df$level == level]
  linfuncs = df$linfun[df$level == level]
  out = add_letters(words, object@voc)
  level_out = rep(level + 1, length(out))
  linfunc_out = add_linfunc(linfuncs, object@linfunc)
  if(length(object@x_init) != 0) {
    values = df$values[df$level == level]
    if(length(values) == 0) {
      stop("Cannot push, there are no word in the dataframe at this level. ",
           "It may occur when x_integer_only = TRUE and all pushed matrices ",
           "contain at least one non integer value.")
    }
    values_out = add_values(values, object@linfunc)
    new_lines = data.table(word = out, 
                           level = level_out,
                           linfun = linfunc_out,
                           values = values_out)
    # Select only matrix of integers in x_integer_only
    if(object@x_integer_only) {
      new_lines = df_to_integer_df(new_lines)
    }
  } else {
    new_lines = data.table(word = out, 
                           level = level_out,
                           linfun = linfunc_out)
  }

  # Update the object
  object@df = rbind(df, new_lines, fill = TRUE)
  # fill = TRUE because there may be other columns in the df,
  # so we fill them with NA.
  object@level = level + 1L
  return(object)
})

##
# Examples
##
wt0 = new("Word tree", voc = c("f", "g"), words_init = "")

# Push once
wt1 = push(wt0)
wt1@df
wt1@df$linfun

# Push again
wt2 = push(wt1)
wt2@df
wt2@df$linfun

rm(wt0, wt1, wt2)

##
# Unit tests
##
## Without x_init
checkEquals(filter(push(new("Word tree"))@df, level == 1)$word,
            c("f", "g"))
checkEquals(filter(push(new("Word tree", words_init = "f"))@df,
                   level == 2)$word, 
            c("ff", "gf"))

df_test = push(new("Word tree", voc = "c", words_init = "ccc", 
                    linfunc = list(f_base)))@df
checkEquals(df_test[df_test$level == 4,]$word, "cccc")
rm(df_test)

## With x_init
voc = c("f", "g")
words_init = c("")
linfunc = c(f_base, g_base)
x_init = c(as.bigq(1L, 1L), as.bigq(1L, 3L))
wt = new("Word tree", voc = voc,
         words_init = words_init, linfunc = linfunc,
         x_init = x_init)
# Push
wt_level1 = push(wt)
df_level1 = wt_level1@df
values_f = df_level1[df_level1$word == "f"]$values[[1]][[1]]
values_g = df_level1[df_level1$word == "g"]$values[[1]][[1]]
checkEquals(values_f, c(as.bigq(0L, 1L), -as.bigq(2L, 9L)))
checkEquals(values_g, c(as.bigq(2L, 1L), as.bigq(2L, 3L)))
# Push push
wt_level2 = push(wt_level1)
df_level2 = wt_level2@df
values_ff = df_level2[df_level2$word == "ff"]$values[[1]][[1]]
values_gf = df_level2[df_level2$word == "gf"]$values[[1]][[1]]
values_fg = df_level2[df_level2$word == "fg"]$values[[1]][[1]]
values_gg = df_level2[df_level2$word == "gg"]$values[[1]][[1]]
checkEquals(values_ff, c(-as.bigq(1L, 3L), -as.bigq(11L, 27L)))
checkEquals(values_gf, c(as.bigq(0L, 1L), -as.bigq(4L, 9L)))
checkEquals(values_fg, c(as.bigq(1L, 3L), -as.bigq(1L, 9L)))
checkEquals(values_gg, c(as.bigq(4L, 1L), as.bigq(4L, 3L)))
rm(voc, words_init, linfunc, x_init, wt, 
   wt_level1, df_level1, values_f, values_g,
   wt_level2, df_level2, values_ff, values_gf, values_fg, values_gg)

## With x_init and x_integer_only
voc = c("f", "g")
words_init = c("")
linfunc = c(f_base, g_base)
x_init = as.bigq(1L, 1L) # x_init must be integer(s) with x_integer_only!
wt = new("Word tree", voc = voc,
         words_init = words_init, linfunc = linfunc,
         x_init = x_init, x_integer_only = TRUE)
# Push
wt_level1 = push(wt)
df_level1 = wt_level1@df
values_f = df_level1[df_level1$word == "f"]$values[[1]][[1]]
values_g = df_level1[df_level1$word == "g"]$values[[1]][[1]]
checkEquals(values_f, c(as.bigq(0L, 1L)))
checkEquals(values_g, c(as.bigq(2L, 1L)))
# Push push
wt_level2 = push(wt_level1)
df_level2 = wt_level2@df
values_gf = df_level2[df_level2$word == "gf"]$values[[1]][[1]]
values_gg = df_level2[df_level2$word == "gg"]$values[[1]][[1]]
checkEquals(df_level2$word, c("", "f", "g", "gf", "gg"))
checkEquals(values_gf, c(as.bigq(0L, 1L)))
checkEquals(values_gg, c(as.bigq(4L, 1L)))
rm(voc, words_init, linfunc, x_init, wt, 
   wt_level1, df_level1, values_f, values_g,
   wt_level2, df_level2, values_gf, values_gg)

################
# pushn method #
################
# We repeat push method n times, i.e. we take all words of the current level,
# and push the `voc` to the left n times.
setGeneric("pushn", function(object, n) {
  standardGeneric("pushn")
})

setMethod("pushn", signature("Word tree", "numeric"), function(object, n) {
  if(n <= 0) {
    stop("Need n > 0")
  }
  
  for(k in 1:n) {
    object = push(object)
  }
  
  return(object)
})

##
# Examples
##
# Compute 1 iteration from words_init = c("gff", "fgf", "ffg")
pushn(new("Word tree", words_init = c("gff", "fgf", "ffg")), 1)@df
#    word level
# 1:  gff     3
# 2:  fgf     3
# 3:  ffg     3
# 4: fgff     4
# 5: ggff     4
# 6: ffgf     4
# 7: gfgf     4
# 8: fffg     4
# 9: gffg     4

# Comparison between push and pushn
object = new("Word tree", words_init = c("gff", "fgf", "ffg"))
push(object)
push(push(object))
pushn(object, 2)
rm(object)

# Compute 4 iterations from words_init = c("f", "g")
object = new("Word tree")
push(push(push(push(object))))@df
pushn(object, 4)@df
rm(object)

# Slot "df":
#     word level
#  1:          0
#  2:    f     1
#  3:    g     1
#  4:   ff     2
#  5:   gf     2
#  6:   fg     2
#  7:   gg     2
#  8:  fff     3
#  9:  gff     3
# 10:  fgf     3
# 11:  ggf     3
# 12:  ffg     3
# 13:  gfg     3
# 14:  fgg     3
# 15:  ggg     3
# 16: ffff     4
# 17: gfff     4
# 18: fgff     4
# 19: ggff     4
# 20: ffgf     4
# 21: gfgf     4
# 22: fggf     4
# 23: gggf     4
# 24: fffg     4
# 25: gffg     4
# 26: fgfg     4
# 27: ggfg     4
# 28: ffgg     4
# 29: gfgg     4
# 30: fggg     4
# 31: gggg     4

##
# Unit tests
##
checkEquals(filter(pushn(new("Word tree"), 3)@df, level == 3)$word, 
            c("fff", "gff", "fgf", "ggf", "ffg", "gfg", "fgg", "ggg"))
checkEquals(filter(pushn(new("Word tree", words_init = "f"), 2)@df, 
                   level == 3)$word, 
            c("fff", "gff", "fgf", "ggf"))

df_test = pushn(new("Word tree", voc = "c", words_init = "ccc", 
                    linfunc = list(f_base)), 3)@df
checkEquals(df_test[df_test$level == 4,]$word, "cccc")
# Note: the words_init must be words formed by voc elements.
rm(df_test)

object = new("Word tree")
checkEquals(push(push(push(push(object)))), pushn(object, 4))
rm(object)

######################
# signature function #
######################
# The name signature only refers to the count of each letter of the vocabulary
# for each word.
# For example, fgff is (3, 1), because there are 3 f and 1 g.
signature_func = function(words, voc = c("f", "g")) {
  names(words) = words
  cut_words = sapply(words, function(x){strsplit(x, "")})
  out = t(sapply(cut_words, 
                 function(cut_word) {
                   sapply(voc, function(x){sum(cut_word == x)
                   })}))
  
  names_signature = paste0("nb_", voc)
  colnames(out) = names_signature
  
  return(out)
}

##
# Examples
##
signature_func("fgff") # 3 f and 1 g
signature_func("g") # 0 f and 1 g

##
# Unit tests
##
checkEquals(signature_func("fggg")[c(1,2)], c(1,3))
checkEquals(signature_func("gggfggg")[c(1,2)], c(1,6))
checkEqualsNumeric(signature_func(c("f", "g", "fg"))[,1], c(1,0,1))

########################
# add_signature method #
########################
# Add columns of the dataframe counting each letter for each word.
setGeneric("add_signature", function(object) {
  standardGeneric("add_signature")
})

setMethod("add_signature", "Word tree", function(object) {
  voc = object@voc
  df = object@df
  words = df$word
  
  names_signature = paste0("nb_", voc)
  
  # Remove already existing "nb_..." columns
  if(any(names_signature %in% names(df))) {
    df = select(df, -one_of(names_signature))
  }
  
  to_add = signature_func(words, voc)
  object@df = cbind(df, to_add)
  
  return(object)
  
  # If we need to compute signature only for non-NA values, please check:
  # stackoverflow.com/questions/7235657
  # stackoverflow.com/questions/19379081
})

##
# Examples
##
object = pushn(new("Word tree"), 5)
add_signature(object)
rm(object)

##
# Unit tests
##
object = pushn(new("Word tree"), 5)
df = add_signature(object)@df
checkEqualsNumeric(as.numeric(df[df$word == "fffgg",c("nb_f", "nb_g")]),
                   c(3, 2))
checkEqualsNumeric(as.numeric(df[df$word == "",c("nb_f", "nb_g")]),
                   c(0, 0))
checkEqualsNumeric(as.numeric(df[df$word == "gggf",c("nb_f", "nb_g")]),
                   c(1, 3))
rm(object, df)

################################
# Add coeffs in the data frame #
################################
# Method to add coeffs related to 1 and x, and the fixed point.
setGeneric("add_coeffs", function(object) {
  standardGeneric("add_coeffs")
})

setMethod("add_coeffs", "Word tree", 
          function(object) {
            df = object@df
            list_of_linfun = df$linfun
            
            coeff_x_out = lapply(list_of_linfun, function(x) {x@coeff_x})
            coeff_1_out = lapply(list_of_linfun, function(x) {x@coeff_1})
            seq_coeff_x_out = lapply(list_of_linfun, function(x) {x@seq_coeff_x})
            seq_coeff_1_out = lapply(list_of_linfun, function(x) {x@seq_coeff_1})
            
            coeff_x_out = do.call(c, coeff_x_out)
            coeff_1_out = do.call(c, coeff_1_out)
            
            object@df$coeff_x = coeff_x_out
            object@df$coeff_1 = coeff_1_out
            object@df$seq_coeff_x = seq_coeff_x_out
            object@df$seq_coeff_1 = seq_coeff_1_out
            
            ##
            # Add fixed point b / (1-a)
            ##
            # (mutate does not work directly, because of bigq)
            len = length(seq_coeff_1_out)
            fixed_point_out = vector("list", len)
            for(k in 1:len) {
              b = seq_coeff_1_out[[k]]
              a = seq_coeff_x_out[[k]]
              fixed_point_out[[k]] = rep(as.bigq(NA), length(a))
              idx_ok = which(a != 1) # if it is a, then divide by 0
              if(length(idx_ok) != 0) {
                fixed_point_out[[k]][idx_ok] = b[idx_ok] / (1 - a[idx_ok])
              }
            }
            object@df$fixed_point = fixed_point_out
            
            return(object)
          })

##
# Examples
##
## Classic example
wt = add_coeffs(pushn(new("Word tree"), 5))
do.call(c, wt@df$seq_coeff_1)
do.call(c, wt@df$seq_coeff_x)
do.call(c, wt@df$fixed_point)
as.numeric(do.call(c, wt@df$seq_coeff_1))
as.numeric(do.call(c, wt@df$seq_coeff_x))

## With an other vocabulary
wt = add_coeffs(pushn(new("Word tree", voc = c("f", "g", "F", "G"),
                          linfunc = list(f_base, g_base, F_base, G_base)), 5))
do.call(c, wt@df$seq_coeff_1)
do.call(c, wt@df$seq_coeff_x)
do.call(c, wt@df$fixed_point)
rm(wt)

##
# Unit tests
##
## Classic example
wt = add_coeffs(pushn(new("Word tree"), 3))
words = wt@df$word
seq_1 = do.call(c, wt@df$seq_coeff_1)
seq_x = do.call(c, wt@df$seq_coeff_x)
seq_fixed = do.call(c, wt@df$fixed_point)
# id is 1x+0, no fixed point
checkEquals(seq_x[words == ""], as.bigq(1L, 1L))
checkEquals(seq_1[words == ""], as.bigq(0L, 1L))
checkEquals(seq_fixed[words == ""], NA)
# f is (1/3)x - 1/3, fixed point is -1/2: (1/3)(-1/2) - 1/3 = -1/6 - 2/6 = -1/2
checkEquals(seq_x[words == "f"], as.bigq(1L, 3L))
checkEquals(seq_1[words == "f"], -as.bigq(1L, 3L))
checkEquals(seq_fixed[words == "f"], -as.bigq(1L, 2L))
# g is 2x, fixed point is 0
checkEquals(seq_x[words == "g"], as.bigq(2L, 1L))
checkEquals(seq_1[words == "g"], -as.bigq(0L, 1L))
checkEquals(seq_fixed[words == "g"], as.bigq(0L, 1L))
# ffg is aacx + aad+ab+b = (2/9)x - 4/9, fixed point is -4/7:
# (2/9)(-4/7) - 4/9 = -8/63 - 28/63 = -36/63 = -4/7
checkEquals(seq_x[words == "ffg"], as.bigq(2L, 9L))
checkEquals(seq_1[words == "ffg"], -as.bigq(4L, 9L))
checkEquals(seq_fixed[words == "ffg"], -as.bigq(4L, 7L))
rm(wt, words, seq_1, seq_x, seq_fixed)

## With an other vocabulary
wt = add_coeffs(pushn(new("Word tree", voc = c("f", "g", "F", "G"),
                          linfunc = list(f_base, g_base, F_base, G_base)), 4))
words = wt@df$word
seq_1 = do.call(c, wt@df$seq_coeff_1)
seq_x = do.call(c, wt@df$seq_coeff_x)
seq_fixed = do.call(c, wt@df$fixed_point)
# Check for some identity words
checkEquals(seq_x[words == "ffFF"], as.bigq(1L, 1L))
checkEquals(seq_1[words == "ffFF"], as.bigq(0L, 1L))
checkEquals(seq_fixed[words == "ffFF"], NA)
checkEquals(seq_x[words == "GgfF"], as.bigq(1L, 1L))
checkEquals(seq_1[words == "GgfF"], as.bigq(0L, 1L))
checkEquals(seq_fixed[words == "GgfF"], NA)
rm(wt, words, seq_1, seq_x, seq_fixed)
