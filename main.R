# Coal: Composition of Linear Functions
rm(list=ls())
setwd("~/Documents/GitHub/coal/")
library(igraph)
library(RUnit)
library(data.table)
library(dplyr)
library(gmp) # for bigq
library(distr) # only for distribution plot
source("helpers/1_linear_function.R") # define linear function class
source("helpers/2_evaluate_x.R") # evaluate words in some x
source("helpers/3_word_tree.R") # define word tree class
source("helpers/4_word2coordinates.R") # convert words to vector of coordinates
source("helpers/5_word2index.R") # index for the words
source("helpers/6_random_words.R") # sample random words
source("helpers/7_plot_trajectories.R") # to show trajectory of words
source("helpers/8_plot_trees.R") # to show tree of words
source("helpers/9_continuous.R") # for continuous words

###################
# Minimal example #
###################
# Define main object with default values
wt = new("Word tree")
# Compute compositions for all words of size 10 or less
wt = pushn(wt, 10)
# Extract slope, intercept and fixed point coefficients
wt = add_coeffs(wt)
# Retrieve data frame of interest (one row = one word)
df = wt@df
# Select columns of interest
df = df[,c("word", "level", "fixed_point")]
df[1:30,]
# Extract fixed points and plot them
values = do.call(c, lapply(df$fixed_point, function(x){x[[1]]}))
fixed = data.table(word = wt@df$word, values = as.numeric(values))
plot(fixed$values)
plot(fixed$values - floor(fixed$values))

####################
# Detailed example #
####################
## Define linear functions
# We define f and g two objects of class Linear function.
# For f, we consider two explicit functions related to the formal f: ax+b
# f_(a1, b1) = (1/3)x-(1/3) and 
# f_(a2, b2) = (1/4)x-(1/4).
f_test = new("Linear function", 
             "a", "b", # formal names for coefficients in f: ax+b
             c(as.bigq(1L, 3L), as.bigq(1L, 4L)), # slopes a
             c(as.bigq(1L, 3L), as.bigq(1L, 4L))) # intercepts b
# For g, we consider the same number of explicit functions for g: cx+d
# g_(c1, d1) = 2x-0 and 
# g_(c2, d2) = 3x-0.
g_test = new("Linear function", 
             "c", "d", # cx+d
             c(as.bigq(2L, 1L), as.bigq(3L, 1L)), 
             c(as.bigq(0L, 1L), as.bigq(0L, 1L)))
# We combine functions f and g
linfunc = c(f_test, g_test)

## Define vocabulary
# Two letters "f" and "g", representing the linear functions f and g.
voc = c("f", "g")

## Define initial words
# Word compositions to compute when the object wt of class Word tree will
# be defined. Default choice is words_init = "" (no word computed)
# Note: All words must have the same length (three here). 
words_init = c("ggf", "gfg", "fgg")

## Define the initial values
# Each word w is evaluated in x_init: w(x_init).
# Here, defining initial values as -1, 0 and 1.
x_init = c(as.bigq(-1L, 1L), as.bigq(1L, 1L), as.bigq(0L, 1L))

## Choose whether we only keep integer values
# If TRUE, we replace non integer values with NA, and delete word if the
# related matrix of values only contain NA values.
x_integer_only = TRUE

## Create the word tree
# This object is the engine, containing the main df of interest in wt@df
wt = new("Word tree", voc = voc, words_init = words_init,
         x_init = x_init, linfunc = linfunc,
         x_integer_only = x_integer_only)
df = wt@df

## The three functions related to the initial words
df$linfun

## The three matrix of values related to the initial words
# Each row is a x_init (either x1, x2, x3)
# each column is a linear function (either (a1,b1,c1,d1) or (a2,b2,c2,d2))
df$values

## Pushing 9 times
# Pushing means to compute g(w) and f(w) for words w already existing
wt = pushn(wt, 9)

## Adding number of f and g for each row, called 'signature' here
wt = add_signature(wt)

## Adding coefficients (in letters and as bigq)
# In details, add: 
# - formal composition coefficients: coeff_x , coeff_1,
# - numeric composition coefficients: seq_coeff_x, seq_coeff_1,
# - numeric value for fixed point: fixed_point
wt = add_coeffs(wt)

## Outputs
# data frame summarizing each composed word
df = wt@df
# fixed points for each word
values = do.call(c, lapply(df$fixed_point, function(x){x[[1]]}))
fixed = data.table(word = wt@df$word, values = as.numeric(values))
plot(fixed$values)
plot(fixed$values - floor(fixed$values))
plot_tree(get_parents(as.vector(fixed$word)))

############
# Examples #
############
# Open each example to understand what is going on.
source("examples/example_1_concatenation.R")
source("examples/example_2_distance_between_words.R")
source("examples/example_3_b_for_each_signature.R")
source("examples/example_4_integer_bottom_to_up.R")
source("examples/example_5_fixed_points_of_continuous_words.R")
