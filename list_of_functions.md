#List of functions

##1_linear_function.R
Define linear function class

This file defines the linear function class mainly used in word tree. A linear function contains the formal slope and intercept (as strings), and the related numeric slopes of intercepts (as vector gmp numbers, since we can consider a whole range of slopes and intercepts together). We can do composition of linear functions with the %o% operator.
This file is not suitable for continuous purpose, and most functions are not useful alone (there are used by word tree class).

- Linear function (class): Define a linear function with slopes and intercepts
- initialize (method)
- f_base (value): The function f for testing code.
- g_base (value): The function g for testing code.
- id_n (function)
- is_defined_as_id (function)
- get_inverse (function): Inverse function of a linear function
- F_base (value): Inverse function F for testing code.
- G_base (value): Inverse function G for testing code.
- printf (method)
- split_coeff (function)
- poly_expansion (function)
- compo_formal (function)
- compo_numeric (function)
- compo (method)
- `%o%` (function): Composition of two linear functions
- word2num (function): Convert word to R^{#word}, e.g. "fffgg" to c(2, 2, 1, 1, 1)
- linfun_init (function): Given f and g linear functions, get the linear function related to a specific word (not useful because we can use word tree class directly with some initialization)

##2_evaluate_x.R"
Evaluate words in some x

To evaluate a word of linear functions in some values x. This is linked with the linear function class, and will be probably used only through the word tree class.

- outer (function)
- rep_mat (function)
- compute_matrix_values (function)
- push_matrix_values (function)
- matrix_values_numeric (function)
- get_pos (function)
- to_integer_mat (function)
- is_NA_mat (function)

##3_word_tree.R"
Define word tree class

Word tree class, giving a dataframe with results on (discrete) words.

- Word tree (class): define a word tree dataframe from voc, words_init, linfunc, x_init and x_integer_only
- df_to_integer_df (function)
- initialize (method)
- add_letters (function)
- add_linfunc (function)
- add_values (function)
- push (method): pushing one more letter
- pushn (method): pushing n more letters
- signature_func (function)
- add_signature (method): Add the (m, n) of words as dataframe columns
- add_coeffs (method): Add slope, intercept, fixed points as dataframe columns


##4_word2coordinates.R
Convert words to vector of coordinates

Convert words to vector of coordinates, i.e. not a single number but a vector. This is not directly related with the linear composition problem, but can be used after for trajectories.

- voc_unique_func (function)
- voc_lower_func (function)
- voc_upper_func (function)
- voc2steps (function)
- walk_word (function): Define the path related to a word. This is useful to get trajectories of a word on the plane as a North-East path, this is done after.
- coord_representation (function)
- coord_representation_n (function)
- compute_coord (method): Get a coordinate in a high dimension, this direction does not seems very good, because it is hard to work in this space, and no relation with the coefficients.

##5_word2index.R
Index for the words

Convert words to index (as a natural number). This is not directly related with the linear composition problem, but can be useful to get an order on all the words.

- word2index (function): get a 'common' index of the words
- add_common_index (method): add this 'common' index to the dataframe of a word tree

##6_random_words.R
Sample random words

Get a random word of a certain size, get random words.

- random_word (function)
- random_words (function)

##7_plot_trajectories.R
To show trajectory of words

Plot the trajectories. Plotting is only for toy purposes.

- trajectory_word (function): get the listing of the trajectory
- trajectory_word_ready_to_plot (function): just a helper function for plotting
- plot_path_word (function): plot the trajectory of a word
- plot_path_multiple_words (function): plot the trajectories of multiple words

##8_plot_trees.R
To show tree of words

Plot the trees of words. Only for toy purposes.

- remove_first_charact (function)
- remove_last_charact (function)
- get_parents (function): get parents of a word or of multiple words. The feature get_parents(w, 0) allows to do not iterate, e.g. "ffg" giving "fg".
- plot_tree (function): lot the tree of a dataframe obtained with get_parents

##9_continuous.R
For continuous words

- get_nb_without (function)
- get_pos_letter (function)
- prob_of (function)
- supp_of (function)
- get_eps (function)
- prob_supp_of (function)
- diff_gmp (function)
- prob_supp_inv_of (function)
- plot_distribution (function)
- iota_vector (function)
- puiss (function)
- bigterms1 (function)
- bigterms2 (function)
- sigma (function)
- bigterms_singular_left (function)
- bigterms_singular_right (function)
- sigma_singular (function)
- alpha (function)
- beta (function)
- x0 (function)
- reordering (function)
- move (function)
