# Exercice 3
# sub-exercice 3-a ================

# Sum values in a column of a data frame.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d, provided as a string
#
# RETURN VALUE:
# if the specified column exists and contains numbers, returns the sum of
# all values in the column; otherwise, returns NULL
sum_column <- function(d, var) {
# Set a default value to return
result <- NULL
x <- d[[var]] # Remember, as we showed in class: there are two ways
# to access a column, and this is one; you should try
# and figure out why the other way won't work here,
# but that's not part of the homework
if (!is.null(x)) { # This tests to see whether the column exists in
                  # d; again, you should try and find a way to test
                  # this out for yourself to verify that it's true,
                  # but that's not part of the homework
                  # YOUR CODE HERE: if x contains numbers, set the variable
                  # result to be the sum of the values in x
  if (is.numeric(x)) {result <- sum(x)}
}
return (result)
}


# sub-exercice 3-b ================

# Sum values in a vector.
#
# ARGUMENTS:
# x: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of all values; otherwise, returns NULL
#
# [YOUR FUNCTION HERE]
# declare a new function called my_sum with one argument x
my_sum <- function(x) {
  # check if there is numbers in vector x
  if (is.numeric(x)){
    # if there is, create a variable called result and assign it with a numeric value of 0
    result <- 0
    # iterate through each element in the vector x
    for (one_element in x){
      # if a element is a number, add it to the result so that result is updated after each addition; 
      # if it's not, just pass it and do nothing
      if (is.numeric(one_element)) {result <- result + one_element}
    }
    # after iterating each element, return the final result
    return (result)
    }
  # if there is no numbers at all in the vector x, return directly NULL
  else{return (NULL)}
}


# sub-exercice 3-c ================

# This is a function that sums up the element in x divided by k
# ARGUMENTS: - x: a vector - k: a non-zero number, not a vector
# RETURN: a number if x contains numbers and k is numeric; NULL if not
#
# declare a new function called sum_divided_by with arguments x and k
sum_divided_by <- function(x, k){
  # check 4 conditions: if k is ONE non-zero numeric element and if there is at least one number in x
  # here I precise the length of k to be 1 to avoid a vector k with multiple values
  if (is.numeric(x) & is.numeric(k) & k != 0 & length(k) == 1){
    # if it's true, call the function my_sum(x) to get the sum of all the numeric elements
    # then divided by k and return the result
    return (my_sum(x) / k)
  }
  # if k is non-numeric or is equal to 0, or there is no number in x, return to NULL
  else {return (NULL)}
}


# sub-exercice 3-d ================

# This is a function that calculates the mean of a vector x
# ARGUMENTS: - x: a vector
# RETURN: the mean of x if contains numbers; NULL if not
#
# declare a new function called my_mean with argument x
my_mean <- function(x){
  # create a variable called len_x to store the length of vector x
  len_x <- length(x)
  # call the function sum_divided_by and pass x as the first argument, the len_x as the second argument
  # here I don't check the validation of x and k since it will be done in sum_divided_by, 
  # so I just directly return the result
  return (sum_divided_by(x, len_x))
}


# Exercice 4
# sub-exercice 4-a ================

# Return a violin plot.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
#
# RETURN VALUE:
# A ggplot plot object containing a violin plot, grouped by the values
# of the grouping variable.
#
grouped_violin_plot <- function(d, var, grouping_var) {
  
  # Create the base ggplot object
  p <- ggplot2::ggplot(d, ggplot2::aes_string(y=var,
                                              x=grouping_var,
                                              fill=grouping_var))
  # YOUR CODE HERE: Create a violin plot
  # here I add the ggplot2:: in order to call the function in ggplot2, and update p with new one
  p <- p + ggplot2::geom_violin()
  # return the violin plot p
  return(p)
}


# Exercice 5
# sub-exercice 5-a ================

# Difference in the medians between two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
#
# RETURN VALUE:
# The median value of var for the first group, minus the median value of var for the second group.
#
difference_in_medians <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  # YOUR CODE HERE: assign the difference in the medians to the variable 'result'
  # take the colume "var" from the data frame d_1 and d_2, UNLIST them and convert to a single numeric vector
  # then calculate the median of the 2 groups
  result <- median(as.numeric(unlist(d_1[var]))) - median(as.numeric(unlist(d_2[var])))
  return(result)
}


# sub-exercice 5-b ================

# Randomize the order of a column.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the variable to randomize, provided as a string
#
# RETURN VALUE:
# A data frame or tibble exactly the same as d, except with the order of
# var permuted randomly.
#
randomize <- function(d, var) {
  d[[var]] <- sample(d[[var]])# YOUR CODE HERE: generate a shuffled version of d[[var]]
  return(d)
}


# sub-exercice 5-c ================

# Perform a permutation test for two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of the column in d on which the test statistic will be calculated,
# provided as a string
# grouping_var: the name of the column in d which gives the grouping
# group1: the value of grouping_var corresponding to the first group
# group2: the value of grouping_var corresponding to the second group
# statistic: a function yielding a test statistic, which takes as input
# a data frame, the name of a variable on which to calculate the
# test statistic, the name of a grouping variable, the value of
# the grouping variable corresponding to the first group, and
# the value of the grouping variable corresponding to the second
# group
# n_samples: the number of permutation samples to draw (default: 9999)
#
# RETURN VALUE:
#
# A list containing two elements:
# - observed: the value of statistic() in d
# - permuted: a vector containing the values of statistic() under n_samples permutations
#
permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                n_samples=9999) {
observed_statistic <- statistic(d, var, grouping_var, group1, group2)
permutation_statistics <- rep(0, n_samples)
for (i in 1:n_samples) {
  # YOUR CODE HERE: use randomize(...) to create a permutation and then
  # fill in the vector permutation_statistics with the
  # value of statistic(...) for this new permutation
  permutation <- randomize(d, var)
  permutation_statistics[i] <- statistic(permutation, var, grouping_var, group1, group2)
}
result <- list(observed=observed_statistic,
               permuted=permutation_statistics)
return(result)
}

ptest_1 <- permutation_twogroups(iris, "Sepal.Width", "Species", "versicolor",
                                 "virginica", difference_in_medians, n_samples=10)



