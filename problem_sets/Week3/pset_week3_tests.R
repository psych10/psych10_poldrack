# tests for pset 3

library(assertthat)

flips_tests <- function(){
  cat('Checking your code...\n')
  # check for correct number of flips
  len_test <- assert_that(length(flips) == 20)
  
  # check to make sure that only 'H' and 'T' are used
  # if any errors occur here then you need to fix them
  h_test <- assert_that("H" %in% unique(flips))
  t_test <- assert_that("T" %in% unique(flips))
  uniq_test <- assert_that(length(unique(flips))==2)
  cat('passed all tests successfully!\n')
}

pheads_tests <- function() {
  cat('Checking your code...\n')
  cat(sprintf('p(heads) = %0.3f\n',p_heads))
  isheads_bool_test <- assert_that(is_logical(is_heads))
  isheads_len_test <- assert_that(length(is_heads)==length(flips))
  pheads_range_test <- assert_that(p_heads>=0 & p_heads<=1)
  pheads_val_test <- assert_that(p_heads==sum(flips!='T')/length(is_heads))
  
  cat('passed all tests successfully!\n')
}

binom_test <- function(){
  cat('Checking your code...\n')
  cat(sprintf('p(7/20 heads observed|pheads=0.5) = %0.3f\n',p_observed_given_fair))
  x=0
  for (i in 0:7){
    x <- x + dbinom(i,20,0.5)
  }
  prob_test <- assert_that(x==p_observed_given_fair)
  cat('passed all tests successfully!\n')
  
}

get_warriors_data <- function(){
  gamedata <- read.csv('warriors_data.csv')
  df <- data.frame(outcome = gamedata$X.3,
                   away = gamedata$X.2=='@')
  return(df)
}

