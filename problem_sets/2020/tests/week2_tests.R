library(assertthat)

check_values <- function(problem){
  if (problem == 1){
    assert_that(is.numeric(my_age))
    assert_that(is.character(my_major))
    assert_that(is.logical(i_am_senior))
  }
  print('good job!')
}
