# week 5 tests

library(assertthat)

stem_test <- function(){
  cat('Checking your code...\n')
  set.seed(1234)
  c <- round(runif(16)*40)
  assert_that(length(d)==length(c))
  assert_that(all(sort(d)==sort(c)))
  cat('Data were entered successfully!\n')
  
}


