library(assertthat)

check_values <- function(problem){
  if (problem == 1){
    assert_that(is.numeric(my_age))
    assert_that(is.character(my_major))
    assert_that(is.logical(i_am_senior))
  } else if (problem == 2){
    assert_that(all(temp_farenheit == c(74, 81, 65, 49)))
    assert_that(all(temp_celsius == (temp_farenheit - 32) / (5/9)))
    assert_that(all(is_freezing == (temp_celsius < 0)))
    assert_that(all(c('temp_farenheit', 'temp_celsius', 'is_freezing') %in% names(temp_df)))
  } else if (problem == 3){
    assert_that(all(c('temp_farenheit', 'temp_celsius', 'is_freezing', 'temp_kelvin') %in% names(temp_df_k)))
    assert_that(all(temp_df_k$temp_kelvin == (temp_celsius +273.15 )))
  } else if (problem == 4){
    assert_that(all_equal(temp_df_k_freezing, filter(temp_df_k, temp_celsius < 0)))
  } else if (problem == 5){
    assert_that(all_equal(survey_data, suppressMessages(read_csv('https://raw.githubusercontent.com/poldrack/learnr_demos/master/data/surveydata.csv'))))
  } else if (problem == 6){
    assert_that(all(survey_data$programmed_before == (survey_data$programming_experience > 1)))
  }
  print('good job!')
}
