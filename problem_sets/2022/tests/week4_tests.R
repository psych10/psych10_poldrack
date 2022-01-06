library(assertthat)

check_values <- function(problem){
  if (problem == 1){
    assert_that(all(my_data == c(18, 25, 64, 43, 38)))
  } else if (problem == 2){
    assert_that(mean_by_hand == mean(my_data))
  } else if (problem == 3){
    assert_that(all(squared_errors == (my_data - mean(my_data))**2))
    assert_that(variance == var(my_data))
    assert_that(std_deviation == sd(my_data))
    
  } else if (problem == 4){
    assert_that(all(z_scores == (my_data - mean(my_data))/sd(my_data)))
    
  } else if (problem == 5){
    assert_that(all(unique_songs == unique(rock_data$SongName)))
    assert_that(number_of_songs == length(unique_songs))
    
  } else if (problem == 6){
    assert_that(all_equal(song_counts, count(rock_data, SongName)))
    
  } else if (problem == 7){
    assert_that(all_equal(day_counts, count(rock_data, DayOfWeek)))
    assert_that(all_equal(day_probability, mutate(day_counts, p = n / sum(n))))
    
  }else if (problem == 8){
    assert_that(all_equal(wftw_data, filter(rock_data, SongName == "Working for the Weekend")))
    assert_that(all_equal(day_counts, count(wftw_data, DayOfWeek)))
    assert_that(all_equal(day_probability, mutate(day_counts, p = n / sum(n))))
    
  }else if (problem == 9){
    assert_that(p_friday_given_wftw == mean(wftw_data$DayOfWeek == "Friday"))
    
  }else if (problem == 10){
    assert_that(p_wftw == mean(rock_data$SongName == "Working for the Weekend")) 
    assert_that(p_friday == mean(rock_data$DayOfWeek == "Friday"))
    
  }
  else if (problem == 6){
    assert_that(p_wftw_given_friday == (p_friday_given_wftw * p_wftw) / p_friday)
    
  }
  print('good job!')
}
