library(assertthat)

check_values <- function(problem){
  if (problem == 1){
    assert_that(all(my_data == c(12, 58, 21, 24, 33)))
  } else if (problem == 2){
    assert_that(my_mean_by_hand == mean(my_data))
  } else if (problem == 3){
    assert_that(all(my_squared_errors == (my_data - mean(my_data))**2))
    assert_that(my_variance == var(my_data))
    assert_that(my_std_deviation == sd(my_data))
    
  } else if (problem == 4){
    assert_that(all(my_z_scores == (my_data - mean(my_data))/sd(my_data)))
    
  } else if (problem == 5){
    assert_that(all(unique_songs == unique(rockdata$SongName)))
    assert_that(num_songs == length(unique_songs))
    
  } else if (problem == 6){
    assert_that(all_equal(song_counts, count(rockdata, SongName)))
    
  } else if (problem == 7){
    assert_that(all_equal(day_counts, count(rockdata, DayOfWeek)))
    assert_that(all_equal(day_prob, mutate(day_counts, p = n / sum(n))))
    
  }else if (problem == 8){
    assert_that(all_equal(wftw, filter(rockdata, SongName == "Working for the Weekend")))
    assert_that(all_equal(day_counts, count(wftw, DayOfWeek)))
    assert_that(all_equal(day_prob, mutate(day_counts, p = n / sum(n))))
    
  }else if (problem == 9){
    assert_that(p_friday_given_wftw == mean(wftw$DayOfWeek == "Friday"))
    
  }else if (problem == 10){
    assert_that(p_wftw == mean(rockdata$SongName == "Working for the Weekend")) 
    assert_that(p_friday == mean(rockdata$DayOfWeek == "Friday"))
    
  }
  else if (problem == 6){
    assert_that(p_wftw_given_friday == (p_friday_given_wftw * p_wftw) / p_friday)
    
  }
  print('good job!')
}
