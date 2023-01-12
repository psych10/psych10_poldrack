library(assertthat)

check_values <- function(problem){
  if (problem == 1){
    assert_that(all(my_data == c(15, 23, 61, 52, 48)))
  } else if (problem == 2){
    assert_that(mean_byhand == mean(my_data))
  } else if (problem == 3){
    assert_that(all(squared_error == (my_data - mean(my_data))**2))
    assert_that(variance == var(my_data))
    assert_that(std_deviation == sd(my_data))
    
  } else if (problem == 4){
    assert_that(all(Z_scores == (my_data - mean(my_data))/sd(my_data)))
    
  } else if (problem == 5){
    assert_that(all(all_songs == unique(song_data$SongName)))
    assert_that(n_songs == length(all_songs))
    
  } else if (problem == 6){
    assert_that(all_equal(song_n_plays, count(song_data, SongName)))
    
  } else if (problem == 7){
    assert_that(all_equal(day_n_plays, count(song_data, DayOfWeek)))
    assert_that(all_equal(day_prob, mutate(day_n_plays, p = n / sum(n))))
    
  }else if (problem == 8){
    assert_that(all_equal(WFTW_data, filter(song_data, SongName == "Working for the Weekend")))
    assert_that(all_equal(day_plays, count(WFTW_data, DayOfWeek)))
    assert_that(all_equal(day_prob_wftw, mutate(day_plays, p = n / sum(n))))
    
  }else if (problem == 9){
    assert_that(p_friday_given_wftw == mean(WFTW_data$DayOfWeek == "Friday"))
    
  }else if (problem == 10){
    assert_that(p_wftw == mean(song_data$SongName == "Working for the Weekend")) 
    assert_that(p_friday == mean(song_data$DayOfWeek == "Friday"))
    
  }
  else if (problem == 6){
    assert_that(p_wftw_given_friday == (p_friday_given_wftw * p_wftw) / p_friday)
    
  }
  print('good job!')
}
