library(assertthat)

check_values <- function(problem){
  if (problem == 1){
    set.seed(1)
    assert_that(sample_size == 2000)
    assert_that(all_equal(unifsample_df, data.frame(unifsample = runif(sample_size))))
  } else if (problem == 2){
    assert_that(num_samples == 10000)
    assert_that(sample_size == 2000)
    sm <- data.frame(mean=rep(0, num_samples))
    set.seed(1)
    for (i in 1:num_samples){
      sm$mean[i] = mean(runif(sample_size))
    }
    assert_that(all_equal(sm, sample_means))
  } else if (problem == 3){
    assert_that(all_equal(fire_df,
                          suppressWarnings(read_csv('https://raw.githubusercontent.com/psych10/psych10/master/problem_sets/data/wildfires/CA_fire_size_by_year.csv', col_types = cols()))))
    
  } else if (problem == 4){
    assert_that(all(years_to_keep == c(1992:1995, 2012:2015)))
    assert_that(all_equal(fire_df_filtered, filter(fire_df, year %in% years_to_keep)))
    assert_that(all_equal(fire_df_decade, mutate(fire_df_filtered, decade = as.factor(if_else(year < 2000, 1990, 2010)))))
    
  } else if (problem == 5){
    assert_that(all_equal(area_burned_by_decade, summarize(group_by(fire_df_decade, decade), 
                                                   total_area = sum(size_acres))))
    
  } else if (problem == 6){
    assert_that(observed_diff == diff(area_burned_by_decade$total_area))

  }else if (problem == 7){
    assert_that(num_resamples == 1000)
    assert_that(all_equal(resample_results, data.frame(area_diff=array(dim=num_resamples))))
  }else if (problem == 8){
    
    fire_df_copy <- fire_df_decade 
    rr = data.frame(area_diff=array(dim=num_resamples))
    set.seed(1) 
    for (resample_run in 1:num_resamples){
      # compute the amount burned in each decade
      area_burned_by_decade <- summarize(group_by(fire_df_copy, decade), 
                                         total_area = sum(size_acres))
      # compute the difference in area between decades and store to our results data frame
      rr$area_diff[resample_run] <- diff(area_burned_by_decade$total_area)
      # now shuffle the decade labels
      fire_df_copy$decade <- sample(fire_df_copy$decade)
    }

    assert_that(all_equal(rr, resample_results))
  }else if (problem == 9){
    assert_that(pvalue == mean(resample_results$area_diff >= observed_diff))
    
  }
  else if (problem == 10){
    assert_that(number_of_sims == 10000)
    assert_that(group_size == 50)  
    set.seed(1) # PROVIDED CODE - DO NOT CHANGE!
    
    sr <- data.frame(p=array(dim=number_of_sims))
    
    for (i in 1:number_of_sims){
      rand_df = data.frame(group = group_labels, response = rnorm(group_size*2) )
      ttest_result <- t.test(response ~ group, data=rand_df)
      sr$p[i] <- ttest_result$p.value
    }
    assert_that(all_equal(sr, sim_results_df))
  }
  else if (problem == 11){
    assert_that(sim_pvalue == mean(sim_results_df$p < .05))
  }
  print('good job!')
}