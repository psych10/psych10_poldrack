library(assertthat)

check_values <- function(problem){
  if (problem == 1){
    set.seed(1)
    assert_that(sample_size == 1000)
    assert_that(all_equal(my_sample, data.frame(x = runif(sample_size))))
  } else if (problem == 2){
    assert_that(num_samples == 10000)
    assert_that(sample_size == 1000)
    sm <- data.frame(mean=rep(0, num_samples))
    set.seed(1)
    for (i in 1:num_samples){
      sm$mean[i] = mean(runif(sample_size))
    }
    assert_that(all_equal(sm, sample_means))
  } else if (problem == 3){
    assert_that(all_equal(fire_df,
                          suppressWarnings(read_csv('https://raw.githubusercontent.com/psych10/psych10/master/problem_sets/2020/data/wildfires/CA_fire_size_by_year.csv', col_types = cols()))))
    
  } else if (problem == 4){
    assert_that(all(years_to_keep == c(1992:1995, 2012:2015)))
    assert_that(all_equal(fire_df_filtered, filter(fire_df, year %in% years_to_keep)))
    assert_that(all_equal(fire_df_decade, mutate(fire_df_filtered, decade = as.factor(if_else(year < 2000, 1990, 2010)))))
    
  } else if (problem == 5){
    assert_that(all_equal(count_all_fires, count(group_by(fire_df_decade, decade))))
    
  } else if (problem == 6){
    assert_that(all_equal(large_fires, filter(fire_df_decade, size_acres >= 5000)))
    assert_that(all_equal(large_fire_count, count(group_by(large_fires, decade))))
    
  } else if (problem == 7){
    assert_that(observed_diff == diff(large_fire_count$n))
    
  }else if (problem == 8){
    assert_that(num_resamples == 1000)
    assert_that(all_equal(resample_results, data.frame(fire_diff=array(dim=num_resamples))))
  }else if (problem == 9){
    
    fire_df_copy <- fire_df_decade 
    rr = data.frame(fire_diff=array(dim=num_resamples))
    set.seed(1) 
    for (resample_run in 1:num_resamples){
      # filter to include the large fires
      large_fires_sim <- filter(fire_df_copy, size_acres >= 5000)
      # count the large fires by decade
      large_fire_count <- count(group_by(large_fires_sim, decade))
      # compute the difference in number of large fires between decades and store to our results data frame
      rr$fire_diff[resample_run] <- diff(large_fire_count$n)
      # now shuffle the decade labels
      fire_df_copy$decade <- sample(fire_df_copy$decade)
    }
    
    assert_that(all_equal(rr, resample_results))
  }else if (problem == 10){
    assert_that(pvalue == mean(resample_results$fire_diff >= observed_diff))
    
  }
  else if (problem == 11){
    assert_that(num_sims == 5000)
    assert_that(group_size == 50)  
    set.seed(1) # PROVIDED CODE - DO NOT CHANGE!
    
    sr <- data.frame(p=array(dim=num_sims))
    
    for (i in 1:num_sims){
      rand_df = data.frame(group1 = rnorm(group_size), group2 = rnorm(group_size) )
      ttest_result <- t.test(rand_df$group1, rand_df$group2)
      sr$p[i] <- ttest_result$p.value
    }
    assert_that(all_equal(sr, sim_results))
  }
  else if (problem == 12){
    assert_that(sim_pvalue == mean(sim_results$p < .05))
  }
  print('good job!')
}