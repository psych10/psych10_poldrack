library(assertthat)

check_values <- function(problem){
  NHANES_a <- drop_na(filter(NHANES, Age >= 30, Age <= 40), AlcoholYear, HealthGen)
  
  # create the drinks_regularly and good_health variables
  NHANES_a <- mutate(NHANES_a,
                     drinks_regularly = AlcoholYear > 30,
                     good_health = HealthGen %in% c('Excellent', 'Vgood', 'Good'))
  cr <- chisq.test(NHANES_a$drinks_regularly, NHANES_a$good_health)
  o_goodhealth_drinks <- cr$observed[2, 2] / cr$observed[2, 1]
  o_goodhealth_nodrinks <- cr$observed[1, 2] / cr$observed[1, 1]
  o_ratio <- o_goodhealth_drinks / o_goodhealth_nodrinks
  
  NHANES_b <- NHANES %>% filter(Age > 17, Age < 60) %>% drop_na(TotChol)
  cor_a_c <- cor(NHANES_b$Age, NHANES_b$TotChol)
  pwr_res <- pwr.r.test(r=cor_a_c, power=.8, sig.level=.05)
  samp_size <- round(pwr_res$n)
  n_samps = 1000
  result_df_test <- data.frame(pvalue=rep(NA, n_samps),
             corr = rep(NA, n_samps)) 

  if (problem == 1){
    assert_that(all_equal(NHANES_adult, NHANES_a))

  } else if (problem == 2){
    assert_that(cr$statistic == chisq_result$statistic)
    
  }else if (problem == 3){
    assert_that(all_equal(std_residuals, chisq_result$residuals))
    
  } else if (problem == 4){
    assert_that(o_goodhealth_drinks == odds_goodhealth_drinks)
    assert_that(o_goodhealth_nodrinks == odds_goodhealth_nodrinks)
    assert_that(odds_ratio == o_ratio)

  } else if (problem == 5){
    assert_that(cor_a_c == cor_age_chol)
    assert_that(power_result$n == pwr_res$n)
    
  } else if (problem == 6){
    assert_that(n_samples == n_samps)
    
    assert_that(sample_size == samp_size)
    
    assert_that(all_equal(result_df, result_df_test))
    
    
  } else if (problem == 7){
    set.seed(1234)  # DO NOT CHANGE THIS - it ensures the same result each time
    
    # Now loop through for all of the n_samples runs
    for (i in 1:n_samps){
      
      # take a sample from the dataset, using the sample size determined via power analysis
      sample_df <- sample_n(NHANES_adult, samp_size)
      
      # perform a correlation test to compare wing consumption between the groups
      corr_result <- cor.test(sample_df$Age, sample_df$TotChol)
      
      # store the pvalue from the correlation test into our data frame
      result_df_test$pvalue[i] <- corr_result$p.value
      
      # the correlation estimate is stored in the estimate variable within
      # the output from the cor.test function.  extract that value and save 
      # it to our data frame for this simulation
      result_df_test$corr[i] <- corr_result$estimate
      
    }
    assert_that(all_equal(result_df, result_df_test))
    
  } else if (problem == 8){
    assert_that(power == mean(result_df$pvalue < .05))
    
    
  } else if (problem == 9){
    assert_that(mean_corr == mean(result_df$corr))
  } else if (problem == 10){
    corr_summary_test <- result_df %>%
      mutate(significant = pvalue < .05) %>%
      group_by(significant) %>%
      summarize(mean_corr = mean(corr))
    
    assert_that(all_equal(corr_summary, corr_summary_test))
  }
  print('good job!')
}