library(assertthat)

check_values <- function(problem){
  if (problem == 1){
    assert_that(chisq_result$statistic
                == chisq.test(maskdata$WearingMask, maskdata$TouchingFace)$statistic)
    
  } else if (problem == 2){
    assert_that(all_equal(std_residuals, chisq_result$residuals))
    
  }else if (problem == 3){
    assert_that(odds_touching_mask == chisq_result$observed[2, 2] / chisq_result$observed[2, 1])
    assert_that(odds_touching_nomask == chisq_result$observed[1, 2] / chisq_result$observed[1, 1])
    assert_that(odds_ratio == odds_touching_nomask / odds_touching_mask)
    
  } else if (problem == 4){
    assert_that(power_result$n == power.t.test(delta=0.5, power=0.8)$n)
  } else if (problem == 5){
    assert_that(n_simulations == 1000)
    
    assert_that(sample_size_per_group == round(power_result$n))
    
    assert_that(mean_wings == 100)
    assert_that(sd_wings == 10)
    
    assert_that(all_equal(result_df, data.frame(pvalue=rep(NA, n_simulations),
                                                cohensd = rep(NA, n_simulations))))
    
  } else if (problem == 6){
    
    result_df_test <- data.frame(pvalue=rep(NA, n_simulations),
                                 cohensd = rep(NA, n_simulations))
    set.seed(1) 
    cohensd_test <- 0.2
    assert_that(cohensd == cohensd_test)
    
    for (i in 1:n_simulations){
      sport_test <- c(rep('football', sample_size_per_group), 
                      rep('basketball', sample_size_per_group))
      
      wings_test <- rnorm(sample_size_per_group*2, mean=mean_wings, sd=sd_wings)
      
      group_difference_test <- cohensd_test * sd(wings_test)
      
      wings_test = wings_test + group_difference_test * (sport_test == 'basketball')
      
      sim_df_test <- data.frame(sport=sport_test, wings=wings_test)
      
      ttest_result_test <- t.test(wings ~ sport, sim_df_test)
      
      result_df_test$pvalue[i] = ttest_result_test$p.value
      
      cohensd_result_test <- cohen.d(wings ~ sport, sim_df_test)
      
      result_df_test$cohensd[i] = cohensd_result_test$estimate
      
    }
    assert_that(all_equal(result_df, result_df_test))
  } else if (problem == 7){
    assert_that(power == mean(result_df$pvalue < .05))
    
  } else if (problem == 8){
    assert_that(mean_cohensd == mean(result_df$cohensd))
    
  } else if (problem == 9){
    cohensd_summary_test <- result_df %>%
      mutate(significant = pvalue < .05) %>%
      group_by(significant) %>%
      summarize(mean_cohensd = mean(cohensd))
    assert_that(all_equal(cohensd_summary, cohensd_summary_test))
  }
  print('good job!')
}