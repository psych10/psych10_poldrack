library(shiny)
library(tidyverse)

ui <- fluidPage(
   
   # Application title
   titlePanel("Hypothesis Testing and Error"),
   
   sidebarLayout(
      sidebarPanel(
        p("Hypothesis testing is a tricky concept. In this visualization, we 
          will try to understand hypothesis testing by using the cars data set. 
          The cars data set was recorded in the 1920s and has two variables:
          speed and distance. Speed is the speed of the car initially and
          distance is the distance taken to stop the car. Let's say that the 
          grey histogram."),
        p(""),
        p("We want to test the null hypothesis that the true average distance it
          takes to stop a car is the sample mean. Thus, our alternative 
          hypothesis is that the null hypothesis is not true. Note that by 
          changing the Alternative Hypothesis Mean (effect size) and the Alternative
          Hypothesis Variance (sample size), we change the size of the errors 
          and the size of the acceptance regions."), 
        p(""),
        p("Type I Error is the likelihood of rejecting the null hypothesis even
          though it is true. Type II Error is the likelihood of accepting the
          null hypothesis even though it is false. The Power of a test is 
          the likelihood of correctly rejecting the null hypothesis."),
         sliderInput("mean",
                     "Alternative Hypothesis Mean:",
                     min = 35,
                     max = 100,
                     value = 75,
                     step = 1),
        sliderInput("variance",
                    "Alternative Hypothesis Variance:",
                    min = 150,
                    max = 1000,
                    value = 300,
                    step = 50),
        selectInput("sig",
                    "Significance Level",
                    choices = c("1%" = 0.01,
                                "5%" = 0.05,
                                "10%" = 0.10,
                                "15%" = 0.15)),
        checkboxInput("type1", "Show Type I Error", 
                      value = FALSE),
        checkboxInput("type2", "Show Type II Error", value = FALSE),
        checkboxInput("power", "Show Power (1 - Type II Error)", 
                      value = FALSE),
        checkboxInput("accept", "Show Acceptance Region", 
                      value = FALSE)
         ),
      
      mainPanel(
         plotOutput("popPlot"),
         plotOutput("distPlot"),
         h4("Questions"),
         p("1. True or False: Increasing the sample size will decrease 
          the Power."),
         p("2. What factors decrease Type II Error?"),
         p("3. How does Type II Error change relative to Type I Error?
          In other words, if Type I Error increases, what happens to Type II Error?")
         
      )
   )
)

server <- function(input, output) {
  
  # Obtaining the original set of data for our null hypothesis
   df <- cars %>% 
     filter(dist < 85)
   cars_mean <- mean(df$dist)
   cars_sd <- sd(df$dist)
  
   # Population plot
   output$popPlot <- renderPlot({
     
     line_seq <- seq(-20, 200, by = 0.01)
     norm_curve <- tibble(x = line_seq,
                          y = dnorm(line_seq, mean = cars_mean, 
                                    sd = cars_sd))
     
     df %>%
       ggplot() +
       geom_histogram(mapping = aes(x = dist, y = ..density..),
                      binwidth = 15, alpha = 0.5) +
       geom_line(data = norm_curve,
                 mapping = aes(x = x, y = y),
                 size = 1, color = "indianred2",
                 alpha = 0.7) +
       theme_bw() +
       coord_cartesian(x = c(-10, 150)) +
       labs(x = "", y = "", 
            title = "Sample Stopping Distance Distribution")
   })
   
   # Alternative hypothesis plot
   output$distPlot <- renderPlot({
     alt_mean <- input$mean
     alt_sd <- sqrt(input$variance)
     
     line_seq <- seq(-20, 200, by = 0.01)
     norm_curve <- tibble(x = line_seq,
                          y = dnorm(line_seq, mean = cars_mean, 
                                    sd = cars_sd))
     sig <- input$sig %>% as.numeric()
     thresh <- quantile(df$dist, 1 - sig)
     
     alt_curve <- tibble(x = line_seq,
                          y = dnorm(line_seq, mean = alt_mean, 
                                    sd = alt_sd))
     
     curr_plot <- df %>%
       ggplot() +
       geom_line(data = norm_curve,
                 mapping = aes(x = x, y = y),
                 size = 1, color = "indianred2",
                 alpha = 0.7) +
       geom_line(data = alt_curve,
                 mapping = aes(x = x, y = y),
                 size = 1, color = "steelblue3",
                 alpha = 0.7) +
       theme_bw() +
       coord_cartesian(x = c(-10, 150)) +
       labs(x = "Car Stopping Distance (ft)", y = "Density",
            title = "Null and Alternative Hypotheses")
     
     # Crazy logic to highlight different parts of the hypothesis test
     if (input$accept == TRUE) {
       accept_curve <- norm_curve %>%
         mutate(ymin = 0) %>% 
         filter(x < thresh) 
       
       curr_plot <- curr_plot +
         geom_ribbon(data = accept_curve,
                     mapping = aes(x = x, ymin = ymin, ymax = y),
                     fill = "goldenrod2", alpha = 0.3)
     }
     
     if (input$type1 == TRUE) {
       shade_curve <- norm_curve %>%
         filter(x > thresh) %>%
         mutate(ymin = 0)
       
       curr_plot <- curr_plot +
         geom_ribbon(data = shade_curve,
                     mapping = aes(x = x, ymin = ymin, ymax = y),
                     fill = "indianred2", alpha = 0.3)
     }
     
     if (input$type2 == TRUE) {
       reg_curve <- norm_curve %>%
         mutate(ymin = 0,
                y = dnorm(line_seq, mean = alt_mean,
                          sd = alt_sd)) %>%
         filter(x < thresh)
       
       curr_plot <- curr_plot +
         geom_ribbon(data = reg_curve,
                     mapping = aes(x = x, ymin = ymin, ymax = y),
                     fill = "steelblue3", alpha = 0.3)
     }
     
     if (input$power == TRUE) {
       power_curve <- norm_curve %>%
         mutate(ymin = 0,
                y = dnorm(line_seq, mean = alt_mean,
                          sd = alt_sd)) %>%
         filter(x > thresh)
       
       curr_plot <- curr_plot +
         geom_ribbon(data = power_curve,
                     mapping = aes(x = x, ymin = ymin, ymax = y),
                     fill = "aquamarine3", alpha = 0.3)
     }
     
     # Plot the final plot
     curr_plot    
         
     })
}

# Run the application 
shinyApp(ui = ui, server = server)


