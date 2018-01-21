library(shiny)
library(tidyverse)

ui <- fluidPage(

  titlePanel("Central Limit Theorem"),
  
  sidebarLayout(
    sidebarPanel(
      p("The goal of this visualization is to help students understand the 
        Central Limit Theorem. The Central Limit Theorem states that
        when independent random variables are averaged, the distribution of
        sample averages tends to a normal distribution. We this happen
        even if the the original variables are not normally distributed. This
        allows us to make inferences about the center of a distribution
        with only information from the sampling distribution and without 
        information of the true distribution."),
      
      selectInput("distribution", "Population Distribution:",
                  choices = c("Uniform Distribution, 0 to 4" = "uniform", 
                              "Standard Normal Distribution" = "normal",
                              "Weibull Distribution, Shape 1" = "weibull",
                              "T-Distribution, 1 DF" = "tdist")),
      sliderInput("n",
                  "Sample Size",
                  min = 10,
                  max = 2500,
                  step = 50,
                  value = 100,
                  dragRange = FALSE),
      checkboxInput("pop_mean", "Population Mean Overlay", FALSE),
      checkboxInput("overlay", "Normal Overlay on Sampling Distribution", 
                    FALSE),
      checkboxInput("samp_mean", "Sampling Mean", FALSE),
      
      h4("Questions"),
      p("1. What happens to the sampling distribution mean as we increase
        the sample size?"),
      p("2. Can you infer anything about the variance of the population
        distribution given the distribution of sampling averages?"),
      p("3. True or False: All sampling distributions converge to a 
        standard normal distribution with a mean of 0 and a standard
        deviation of 1.")
      ),
    
    mainPanel(
      plotOutput("mainPlot"),
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  set.seed(1)
  pop_size <- 5000
  # Generating data for the different distributions
  data <- reactive({ tibble(uniform = runif(n = pop_size, min = 0, max = 4),
                            weibull = rweibull(n = pop_size, shape = 1),
                            normal = rnorm(n = pop_size, 0, 1),
                            tdist = rt(n = pop_size, df = 1)) })
  
  output$mainPlot <- renderPlot({
    dist <- input$distribution
    
    df <- tibble(val = c(as.matrix(data()[,dist]))) 
    
    curr_plot <- df %>% 
      ggplot(mapping = aes(x = val)) +
      geom_histogram(mapping = aes(y = ..density..), binwidth = 1) +
      theme_bw() +
      labs(x = "", y = "Density", title = "Original Population Distribution") +
      coord_cartesian(xlim = c(mean(df$val) - 3*sd(df$val), 
                               mean(df$val) + 3*sd(df$val))) 
    
    # Adding the mean line if chosen to do so by the user
    if (input$pop_mean == TRUE) {
      curr_plot +
        geom_vline(mapping = aes(xintercept = mean(df$val)), 
                   color = "red", size = 1)
    } else {
      curr_plot
    }
  })
  
  output$distPlot <- renderPlot({
    dist <- input$distribution
    sampsize <- input$n
    
    samp_means <- array(0, sampsize)
    
    set.seed(12)
    # Sampling from the chosen distribution
    for (i in 1:sampsize) {
      samp_means[i] <- tibble(val = c(as.matrix(data()[,dist]))) %>% 
        sample_n(size = sampsize, replace = FALSE) %>% 
        .$val %>% 
        mean()
    }
    
    samp_plot <- tibble(val = samp_means) %>% 
      ggplot(mapping = aes(x = val)) +
      geom_histogram(aes(y = ..density..)) +
      theme_bw() +
      labs(x = "Distribution of Sample Means", 
           y = "Density", title = "Sampling Distribution") +
      coord_cartesian(xlim = c(mean(samp_means) - 3*sd(samp_means), 
                               mean(samp_means) + 3*sd(samp_means)))
    
    # More logic for the graphic overlays
    if (input$samp_mean == TRUE) {
      if (input$overlay == TRUE) {
        samp_plot +
          geom_vline(xintercept = mean(samp_means), color = "red",
                     size = 1) +
          stat_function(fun = dnorm,
                        color = "blue",
                        args = list(mean = mean(samp_means),
                                    sd = sd(samp_means)),
                        size = 1) 
      } else {
        samp_plot +
          geom_vline(xintercept = mean(samp_means), color = "red",
                     size = 1) 
      }
    } else {
      if (input$overlay == TRUE) {
        samp_plot +
          stat_function(fun = dnorm,
                        color = "blue",
                        args = list(mean = mean(samp_means),
                                    sd = sd(samp_means)),
                        size = 1)
      } else {
        samp_plot
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

