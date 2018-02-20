library(stringr)
library(shiny)
library(tidyverse)
library(MASS)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Linear Regression and Residual Sum of Squares"),
  
  sidebarLayout(
    sidebarPanel(
      p("The goal of linear regression is to find a line of best fit. 
        The line of best fit explains the variation in the data points the best. 
        A common way too optimize for the best linear fit is to minimize the
        residual sum of squares (RSS). The RSS is one of the many measures
        of the amount of variance that is not captured or explained by
        our linear model."),
      sliderInput("slope",
                  "Slope of line:",
                  min = -4,
                  max = 4,
                  value = 0, 
                  step = 0.01),
      sliderInput("intercept",
                  "Y-Intercept",
                  min = -10,
                  max = 10,
                  value = 6, 
                  step = 0.01),
      p(""),
      sliderInput("cor",
                  "Correlation of Points",
                  min = -1,
                  max = 1,
                  value = 0.5, 
                  step = 0.1),
      checkboxInput("answer", "Reveal Line of Best Fit", FALSE),
      checkboxInput("resid", "Show Residual Lines", FALSE),
      h4("Questions"),
      p("1. True or False: Different best fit lines on different data have
        the same sum of residuals. "),
      p("2. What are some assumptions of a linear model?"),
      p("3. What are pros and cons of a linear model?")
      ),
    
    mainPanel(
      plotOutput("distPlot"),
      dataTableOutput("rss_table"),
      textOutput("best_fit")
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # Set seed the process is less random.
  set.seed(78)
  samp_display <- 50
  mean_vals <- c(6, 6)
  
  # Generate the values to be displayed on the graph
  v <- reactiveValues(data = mvrnorm(n = samp_display,
                                     mu = mean_vals,
                                     Sigma = matrix(c(1, 0.5, 0.5, 1),
                                                    ncol = 2)) %>% 
                        as_tibble() %>% 
                        rename(x = V1, y = V2))
  
  observeEvent(input$cor, {
    
    # If we change the correlation in the UI, we want to regenerate the points
    v <- reactiveValues(data = mvrnorm(n = samp_display,
                                       mu = mean_vals,
                                       Sigma = matrix(c(1, input$cor, 
                                                        input$cor, 1),
                                                      ncol = 2)) %>% 
                          as_tibble() %>% 
                          rename(x = V1, y = V2))
    
    # If we add in the residuals, we want to add in the residual bars to the graph
    observeEvent(input$resid, {
      
      output$distPlot <- renderPlot({
        
        fit <- lm(data = v$data, y ~ x)
        
        input_slope <- input$slope %>% as.numeric()
        input_intercept <- input$intercept %>% as.numeric()
        slope_real <- coef(fit)[2]
        int_real <- coef(fit)[1]
        
        # Crazy logic to customize the graph depending if the user wants to see
        # the line of best fit and the residuals
        if (input$answer == TRUE) {
          
          plot_img <- v$data %>% 
            ggplot() +
            geom_point(mapping = aes(x = x, y = y)) +
            geom_abline(slope = input_slope, intercept = input_intercept,
                        size = 1, color = "blue") +
            geom_abline(slope = slope_real, intercept = int_real,
                        size = 1, color = "black") +
            theme_bw() +
            labs(x = "X", y = "Y") +
            coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))
          
          if (input$resid == TRUE) {
            v$data %>% 
              group_by(x) %>% 
              mutate(fitted_values = input$intercept + x * input$slope,
                     top_y = max(y, fitted_values),
                     bottom_y = min(y, fitted_values)) %>%  
              ungroup() %>% 
              ggplot() +
              geom_point(mapping = aes(x = x, y = y)) +
              geom_abline(slope = input_slope, intercept = input_intercept,
                          size = 1, color = "blue") +
              geom_abline(slope = slope_real, intercept = int_real,
                          size = 1, color = "black") +
              geom_segment(mapping = aes(x = x, y = top_y, xend = x, 
                                         yend = bottom_y), 
                           colour = "red") +
              theme_bw() +
              labs(x = "X", y = "Y", colour = NULL) +
              coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))
            
          } else {
            plot_img
          }
        } else {
          plot_img <- v$data %>% 
            ggplot() +
            geom_point(mapping = aes(x = x, y = y)) +
            geom_abline(slope = input_slope, intercept = input_intercept,
                        size = 1, color = "blue") +
            theme_bw() +
            labs(x = "X", y = "Y") +
            coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))
          
          if (input$resid == TRUE) {
            v$data %>% 
              group_by(x) %>% 
              mutate(fitted_values = input$intercept + x * input$slope,
                     top_y = max(y, fitted_values),
                     bottom_y = min(y, fitted_values)) %>%  
              ungroup() %>% 
              ggplot() +
              geom_point(mapping = aes(x = x, y = y)) +
              geom_abline(slope = input_slope, intercept = input_intercept,
                          size = 1, color = "blue") +
              geom_segment(mapping = aes(x = x, y = top_y, xend = x, 
                                         yend = bottom_y), 
                           colour = "red") +
              theme_bw() +
              labs(x = "X", y = "Y", colour = NULL) +
              coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))
            
          } else {
            plot_img
          }
        }
      })
    })  
  })
  
  # Adding in the table of residuals to be displayed at the bottom
  output$rss_table <- renderDataTable(
    df <- v$data %>% 
      mutate(fitted_values = input$intercept + x * input$slope,
             residuals = y - fitted_values,
             residuals_squared = residuals^2) %>% 
      summarise(`Sum of Residuals` = sum(residuals) %>% round(1),
                `RSS` = sum(residuals_squared) %>% round(1))
  )
  
  output$best_fit <- renderText({
    # Fitting the best fit line
    fit <- lm(data = v$data, y ~ x)
    
    # Output text
    if (input$answer == TRUE) {
      str_c("The line of best fit has an intercept of ", 
            coef(fit)[1] %>% round(2),
            " and a slope of ", coef(fit)[2] %>% round(2), ".")
    } else {
      str_c("To find the line of best fit, try visualizing what trend
            the points make on the graph. Do the points increase in y as 
            you increase x? If so, try a positive slope.")
    }
    })
}

shinyApp(ui = ui, server = server)

