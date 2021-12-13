library(shiny)
library(tidyverse)
library(cowplot)
library(rhandsontable)
library(kin697u)
ui <- navbarPage("KIN697U",
       tabPanel("Critical Speed",
        sidebarLayout(
         sidebarPanel(width = 4,
          h4("Data Input:"),
          h6("Fully editable table"),
          h6("Right click to add more rows"),
          h6("Multiple athletes accepted in long format"),
          rHandsontableOutput("runner_pr_input"),
          br(),
          actionButton("cs_enter", "Calculate")
           ),
          mainPanel(width = 8, 
            tabsetPanel(
             tabPanel("Plot",
              plotOutput("cs_plot")
                     ),
              tabPanel("Race Predictions",
                tableOutput("cs_prediction_table")
               )
              )
             )
            )
           ),
          tabPanel("D' Balance",
            sidebarLayout(
             sidebarPanel(width = 5,
              tabsetPanel(id = "sidebar",
               tabPanel("Runner Data",
                rHandsontableOutput("runner_data_input")),
               tabPanel("Race Data",
                rHandsontableOutput("race_data_input"))
              )
            ),
            mainPanel(width = 7, 
             tabsetPanel(
               
              tabPanel("Plot",
               plotOutput("lm_scatter_plot")
                ),
              tabPanel("Race Predictions",
               tableOutput("prediction_table")
                )
               )
              )
             )
            )
           )

# Define server logic required to draw a histogram
server <- function(input, output) {

  
  output$runner_pr_input <- renderRHandsontable({
    tibble("Athlete" = rep("Athlete 1", 4),
           "Distance (m)" = as.integer(c(1500, 3000, 5000, 10000)) ,
           "Hours" = rep(0, 4),
           "Minutes" = rep(0, 4),
           "Seconds" = rep(0, 4)) %>% 
      rhandsontable() %>% 
      hot_col(col = "Hours", type = "dropdown", source = 0:9) %>% 
      hot_col(col = "Minutes", type = "dropdown", source = 0:59) %>% 
      hot_col(col = "Seconds", type = "dropdown", source = 0:59)
  })
  
  output$runner_data_input <- renderRHandsontable({
     tibble("Athlete" = rep("Athlete 1", 4),
                "Distance (m)" = as.integer(c(1500, 3000, 5000, 10000)) ,
                "Hours" = rep(0, 4),
                "Minutes" = rep(0, 4),
                "Seconds" = rep(0, 4)) %>% 
     rhandsontable() %>% 
     hot_col(col = "Hours", type = "dropdown", source = 0:9) %>% 
     hot_col(col = "Minutes", type = "dropdown", source = 0:59) %>% 
     hot_col(col = "Seconds", type = "dropdown", source = 0:59)
  })
  
  
  output$race_data_input <- renderRHandsontable({
    tibble("Meter Marker" =  as.integer(c(200, 600, 1000)),
           "Lap Speed (m/s)" = rep(NA, 3)) %>% 
      rhandsontable() %>% 
      hot_col(col = "Lap Speed (m/s)", type = "numeric") 
    })
  
  
    cs <- reactiveValues()
    observeEvent(input$cs_enter, {
      print("hi")
      pr_data <- 
        hot_to_r(input$runner_pr_input) %>% 
        rename(athlete = "Athlete",
               meters = "Distance (m)") %>% 
        mutate(seconds = ((Hours*3600) + (Minutes*60) + Seconds))
    
       if(anyNA(pr_data)){
          showNotification("Whoops. Missing values detected. All cells must contain a value.")
          req(!anyNA(pr_data))
       }
      
      if(any(pr_data == "")){
        showNotification("Whoops. Missing values detected. All cells must contain a value.")
        req(!any(pr_data == ""))
      }
        
      cs_data <-
        pr_data %>% 
        dplyr::select(athlete, meters, seconds) %>% 
        group_by(athlete) %>% 
        nest(pr_data = !athlete) %>% 
        mutate(lm_mod = map(pr_data, ~lm(meters ~ seconds, data = .x)),
               d_prime_meters = map_dbl(lm_mod, ~coef(.x)[[1]]),
               critical_speed_meters_second =  map_dbl(lm_mod, ~coef(.x)[[2]]),
               lm_predict = map(lm_mod, 
                                ~as.data.frame(
                                 predict(.x, 
                                         newdata = data.frame(seconds = 0:21600),
                                         interval = "conf")) %>% 
                                  mutate(x = 0:21600)))
          
        cs$pr_data <- pr_data
        cs$cs_data <- cs_data
         
 })
     
    sim_data <- eventReactive(input$sim,{
      tibble(meters = 1:43000,
             seconds = (meters - input$sim_d_prime)/input$sim_cs)
    })
   
     output$lm_scatter_plot <- renderPlot({
       
       if(input$sidebar == "Input Data"){
       validate(need(input$time1, "Enter PR data to begin. Press 'Go' to calculate."))
       
         
         user_data <- user_data()$data
         cs <- round(as.numeric(user_data()$lm_coefs[[2]]), 2)
         d_prime <- round(as.numeric(user_data()$lm_coefs[[1]]), 2)
         lm_predict <- user_data()$lm_predict
         
         minutes_per_mile_decimal <- as.character(round(26.82/cs, 2))
         minute_decimal_split <- str_split(minutes_per_mile_decimal, "[.]", n = 2, simplify = TRUE)
         decimal_to_seconds <- as.numeric(paste0("0.", minute_decimal_split[[2]]))*60
         mile_time <- paste0(minute_decimal_split[[1]], ":", round(decimal_to_seconds, 0))
         
         # user_data <- data.frame(meters = c(1000, 5000),
         #                         seconds = c(151, 1000))
         # cs <- 4.7
         # d_prime <- 280
       
          ggplot()+
             geom_point(data = user_data, aes(seconds, meters), size = 4)+
             geom_ribbon(data = lm_predict, aes(x = x, ymin = lwr, ymax = upr), alpha = 0.3)+
             geom_line(data = lm_predict, aes(x = x, y = fit), linetype = "dashed")+
             annotate("text", 
                      x = 0 , 
                      y = Inf,
                      hjust = 0,
                      vjust = 1,
                      label = paste0("y = ", cs, "x + ", d_prime, "\n",
                                     "CS = ", cs, " m/s", " (", mile_time, " min/mile)\n",
                                     "D' = ", d_prime, " m"),
                      size = 5)+
             coord_cartesian(xlim = c(0, max(user_data$seconds)+10), 
                             ylim = c(0, max(user_data$meters)+10))+
             xlab("Seconds")+
             ylab("Meters")+
             theme_cowplot(font_size = 16)
       } else {
         
         ref <- tibble(meters = 1:43000,
                seconds = (meters - 200)/4)
         ggplot()+
          geom_line(data = sim_data(), aes(x = seconds, y = meters), color = "firebrick", size = 1.25)+
           geom_line(data = ref, aes(x = seconds, y = meters), color = "grey40", linetype = "dashed")+
           annotate("text", 
                    x = 0 , 
                    y = Inf,
                    hjust = 0,
                    vjust = 1,
                    label = paste0("Gray-dashed reference line: \n",
                                   "CS = 4 m/s \n",
                                   "D' = 200 m"),
                    size = 5)+
          xlab("Seconds")+
          ylab("Meters")+
          ggtitle("Simulation")+
          theme_cowplot(font_size = 16)
        
        }
          
     })
     
     output$prediction_table <- renderTable({
       if(input$sidebar == "Input Data"){
         validate(need(input$time1, "Enter PR data to begin. Press 'Go' to calculate."))
         req(user_data())
         cs <- round(as.numeric(user_data()$lm_coefs[[2]]), 2)
         d_prime <- round(as.numeric(user_data()$lm_coefs[[1]]), 2)
        
         
         meters <- c(800, 1000, 1500, 1609, 3000, 3200, 5000, 10000, 21097, 42195)
         
         predictions <- map_dbl(meters, ~round(predict_time_seconds(.x, d_prime, cs), 0))
         
         tibble(Meters = meters,
                Time = as.character(lubridate::seconds_to_period(predictions)))
         
       } else {
         meters <- c(800, 1000, 1500, 1609, 3000, 3200, 5000, 10000, 21097, 42195)
         
         predictions <- map_dbl(meters, ~round(predict_time_seconds(.x, input$sim_d_prime, input$sim_cs), 0))
         
         tibble(Meters = meters,
                "Sim Time" = as.character(lubridate::seconds_to_period(predictions)))
       }
     })
     observe({print(input$sidebar)})
}

# Run the application 
shinyApp(ui = ui, server = server)
