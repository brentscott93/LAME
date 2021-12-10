
ui <- fluidPage(
    titlePanel("Critical Speed App"),
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

# Define server logic required to draw a histogram
server <- function(input, output) {

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
  
  
  
  
 athlete_names <- reactive( paste0("athlete", seq_len(input$number_of_athletes)) )
  
 distance_names <- reactive( paste0("distance", seq_len(input$number_of_prs)) )
    
 time_names <- reactive( paste0("time", seq_len(input$number_of_prs)) )

 output$pr_inputs <- renderUI({
     l <- list(a = athlete_names(),
               d = distance_names(), 
               t = time_names(), 
               n = seq_len(input$number_of_prs))
     pmap(l,
          function(a, d, t, n){
            tabPanel(title = paste0("Athlete ", n),
             fluidRow(
               sliderInput(paste0(a, "number_of_prs"), 
                           "Number of PRs",
                           min = 2,
                           max = 10,
                           value = 2),
              column(
               width = 6,
               textInput(
                 paste0(a, "_", d), 
                 paste0("Distance ", n ),
                 placeholder = "meters")
               ),
             column(width = 6,
                textInput(
                paste0(a, "_", t), 
                paste0("Time ", n),
                placeholder = "HH:MM:SS")
           )
          )
         )
        }
       )
 })
     
     user_data <- eventReactive(input$enter, {
         d <- map_chr(distance_names(), ~input[[.x]] %||% "")
         if(!all(d!="")){
             showNotification("Distance input missing")
         }
         req(all(d!=""))
         
         is_only_numbers <- str_detect(d, "[:digit:]")
         if(!all(is_only_numbers)) {
             showNotification("Only numbers in 'Distance' input")
         }
         req(all(is_only_numbers))
         
         t <- map_chr(time_names(), ~input[[.x]] %||% "")
        
         if(!all(t!="")){
             showNotification("Time input missoing")
         }
         req(all(t!=""))
         
         walk(t, check_time)
         
         t <- hms_to_seconds(t)
         df <- data.frame(meters = as.numeric(d),
                          seconds = as.numeric(t))
         
         # df <- data.frame(meters = c(1000, 5000),
         #                  seconds = c(151, 960))
         lm_mod <- lm(meters ~ seconds, data = df)
         coefs <- coef(lm_mod)
         
         lm_predict <- as.data.frame(
                        predict(lm_mod, 
                                newdata = data.frame(seconds = 0:21600),
                                interval = "conf")) %>% 
             mutate(x = 0:21600)
         
         list(data = df,
              lm_coefs = coefs, 
              lm_predict = lm_predict)
         
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
