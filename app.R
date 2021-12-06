library(shiny)
library(tidyverse)
library(cowplot)

hms_to_seconds <- function(hms){
    #hms <- c("5:12", "04:22", "03:01:01")
    hms <- str_split(hms, ":")
    map_dbl(hms, 
        ~if(length(.x) == 2){
            (as.numeric(pluck(.x, 1))*60) + (as.numeric(pluck(.x, 2)))
        } else if(length(.x) == 3){
            (as.numeric(pluck(.x, 1))*3600) + (as.numeric(pluck(.x, 2))*60) + (as.numeric(pluck(.x, 3)))
        }
    )

}

check_time <- function(string){
  if(nchar(string) <= 3){
    showNotification("All times must be at least 3 digits and include a colon. Ex. 5:20", type = "error")
    req(nchar(string)>3)
  }
  
  if(nchar(string)<=5){
    if(str_sub(string, -3, -3) != ":"){
      showNotification("All times must be in HH:MM:SS or HH:SS", type = "error")
      req(str_sub(string, -3, -3) == ":")
    }
    
  } else if(nchar(string)<=8 & nchar(string) > 5){
    if(str_sub(string, -3, -3) != ":"){
      showNotification("All times must be in HH:MM:SS or HH:SS 2", type = "error")
      req(str_sub(string, -3, -3) == ":")
    } 
    
    if(str_sub(string, -6, -6) != ":"){
      showNotification("All times must be in HH:MM:SS or HH:SS 3", type = "error")
      req(str_sub(string, -6, -6) == ":")
    }
  } else if(nchar(string)>8){
    showNotification("Stime string is too long. Must be HH:MM:SS maximum.", type = "error")
    req(nchar(string)<=8)
  }
  
  
  if(str_detect(string, "[:alpha:]")){
    showNotification("No letters allowed in time input.", type = "error")
    req(!str_detect(string, "[:alpha:]"))
    
  }
  
  if(str_detect(string, "[!@#$%^&*()-+={};<>,.?\"\'_]")){
    showNotification("Only special character allowed in time input is a ':'", type = "error")
    req(!str_detect(string, "[!@#$%^&*()-+={};<>,.?\"\'_]"))
  } 
  
}

predict_time_seconds <- function(meters, d_prime, cs){
  (meters - d_prime)/cs
}

ui <- fluidPage(
    titlePanel("Critical Speed App"),
    sidebarLayout(
        sidebarPanel(
          tabsetPanel(id = "sidebar",
            tabPanel("Input Data",
                     br(),
            sliderInput("number_of_prs", 
                        "Select number of PRs to input:",
                        min = 2,
                        max = 10,
                        value = 2),
            uiOutput("pr_inputs"),
            actionButton("enter", "Go")
          ),
          tabPanel("Sim Data",
                   br(),
            fluidRow(
              column(6, 
                     sliderInput("sim_cs", 
                                 "Critical Speed (m/s)",
                                 value = 4,
                                 min = 0, 
                                 max = 10,
                                 step = 0.1)),
              column(6,
                     sliderInput("sim_d_prime", 
                                 "D' (m)",
                                 value = 200,
                                 min = 0, 
                                 max = 1000)
                     )
                   ),
            actionButton("sim", "Go"),
            )
         )
       ),
        mainPanel(
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

 distance_names <- reactive( paste0("distance", seq_len(input$number_of_prs)) )
    
 time_names <- reactive( paste0("time", seq_len(input$number_of_prs)) )

 output$pr_inputs <- renderUI({
     l <- list(d = distance_names(), t = time_names(), n = seq_len(input$number_of_prs))
     pmap(l,
          function(d, t, n)
            fluidRow(
              column(
               width = 6,
               textInput(
                 d, 
                 paste0("Distance ", n ),
                 placeholder = "meters")
               ),
             column(width = 6,
                textInput(
                t, 
                paste0("Time ", n),
                placeholder = "HH:MM:SS")
           )
          )
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
