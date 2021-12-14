library(shiny)
library(tidyverse)
library(cowplot)
library(rhandsontable)
library(RColorBrewer)
library(ggpubr)
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
    tibble("Athlete" = c(rep("Athlete 1", 4), rep("Athlete 2", 4)),
           "Distance (m)" = as.integer(c(1500, 3000, 5000, 10000, 1500, 3000, 5000, 10000)) ,
           "Hours" = rep(0, 8),
           "Minutes" = c(4, 10, 16, 33, 5, 11, 18, 36),
           "Seconds" = c(18, 00, 20, 30, 10, 2, 55, 12)) %>% 
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
        
      lm_predict <- 
        cs_data %>% 
        group_by(athlete) %>% 
        dplyr::select(athlete, lm_predict) %>% 
        unnest(cols = lm_predict)

      
      # text_labels <- 
      #   cs_data %>% 
      #   dplyr::select(athlete, critical_speed_meters_second, d_prime_meters, label_offset, y, x) %>% 
      #   mutate(label = paste0("y = ", 
      #                         round(critical_speed_meters_second, 2), 
      #                         "x + ", 
      #                         round(d_prime_meters, 0)))
      
      plot_colors <- RColorBrewer::brewer.pal(8, "Dark2")[1:nrow(cs_data)]
      cs_table <-
        cs_data %>% 
        dplyr::select("Athlete" = athlete, 
                      "CS (m/s)" = critical_speed_meters_second, 
                      "D' (m)" = d_prime_meters) %>% 
        mutate(across(where(is.numeric), ~round(., digits = 2))) %>% 
        ggpubr::ggtexttable(theme = ggpubr::ttheme(base_style = 'light', 
                                                   base_size = 20,
                                                   colnames.style = ggpubr::colnames_style(fill = "black", color = "white", size = 16),
                                                   tbody.style = ggpubr::tbody_style(fill = alpha(plot_colors, 0.4), size = 16)),
                            
                            rows = NULL) 
      
      cs$pr_data <- pr_data
      cs$cs_data <- cs_data
      cs$cs_table <- cs_table
      cs$lm_predict <- lm_predict
      cs$plot_colors <- plot_colors
         
 })
     
    sim_data <- eventReactive(input$sim,{
      tibble(meters = 1:43000,
             seconds = (meters - input$sim_d_prime)/input$sim_cs)
    })
   
     output$cs_plot <- renderPlot({

       validate(need(cs$pr_data, "Enter PR data to begin. Press 'Go' to calculate."))
    
         main_plot <-
           ggplot()+
             geom_point(data = cs$pr_data, aes(seconds, meters, color = athlete), size = 2)+
             geom_ribbon(data = cs$lm_predict, aes(x = x, ymin = lwr, ymax = upr, fill  = athlete), alpha = 0.3)+
             geom_line(data = cs$lm_predict, aes(x = x, y = fit, color = athlete), linetype = "dashed")+
             coord_cartesian(xlim = c(0, max(cs$pr_data$seconds)+10),
                             ylim = c(0, max(cs$pr_data$meters)+10))+
             scale_color_manual(values = cs$plot_colors)+
             scale_fill_manual(values = cs$plot_colors)+
             xlab("Seconds")+
             ylab("Meters")+
             theme_minimal_grid(font_size = 16)+
           theme(
             legend.position = "none"
           )
    
         # ggdraw(main_plot)+
         #   draw_plot(cs$cs_table, 0, 1, 0.4, 0.4)
         #  
         plot_grid(main_plot, cs$cs_table, rel_widths = c(0.7, 0.3))
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
