library(shiny)
library(tidyverse)
library(cowplot)
library(rhandsontable)
library(RColorBrewer)
library(ggpubr)
library(DT)
library(kin697u)

ui <- navbarPage("WOUTER",
       tabPanel("Critical Speed",
        sidebarLayout(
         sidebarPanel(width = 4,
          h4("Critical Speed Calculator:"),
          h6("Fully editable table"),
          h6("Right click to add more rows"),
          h6("Multiple athletes accepted in long format"),
          checkboxInput("use_demo_cs", label = "Use demo data", value = FALSE),
          rHandsontableOutput("runner_pr_input"),
          br(),
          actionButton("cs_enter", "Calculate")
           ),
          mainPanel(width = 8, 
            tabsetPanel(
             tabPanel("Plot",
              plotOutput("cs_plot")
                     ),
             tabPanel("Model Summary",
               verbatimTextOutput("lm_model")
             ),
              tabPanel("Race Predictions",
                tableOutput("race_prediction_table")
               )
              )
             )
            )
           ),
          tabPanel("D' Balance",
            sidebarLayout(
             sidebarPanel(width = 5,
              h4("D' Balance Race Simulator:"),
              h6("Fill out BOTH runner & race data"),
              checkboxInput("use_demo_data", label = "Use demo data", value = FALSE),
              tabsetPanel(
                tabPanel("Runner Data",
                 h6("One athlete per row"),
                 h6("Right click to add more rows"),
                 rHandsontableOutput("d_balance_runner_data_input")),
                tabPanel("Race Data",
                 h6("Each row is a split"),
                 h6("Enter cumulative distance (meters) at each split"),
                 h6("Enter total elapsed time at split"),
                 h6("Right click to add more rows"),
                 rHandsontableOutput("d_balance_race_data_input"))
              ),
              br(),
              actionButton("calculate_d_balance", "Calculate")
            ),
            mainPanel(width = 7, 
             tabsetPanel(
              tabPanel("Plot",
               plotOutput("d_balance_plot")
                ),
              tabPanel("Data",
               DTOutput("d_balance_data")
                )
               )
              )
             )
            ),
       tabPanel("Disclaimer",
         HTML("<h6>This is a random web application made by some random guy on the internet. 
            You should verify all the calculation by looking through the source code.</h6>")
       ),
       tabPanel("Acknowldgements",
                HTML("")
       )
      )
           

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$runner_pr_input <- renderRHandsontable({
    if(input$use_demo_cs){
     pr_input <- 
      tribble(
        ~Athlete, ~"Distance (m)", ~Hours, ~Minutes, ~Seconds, 
        "Hassan", 800, 0, 1, 56,
        "Hassan", 1000, 0, 2, 34,
        "Hassan", 1500, 0, 3, 51,
        "Hassan", 1609, 0, 4, 12,
        "Hassan", 3000, 0, 8, 18,
        "Hassan", 5000, 0, 14,22,
        "Hassan", 10000, 0, 29, 06,
        
        "Obiri", 800, 0, 2, 0,
        "Obiri", 1500, 0, 3, 57,
        "Obiri", 1609, 0, 4, 16,
        "Obiri", 3000, 0, 8, 20,
        "Obiri", 3218, 0, 9, 14,
        "Obiri", 5000, 0 , 14, 18,
        "Obiri", 10000, 0, 29, 59)
    } else {
     pr_input <- 
      tibble("Athlete" = rep("Athlete 1", 4),
           "Distance (m)" = as.integer(c(1500, 3000, 5000, 10000)),
           "Hours" = rep(0, 4),
           "Minutes" = rep(0, 4),
           "Seconds" = rep(0, 4))
    }
    pr_input %>% 
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
    plot_grid(main_plot, cs$cs_table, rel_widths = c(0.7, 0.3))
 })
     
 output$lm_model <- renderPrint({
  mod_summary <- map(cs$cs_data$lm_mod, summary)
  names(mod_summary) <- cs$cs_data$athlete
  mod_summary
 })
     
output$race_prediction_table <- renderTable({
 validate(need(cs$cs_data, "Enter PR data to begin. Press 'Go' to calculate."))
 req(cs$cs_data)
 cs$cs_data %>% 
  dplyr::select(athlete, critical_speed_meters_second, d_prime_meters) %>% 
  group_by(athlete) %>% 
  mutate(predict_table = map2(critical_speed_meters_second ,
                              d_prime_meters,
                              ~tibble(Meters = c(800, 1000, 1500, 1609, 3000, 3200, 5000, 10000, 21097, 42195),
                                      Time =  as.character(lubridate::seconds_to_period(round((Meters - .y)/.x, 0)))
                                      )
                                     )
                                    ) %>% 
  dplyr::select("Athlete" = athlete, predict_table) %>% 
  unnest(cols = c(predict_table))
 })

#### D' Balance ####
 output$d_balance_runner_data_input <- renderRHandsontable({
   
   if(input$use_demo_data){
     runner_data_input <- 
       tribble(
               ~Athlete, ~"Critical Speed (m/s)", ~"D' Prime (m)",
               "Hassan", 5.63, 169,
               "Obiri", 5.46, 212,
               "Tsegay", 5.53, 196,
               "Tirop", 5.37, 256
     )
   } else {
     runner_data_input <- 
       tibble(Athlete = "Athlete 1",
             "Critical Speed (m/s)" = NA,
              "D' Prime (m)" = NA) 
   }
   
  runner_data_input %>% 
     rhandsontable() %>% 
     hot_col(col = "Critical Speed (m/s)", type = "numeric") %>% 
     hot_col(col =  "D' Prime (m)", type = "numeric")
     
 })

 output$d_balance_race_data_input <- renderRHandsontable({
   
   if(input$use_demo_data){
     race_data_input <- 
       tribble(
         ~"Cumulative Meters", ~"Hours", ~"Minutes", ~"Seconds",
         200,  "0", "0",   "38",
         1000, "0", "3",   "0",
         1400, "0", "4",   "12",
         1800, "0", "5",   "25",
         2000, "0", "6",   "0",
         2200, "0", "6",   "36",
         2600, "0", "7",   "46",
         3000, "0", "8" ,  "59",
         3400, "0", "10",  "11",
         3800, "0", "11",  "21",
         4000, "0", "11" , "57",
         4200, "0", "12" , "31",
         4600, "0", "13",  "39", 
         4800, "0", "14" , "9",
         5000, "0", "14" , "36"
       ) 
     
   } else {
    race_data_input <- 
     tibble("Cumulative Meters" = rep(NA, 13),
            "Hours" = rep(NA, 13),
            "Minutes" = rep(NA, 13),
            "Seconds" = rep(NA, 13)) 
   }
   
   race_data_input %>% 
    mutate("Cumulative Meters" = as.integer(`Cumulative Meters`)) %>% 
    rhandsontable() %>% 
    hot_col(col = "Cumulative Meters", type = "numeric") %>% 
    hot_col(col = "Hours", type = "dropdown", source = 0:9) %>% 
    hot_col(col = "Minutes", type = "dropdown", source = 0:59) %>% 
    hot_col(col = "Seconds", type = "dropdown", source = 0:59)
 })
 
 d_bal <- reactiveValues()
 observeEvent(input$calculate_d_balance, {
   if(is.null(input$d_balance_race_data_input)){
     showNotification("Need to input Race data, or switch to tab to activate", type = "error")
   } else if(anyNA(hot_to_r(input$d_balance_race_data_input))){
     showNotification("Need to input Race data, or switch to tab to activate", type = "error")
   }
   req(!is.null(input$d_balance_race_data_input))
   req(!anyNA(hot_to_r(input$d_balance_race_data_input)))
   
   if(is.null(input$d_balance_runner_data_input)){
     showNotification("Need to input Runner data, or switch to tab to activate", type = "error")
   } else if(anyNA(hot_to_r(input$d_balance_runner_data_input))){
     showNotification("Need to input Runner data, or switch to tab to activate", type = "error")
   }
   req(!is.null(input$d_balance_runner_data_input))
   req(!anyNA(hot_to_r(input$d_balance_runner_data_input)))
   
   runner_data <- 
     hot_to_r(input$d_balance_runner_data_input) %>% 
     rename("athlete" = Athlete,
            critical_speed_meters_second = "Critical Speed (m/s)",
            d_prime_meters = "D' Prime (m)")
   
   race_data <-
     hot_to_r(input$d_balance_race_data_input) %>% 
     mutate(
       seconds = ((as.numeric(Hours)*3600) + (as.numeric(Minutes)*60) + as.numeric(Seconds)),
       relative_split_time_seconds = seconds-lag(seconds, 1, default = 0),
       lap_distance_meters = `Cumulative Meters` - lag(`Cumulative Meters`, default = 0),
       lap_leader_speed_meters_second = lap_distance_meters/relative_split_time_seconds
     )
   
   
   d_balance_race <- kin697u::calc_d_balance(runner_data = runner_data,
                                             race_data = race_data)
   
   finish <- 
     d_balance_race %>%
     dplyr::filter(total_race_distance_meters == max(total_race_distance_meters)) %>%
     arrange(desc(d_balance)) %>% 
     mutate(finish_position = 1:nrow(.)) 
   
   finish_order <- finish %>% pull(athlete)
   
   d_balance_race$athlete <- factor(d_balance_race$athlete, levels = finish_order)
   
   d_bal$data <- d_balance_race
   d_bal$finish <- finish
 })
 
 output$d_balance_data <- renderDT({
   req(d_bal$data)
   d_bal$data %>% 
     mutate(across(c(meters_second, d_balance, total_race_distance_meters), ~round(., 2))) %>% 
     select("Athlete" = athlete,
            "Lap" = lap,
            "Lap Timer" = "lap_time",
            "Lap Speed (m/s)" = meters_second,
            "D' Balance" = d_balance,
            "Status" = balance_status,
            "Total Elapsed Time (s)" = total_race_time, 
            "Total Distance (m)" = total_race_distance_meters)
     
 })
 
 output$d_balance_plot <- renderPlot({
   req(d_bal$data)
   plot_colors <-  c("gold", "grey50", "darkorange", "black", RColorBrewer::brewer.pal(8, "Dark2"))
   number_finishers <- 1:nrow(d_bal$finish)
   d_bal_plot <- 
     plot_d_balance(d_bal$data)+
      scale_color_manual(values = plot_colors[number_finishers])+
      labs(title = "Race Simulation using D' Balance Model",
           subtitle = "Athlete with highest remaining D' Balance at finish is most likely winner*",
           caption = "*Simulations are not always 100% accurate...")+
     theme(
       legend.position = "none"
     )
   
   finish_table <- 
     d_bal$finish %>% 
     mutate(d_balance =  round(d_balance, 0)) %>% 
     dplyr::select(
       "Athlete" = athlete,
       "D' Finish (m)" = d_balance,
       "Position" = finish_position
     ) %>% 
     ggpubr::ggtexttable(theme = ggpubr::ttheme(base_style = 'light', 
                                                base_size = 14,
                                                colnames.style = ggpubr::colnames_style(fill = "black", color = "white", size = 12),
                                                tbody.style = ggpubr::tbody_style(fill = alpha(plot_colors[number_finishers], 0.4), size = 16)),
                         
                         rows = NULL) 
   
   plot_grid(d_bal_plot, finish_table, rel_widths = c(0.7, 0.3))
 })
}

# Run the application 
shinyApp(ui = ui, server = server)
