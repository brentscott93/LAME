library(shiny)
library(tidyverse)
library(cowplot)
library(rhandsontable)

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
