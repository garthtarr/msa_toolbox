library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(stringr)
library(DT)
library(knitr)
# need data.table
total_msa_count =  read_csv("data/historical_msa_count.csv")
historical_msaindex = read_csv("data/msaindex_over_time.csv")
akd = read_csv("data/annual_kill_data.csv")
akdstate = read_csv("data/msa_kill_data_by_state.csv")
hmi_data = read_csv("data/historical_msa_index.csv")
# akd = akd %>% mutate(
#   year = paste(str_sub(year,1,5),str_sub(year,8,9),sep="")
# )

source("temp_unzip.R")

fread_all <- function(object, ...) {
  # just read directly to test if it is regular file
  data <- try(data.table::fread(object, nrows = 5),silent = TRUE)
  if (class(data) == "data.frame") { 
    return(data.table::fread(object, ...))
  } else {
    return(temp_unzip(object, data.table::fread, ...))
  }
}
