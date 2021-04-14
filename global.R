
library(dplyr)
library(purrr)
library(tidyr)
library(glue)
library(readr)

library(shiny)
library(shinydashboard)

library(httr)


# Source utility functions
purrr::walk(
  list.files(here::here("R"), full.names = TRUE),
  ~ source(.x)
)


data_list <- download_hp_data(
  username = Sys.getenv("KAGGLE_USERNAME"),
  key = Sys.getenv("KAGGLE_KEY")
)


clean_train_data <- prepare_hp_data(data_list)
