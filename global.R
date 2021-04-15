
library(dplyr)
library(purrr)
library(tidyr)
library(glue)
library(readr)
library(here)
library(janitor)

library(shiny)
library(shinydashboard)

library(httr)


# Source utility functions
purrr::walk(
  list.files(here::here("R"), full.names = TRUE),
  ~ source(.x)
)

source_files_in_dir("modules")

data_list <- readRDS("data/data_list.rds")


# clean_train_data <- prepare_hp_data(data_list)
