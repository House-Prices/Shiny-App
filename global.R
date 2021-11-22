
# Load app dependencies
library(dplyr)
library(tidyselect)
library(purrr)
library(tidyr)
library(glue)
library(readr)
library(here)
library(janitor)
library(inspectdf)

library(ggplot2)
library(patchwork)
library(gridExtra)
library(corrplot)

library(shiny)
library(bs4Dash)
library(shinyWidgets)

library(httr)


# Helper function
source_files_in_dir <- function(dir_name) {
  
  purrr::walk(
    list.files(here::here(dir_name), full.names = TRUE),
    ~ source(.x, local = FALSE)
  )
  
}


# Source utility functions
source_files_in_dir("R")
source_files_in_dir("modules")

data_list <- readRDS("data/data_list.rds")

