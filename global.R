
library(dplyr)
library(purrr)
library(tidyr)
library(glue)

library(shiny)
library(shinydashboard)


# Source utility functions
purrr::walk(
  list.files(here::here("R"), full.names = TRUE),
  ~ source(.x)
)


# temporary check that data exists
# TODO: find an api call to get the data or load from external source
# data should not be in git
train_file_path <- "data/train.csv"
test_file_path <- "data/test.csv"


check_for_hp_data(
  file_paths = train_file_path
)


column_specs <- readr::spec(train_df)

train_df <- readr::read_csv(
  file = train_file_path,
  col_types = column_specs
)

clean_train_data <- prepare_hp_data(train_df = train_df)

