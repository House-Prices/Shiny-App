
source_files_in_dir <- function(dir_name) {
  
  purrr::walk(
    list.files(here::here(dir_name), full.names = TRUE),
    ~ source(.x, local = FALSE)
  )
  
}

