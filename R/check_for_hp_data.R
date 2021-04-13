
check_for_hp_data <- function(file_paths) {
  
  purrr::walk(
    file_paths,
    ~ if (!file.exists(.x)) {
      stop(
        glue("File {.x} not found. Please install from Kaggle House Prices!")
      )
    }
  )
  
}
