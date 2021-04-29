
#' Check for House Price Data
#' 
#' Helper function to determine if House Price data is in local project 
#' directory. 
#'
#' @param file_paths character vector; file paths to check for
#'
#' @return Returns no value; performs a check for data and throws error if it 
#' is not found
#' @export
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
