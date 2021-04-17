
download_hp_data <- function(username, key) {
  
  path <- "https://www.kaggle.com/api/v1/competitions/data/download/5407/"
  auth <- httr::authenticate(username, key)
  
  files_to_get <- c("train", "test")
  file_type <- ".csv"
  
  data_list <- purrr::map(
    files_to_get,
    ~ httr::GET(paste0(path, .x, file_type), auth) %>% 
        httr::content(as = "text", encoding = "UTF-8") %>% 
        {read.csv(text = ., colClasses = "character", stringsAsFactors = FALSE)} %>% 
        readr::type_convert(
          col_types = readr::cols(
            Id = readr::col_double(),
            MSSubClass = readr::col_double(),
            MSZoning = readr::col_character(),
            LotFrontage = readr::col_double(),
            LotArea = readr::col_double(),
            Street = readr::col_character(),
            Alley = readr::col_character(),
            LotShape = readr::col_character(),
            LandContour = readr::col_character(),
            Utilities = readr::col_character(),
            LotConfig = readr::col_character(),
            LandSlope = readr::col_character(),
            Neighborhood = readr::col_character(),
            Condition1 = readr::col_character(),
            Condition2 = readr::col_character(),
            BldgType = readr::col_character(),
            HouseStyle = readr::col_character(),
            OverallQual = readr::col_double(),
            OverallCond = readr::col_double(),
            YearBuilt = readr::col_double(),
            YearRemodAdd = readr::col_double(),
            RoofStyle = readr::col_character(),
            RoofMatl = readr::col_character(),
            Exterior1st = readr::col_character(),
            Exterior2nd = readr::col_character(),
            MasVnrType = readr::col_character(),
            MasVnrArea = readr::col_double(),
            ExterQual = readr::col_character(),
            ExterCond = readr::col_character(),
            Foundation = readr::col_character(),
            BsmtQual = readr::col_character(),
            BsmtCond = readr::col_character(),
            BsmtExposure = readr::col_character(),
            BsmtFinType1 = readr::col_character(),
            BsmtFinSF1 = readr::col_double(),
            BsmtFinType2 = readr::col_character(),
            BsmtFinSF2 = readr::col_double(),
            BsmtUnfSF = readr::col_double(),
            TotalBsmtSF = readr::col_double(),
            Heating = readr::col_character(),
            HeatingQC = readr::col_character(),
            CentralAir = readr::col_character(),
            Electrical = readr::col_character(),
            `1stFlrSF` = readr::col_double(),
            `2ndFlrSF` = readr::col_double(),
            LowQualFinSF = readr::col_double(),
            GrLivArea = readr::col_double(),
            BsmtFullBath = readr::col_double(),
            BsmtHalfBath = readr::col_double(),
            FullBath = readr::col_double(),
            HalfBath = readr::col_double(),
            BedroomAbvGr = readr::col_double(),
            KitchenAbvGr = readr::col_double(),
            KitchenQual = readr::col_character(),
            TotRmsAbvGrd = readr::col_double(),
            Functional = readr::col_character(),
            Fireplaces = readr::col_double(),
            FireplaceQu = readr::col_character(),
            GarageType = readr::col_character(),
            GarageYrBlt = readr::col_double(),
            GarageFinish = readr::col_character(),
            GarageCars = readr::col_double(),
            GarageArea = readr::col_double(),
            GarageQual = readr::col_character(),
            GarageCond = readr::col_character(),
            PavedDrive = readr::col_character(),
            WoodDeckSF = readr::col_double(),
            OpenPorchSF = readr::col_double(),
            EnclosedPorch = readr::col_double(),
            `3SsnPorch` = readr::col_double(),
            ScreenPorch = readr::col_double(),
            PoolArea = readr::col_double(),
            PoolQC = readr::col_character(),
            Fence = readr::col_character(),
            MiscFeature = readr::col_character(),
            MiscVal = readr::col_double(),
            MoSold = readr::col_double(),
            YrSold = readr::col_double(),
            SaleType = readr::col_character(),
            SaleCondition = readr::col_character(),
            SalePrice = readr::col_double()
          )
        )
  ) %>% 
    purrr::set_names(files_to_get)
  
  
  description_df <- httr::GET(paste0(path, "data_description.txt"), auth) %>% 
    httr::content(as = "text", encoding = "UTF-8") %>% 
    strsplit("\n") %>% unlist() %>% 
    stringr::str_subset(":") %>% 
    stringr::str_subset("^[:alpha:]") %>% 
    {tibble(
      "raw" = .
    )} %>% 
    tidyr::separate(col = "raw", into = c("variable", "description"), sep = ":") %>% 
    dplyr::mutate(
      description = stringr::str_remove_all(description, "^[:blank:]"),
      description = stringr::str_remove_all(description, "\t")
    )
  
  data_list[["description"]] <- description_df
  
  
  return(data_list)
  
}
