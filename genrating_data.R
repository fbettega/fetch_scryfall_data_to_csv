## parsing deck
#source : https://scryfall.com/docs/api/bulk-data
library(tidyverse)
library(scryr)
library(RCurl)
# library("ndjson")
#conflict_prefer("fromJSON", "rjson")
# mode <- "modern"
# string <- "Oracle Cards"





DL_scry_fall_json <- function(string){
options(timeout = 10000)
  
files <- scry_bulk_files()
# json <- httr::GET(files$download_uri[files$name == string])

# download.file(files$download_uri[files$name == string],
#               destfile = paste0(
#                 "./data/data_",str_replace(string,"\\s+","_"),".json"
#                 ),
#               # cacheOK = FALSE,
#               quiet = TRUE,
#               mode="wb")

print(paste0("start DL : ",string))
file_dl = RCurl::CFILE(
  paste0(
  "./data/data_",str_replace(string,"\\s+","_"),".json"
  ),
  mode = "wb"
  )

RCurl::curlPerform(url = files$download_uri[files$name == string],
                   writedata = file_dl@ref)
close(file_dl)



options(timeout = 60)
print(paste0("end DL : ",string))
data_frame_res <- jsonlite::fromJSON(
  paste0("./data/data_",str_replace(string,"\\s+","_"),".json")#,  flatten = TRUE
  ) %>%
  select(where(~all(!is.na(.))))


# unlist(paste0("./data/data_",str_replace(string,"\\s+","_"),".json"))

return(data_frame_res)
}



pull_scry_fall <- function(mode){
  tictoc::tic(mode)
  if (mode == "oracle") {
  base_data <- DL_scry_fall_json(
    string = "Oracle Cards"
   # "Default Cards"
  ) 
  base_data_rename <- base_data %>%
    rename_all(~make.unique(colnames(base_data))) %>% 
    as_tibble()
  
  rm(base_data)
  } else if(mode == "all") {
    base_data <- DL_scry_fall_json(
      #scry_bulk_file(
      string =  "All Cards"
    ) 
    
    base_data_rename <- base_data %>%
      rename_all(~make.unique(colnames(base_data))) %>% 
      as_tibble()
    
    rm(base_data)
  }
  unnest_column_name <- base_data_rename  %>%
    select(where(is.list)) %>% colnames()
  
  
  unnest_table_col <- map_dfc(unnest_column_name,
                              ~ unnest_wider(base_data_rename[.x], .x, names_sep = "."))
  
  complete_table <- cbind(
    base_data_rename %>% 
      select(-all_of(unnest_column_name)),
    unnest_table_col
    )

  if (mode == "oracle") {
    write.csv(
      complete_table %>%
                  # filter(legalities.modern == "legal")  %>%
                  # distinct(name,.keep_all = TRUE) %>%
                  purrr::keep(~!all(is.na(.))),
              file = "MTG_data/DBcarte_oracle.csv")
  } else if (mode == "all") {
    write.csv(complete_table %>%
                purrr::keep(~!all(is.na(.))) ,
              file = "MTG_data/DBcarte.csv")
  }
  tictoc::toc()
}



pull_scry_fall(mode = "oracle")

pull_scry_fall(mode = "all")


