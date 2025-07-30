library(httr, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(jsonlite, quietly = TRUE)
library(logger, quietly = TRUE)
library(janitor, quietly = TRUE)
library(tictoc, quietly = TRUE)
library(dbutils, quietly = TRUE)
library(purrr, quietly = TRUE)
library(logger, quietly = TRUE)
library(glue, quietly = TRUE)



# FETCH FUNCTION ----------------------------------------------------------

fetch_webuycars <- function(url, max_retries = 5, wait_time = 10) {
  
  attempt <- 1
  
  while (attempt <= max_retries) {
    tryCatch({
      response <- GET(url, config(ssl_verifypeer = FALSE))
      
      if (http_status(response)$category == "Success") {
        result <- content(response, as = "text", encoding = "UTF-8") %>%
          safely(~ fromJSON(.x, flatten = TRUE))()
        
        if (!is.null(result$result)) {
          return(result$result)
        } else {
          message(glue("Attempt {attempt}: JSON parse failed."))
        }
        
      } else {
        message(glue("Attempt {attempt}: API returned status {status_code(response)}."))
      }
      
    }, error = function(e) {
      message(glue("Attempt {attempt} failed: {e$message}"))
    })
    
    attempt <- attempt + 1
    Sys.sleep(wait_time)
  }
  
  message(glue("Failed after {max_retries} attempts. Returning empty tibble."))
  return(tibble())
}

# EXECUTE -------------------------------------------------------------------------

url <- "https://website-elastic-api.webuycars.co.za/api/related/?size=15000"

df <- fetch_webuycars(url, max_retries = 20, wait_time = 20)

log_info(glue("Retrieved {nrow(df)} rows from the webuycars API"))



# CHECK DB ----------------------------------------------------------------

scraped_stock <- db_query(
  "
      SELECT 
        DISTINCT stock_number
      FROM commodities.webuycars
       ", 
  db = "greenplum_warehouse") %>% 
  pull(stock_number)

col_names <- db_query("
          SELECT 
            * 
          FROM 
            commodities.webuycars
          LIMIT 0
         ", db = "greenplum_warehouse") %>% 
  colnames()

new_stock <- stock %>%
  filter(!stock_number %in% scraped_stock) %>% 
  select(all_of(col_names))

log_info("Identified {nrow(new_stock)} new rows for insertion")


# DB WRITE-------------------------------------------------------------------------

if (nrow(new_stock) > 0) {
  new_stock %>% 
    db_write_df("commodities.webuycars", db = "greenplum_warehouse", type = "append")
  
  log_info("Successfully wrote {nrow(new_stock)} new rows to the greenplum")
} else {
  log_info("No new rows found")
}


dates %>% 
  db_write_df("commodities.webuycars_dates", db = "greenplum_warehouse", type = "append")


# DB STATUS ---------------------------------------------------------------

stock_count <- db_query("
                        SELECT
                          COUNT(DISTINCT stock_number) AS cars
                        FROM commodities.webuycars
                         ", 
                        db = "greenplum_warehouse"
) %>% pull(cars)

log_info("-----------------Captured {stock_count} cars from webuycars-----------------")

