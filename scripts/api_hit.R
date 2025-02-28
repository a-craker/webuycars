library(httr)
library(tidyverse)
library(jsonlite)
library(logger)
library(janitor)
library(tictoc)
library(dbutils)
library(purrr)
library(logger)


# FUNCTIONS ---------------------------------------------------------------

fetch_webuycars <- function(url, max_retries, wait_time) {
  
  attempt <- 1
  while (attempt <= max_retries) {
    
    try({
      response <- fromJSON(url, simplifyVector = TRUE, flatten = TRUE)
      if(!is.null(response)) {
        
        return(response)
      }
    }, silent = TRUE)
    
    message(paste("Attempt", attempt, "failed. Retrying in", wait_time, "seconds..."))
    
    Sys.sleep(wait_time)
    attempt <- attempt + 1
  }
  
  stop("Failed to retrieve data after", max_retries, "attempts. Attempt manually.")
  
}

# LOAD DATA ---------------------------------------------------------------

url <- "https://website-elastic-api.webuycars.co.za/api/related/?size=10000"

df <- fetch_webuycars(url, max_retries = 10, wait_time = 20)

log_info("Retrieved {nrow(df)} rows from the webuycars API")

cars <- df %>% 
  as_tibble() %>% 
  clean_names %>% 
  select(-matches("image|auction|total_number_of_bids"), 
         -data_timestamp) %>% 
  select(-where(is.list)) %>% 
  mutate(scrape_date = Sys.time())

dates <- cars %>% 
  select(scrape_date,
         stock_number) %>% 
  mutate(scrape_date = as_date(scrape_date))

stock <- cars %>% 
  select(-scrape_date)

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

