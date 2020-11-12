library(tidyverse) 
library(rvest)
library(robotstxt)
library(stringr)

#check if scraping is allowed
paths_allowed("https://www.cdc.gov/healthyyouth/policy/state_law_summary_reports.html")

#initialize empty data frame to collect data from each state's cdc site
cdc_state_data <- data.frame(matrix(ncol = 3, nrow = 0))
cdc_col_names <- c("State", "URL", "CDC Text")
colnames(cdc_state_data) <- cdc_names

data <- read_csv("project_data.csv")
state_names <- as.vector(data[,1])

cdc_state_data <- rbind(cdc_state_data, data.frame(State = state_names))

cdc_final <- cdc_state_data %>%
  mutate(URL = paste0("https://www.cdc.gov/healthyyouth/policy/txt/summary_report_factsheets/",
                      State,
                      ".txt"))

cdc_scraped <- cdc_final

for(i in 1:50){
  
  #identify url
  url <- cdc_final$URL[i]
  
  cdc_scraped[i,3] <- tryCatch(
    # This is what I want to do...
    { 
      (url %>%               
         read_html() %>%
         html_text)[1]
    }
    # ... but if an error occurs, set to Missing and keep going 
    , error = function(error_message) {
      return("Missing")
    }
  )
}


split_cdc_text <- data.frame(matrix(ncol = 54, nrow = 0))

for(i in 1:50){
  
  split_cdc_text[,i] <- tryCatch(
    # This is what I want to do...
    {
      str_split(cdc_scraped[i,3], "\r\n")
    }
    # ... but if an error occurs, set to Missing and keep going 
    , error = function(error_message) {
      return("Missing")
    }
  )
}


