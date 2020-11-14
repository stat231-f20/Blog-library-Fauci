library(tidyverse) 
library(rvest)
library(robotstxt)
library(stringr)

#check if scraping is allowed
paths_allowed("https://www.cdc.gov/healthyyouth/policy/state_law_summary_reports.html")

#initialize empty data frame to collect data from each state's cdc site
cdc_state_data <- data.frame(matrix(ncol = 3, nrow = 0))
cdc_col_names <- c("State", "URL", "CDC Text")
colnames(cdc_state_data) <- cdc_col_names

project_data <- read_csv("data/project_data.csv")
state_names <- as.vector(data[,1])

cdc_state_data <- rbind(cdc_state_data, data.frame(State = state_names))

cdc_scraped <- cdc_state_data %>%
  mutate(URL = paste0("https://www.cdc.gov/healthyyouth/policy/txt/summary_report_factsheets/",
                      State,
                      ".txt"))

#cdc_scraped <- cdc_final

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


split_cdc_text <- data.frame(matrix(ncol = 54, nrow = 54))

for(i in 1:50){
  
  split_cdc_text[,i] <- tryCatch(
    # This is what I want to do...
    {
      lapply(str_split(cdc_scraped[i,3], "\r\n"), `length<-`, 54)
    }
    # ... but if an error occurs, set to Missing and keep going 
    , error = function(error_message) {
      return("Missing")
    }
  )
}

split_cdc_labels <- split_cdc_text[2,] %>%
  str_replace("Summary Report", "") %>%
  data.frame() %>%
  mutate(name = paste0("X",row_number()))
split_cdc_labels <- split_cdc_labels[1:50,]

cdc_split_longer <- split_cdc_text %>%
  pivot_longer(cols = X1:X50) %>%
  select(name, value) %>%
  filter(str_detect(value, "%"))

cdc_split <- cdc_split_longer %>%
  inner_join(split_cdc_labels, by = "name") %>%
  select(-c(name)) %>%
  rename("State" = ".") %>%
  separate("value", c("percent", "text"), "%", remove = TRUE) %>%
  mutate(
    var = case_when(
      str_detect(text, "Have had sexual intercourse with 4 or more partners") ~ "4_plus_partners",
      str_detect(text, " of secondary schools provided those who teach sexual health education with strategies that are age-appropriate, relevant, and actively engage students in learning") ~ "edu_strategies",
      str_detect(text, " of secondary schools across states provided those who teach sexual health education with strategies that are age-appropriate, relevant, and actively engage students in learning") ~ "edu_strategies",
      str_detect(text, "Drank alcohol or used drugs before last sexual intercourse*") ~ "intoxicated",
      str_detect(text, "Have had sexual intercourse for the first time before age 13 years") ~ "before_13",
      str_detect(text, "Used a condom during last sexual intercourse*") ~ "condom",
      str_detect(text, " of secondary schools taught how HIV and other STDs are transmitted in a required course during grades 9, 10, 11, or 12") ~ "std_edu",
      str_detect(text, " of secondary schools across states taught how HIV and other STDs are transmitted in a required course during grades 9, 10, 11, or 12") ~ "std_edu",
      TRUE ~ as.character(text)
      )
  ) %>%
  select(-c(text))

cdc_split_wider <- cdc_split %>%
  pivot_wider(names_from = var, values_from = percent) %>%
  select(-c(before_13)) %>%
  arrange(State)

project_data <- project_data %>%
  mutate("State" = paste0(State," "))

project_data <- project_data %>%
  left_join(cdc_split_wider, by = "State")

