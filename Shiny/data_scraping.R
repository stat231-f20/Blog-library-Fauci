library(tidyverse) 
library(rvest)
library(robotstxt)
library(readr)
library(datasets)
library(naniar)
library(rlist)

#Scraping from Risky Behaviors (Cat)
#List of State Abbreviations
data(state)
state_info <- data.frame(state_full = tolower(state.name)
                         , State = state.abb
                         , Region = state.region)
abblist <- state_info$State
typelist<- c("COND", "NP", "STD", "HIV")
path = "data_behavior" 
states_list = list()
blank = NA
for (i in abblist){
  info_list = list()
  for (j in typelist){
    filename = paste0("data_behavior/", i, "_", j, ".csv")
    if(file.exists(filename)){
      data = read_csv(filename, skip = 1) %>% 
        head(-3) %>%
        mutate(state = i) %>%
        mutate(type = j) %>%
        mutate_all(funs(str_replace(., "�", "")))%>%
        mutate_all(funs(str_replace(., "\\*", "")))%>%
        mutate_all(funs(na_if(., ""))) %>%
        rename_with( ~ tolower(gsub(" ", "_", .x, fixed = TRUE))) %>% 
        rename_with( ~ gsub("_%", "", .x, fixed = TRUE)) %>%
        mutate(sex_of_sexual_contacts = tolower(gsub(" ", "_", sex_of_sexual_contacts, fixed = TRUE))) %>%
        select("state", "type", "sex_of_sexual_contacts", "total_n","female_n","male_n") %>%
        pivot_wider(names_from = "sex_of_sexual_contacts", values_from = c("total_n","female_n","male_n")) 
      
      if(is.na(blank)){
        blank <- data 
        for (k in c(1:length(blank))){
          blank[k] = NA
        }
      }
      info_list[[paste0(i, "_", j)]] = data
      
    }
    else{
      data = blank
      data["state"] = i
      data["type"] = j
      info_list[[paste0(i, "_", j)]] = data
    }
    states_list[[paste0(i)]] = bind_rows(info_list)
  }
}
full_data <-bind_rows(states_list)

#Scraping from alarms.org and powertodecide.org (Lauren)

url <- "https://www.alarms.org/std-statistics/"
paths_allowed(url)

tables <- url %>% 
  read_html() %>% 
  html_nodes("table")

alarms <- tables %>%
  purrr::pluck(1) %>%
  html_table()

write.csv(alarms, "/Users/laurenpelosi/Desktop/git/Shiny-library-Fauci/STDs")

url2 <- "https://powertodecide.org/what-we-do/information/national-state-data/teen-birth-rate"
paths_allowed(url2)

tables <- url2 %>% 
  read_html() %>% 
  html_nodes("table")

power_to_decide <- tables %>%
  purrr::pluck(1) %>%
  html_table()

write.csv(power_to_decide, "/Users/laurenpelosi/Desktop/git/Shiny-library-Fauci/teen-birth")

View(alarms)
View(power_to_decide)

#Scraping from Guttamacher (Aditi)
#confirm scraping is allowed
library(robotstxt)
paths_allowed("https://www.guttmacher.org")

#scrape table for general sex and HIV education requirements
library(rvest)
library(methods)
url <- "https://www.guttmacher.org/state-policy/explore/sex-and-hiv-education#"
tables <- url %>%
  read_html() %>%
  html_nodes("table")

#collect general education requirement table in csv
education_gen <- as.data.frame(html_table(tables[1], fill = TRUE)) 

education_gen[44,2] <- "X'"

education_gen <- education_gen %>%
  rename(State = X1,
         Sex_Mandated = X2, 
         HIV_Mandated = X3,
         Medically_Accurate = X4,
         Age_Appropriate = X5,
         Unbiased = X6,
         Cannot_Promote_Religion = X7,
         Parents_Notified = X8,
         Parental_Consent = X9,
         Opt_Out = X10) %>%
  head(-2) %>% 
  tail(-3) 

education_gen_text <- education_gen %>% 
  rename_with(~ paste(.x, "txt", sep = "_"))

education_gen_text[education_gen_text == ""] <- NA

education_gen_text <- education_gen_text %>%
  mutate_all(funs(if_else(str_detect(., "X'"),"Yes, but*",.))) %>%
  mutate_all(funs(if_else(str_detect(., "X"),"Yes",.))) %>%
  mutate_all(funs(if_else(str_detect(., "HIV"),"HIV Only",.))) %>%
  mutate_all(funs(if_else(str_detect(., "Sex"),"Sex Only",.))) %>%
  mutate_all(funs(if_else(is.na(.),"No",.)))
#Yes, but only if county pregnancy rate is at least 19.5 or higher per 1K for girls aged 15—17
education_gen[education_gen == "X"] <- 3
education_gen[education_gen == "X'"] <- 2
education_gen[education_gen == ""] <- 0
education_gen[education_gen == "HIV"] <- 1
education_gen[education_gen == "Sex"] <- 2

# education_gen[education_gen$'Parents Noticed' == 0] <- 3
# education_gen[education_gen$'Parents Noticed' == 2] <- 0
# education_gen[education_gen$'Parents Noticed' == 3] <- 2

write_csv(education_gen,
          "data/education-gen.csv", 
          na = "NA", 
          append = FALSE,
          col_names = TRUE,
          quote_escape = "double")

write_csv(education_gen_text,
          "data/education-gen-text.csv", 
          na = "NA", 
          append = FALSE,
          col_names = TRUE,
          quote_escape = "double")

#collect content requirements table in csv
education_content <- as.data.frame(html_table(tables[2], fill = TRUE)) 

education_content <- education_content %>%
  rename(State = X1, 
         Sex_Contraception = X2, 
         Sex_Abstinence = X3,
         Sex_Marriage = X4,
         Sex_Orientation = X5,
         Sex_Negative_Outcomes = X6,
         HIV_Condoms = X7,
         HIV_Abstinence = X8) %>%
  select(-c(X9, X10, X11)) %>%
  tail(-3) %>% 
  head(-2)

education_content[31,5] <- "Nega'tive"

education_content_text <- education_content %>%
  rename_with(~ paste(.x, "txt", sep = "_"))

education_content_text[education_content_text == ""] <- NA

education_content_text <- education_content_text %>%
  mutate_all(funs(if_else(str_detect(., "X"),"Yes",.))) %>%
  mutate_all(funs(if_else(str_detect(., "Inclusive"),"Yes, Inclusive",.))) %>%
  mutate_all(funs(if_else(str_detect(., "Cover"),"Yes, Cover",.))) %>%
  mutate_all(funs(if_else(str_detect(., "Negative"),"Yes, Negative",.))) %>%
  mutate_all(funs(if_else(str_detect(., "Nega'tive"),"Yes, Negative and **",.))) %>%
  mutate_all(funs(if_else(str_detect(., "Stress"),"Yes, Stress",.))) %>%
  mutate_all(funs(if_else(is.na(.),"No",.)))
#and mandated HIV education teaches that, among other behaviors, “homosexual activity” is considered to be “responsible for contact with the AIDS virus


education_content[education_content == "X"] <- 2
education_content[education_content == ""] <- 0
education_content[education_content == "Inclusive"] <- 1
education_content[education_content == "Cover"] <- 1
education_content[education_content == "Nega'tive"] <- -2
education_content[education_content == "Negative"] <- -1
education_content[education_content == "Stress"] <- -1


write_csv(education_content,
          "data/education-content.csv", 
          na = "NA", 
          append = FALSE,
          col_names = TRUE,
          quote_escape = "double")

write_csv(education_content_text,
          "data/education-content-text.csv", 
          na = "NA", 
          append = FALSE,
          col_names = TRUE,
          quote_escape = "double")

#collect life skills requirements table in csv
education_life <- as.data.frame(html_table(tables[3], fill = TRUE))

education_life <- education_life %>%
  rename(State = X1,
         healthy_relationships = X2,
         self_discipline = X3,
         personal_boundaries = X4,
         consent = X5,
         violence_prevention = X6) %>%
  select(-c(X7, X8, X9, X10, X11)) %>%
  tail(-3) %>% 
  head(-2)

education_life_text <- education_life %>%
  rename_with(~ paste(.x, "txt", sep = "_"))
education_life_text[education_life_text == ""] <- NA

education_life_text <- education_life_text %>%
  mutate_all(funs(if_else(str_detect(., "X"),"Yes",.))) %>%
  mutate_all(funs(if_else(is.na(.),"No",.)))

education_life[education_life == "X"] <- 2
education_life[education_life == ""] <- 0


write_csv(education_life,
          "data/education-life.csv", 
          na = "NA", 
          append = FALSE,
          col_names = TRUE,
          quote_escape = "double")

write_csv(education_life_text,
          "data/education-life-text.csv", 
          na = "NA", 
          append = FALSE,
          col_names = TRUE,
          quote_escape = "double")
