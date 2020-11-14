library(tidyverse)

STDs <- read_csv("data/STDs.csv")

teen_birth <- read_csv("data/teen-birth.csv")

hiv_data <- read_csv("data/hiv_data.csv", skip = 9) %>%
  select("State" = "Geography", "HIV" = "Rate per 100000")

#Joining the dataframes, eliminating unneeded columns. 
#Note: no data available for STDs on Washington DC
joined <- inner_join(STDs, teen_birth, by = "State") %>%
  rename("LYR" = "Last year's rank") %>%
  select(-c(X1.x, Rank.x, X1.y, Rank.y, LYR))


gen_ed <- read_csv("data/education-gen.csv")

gen_ed[15,1] <- "Indiana"
gen_ed[25,1] <- "Mississippi"
gen_ed[43,1] <- "Utah"

content_ed <- read_csv("data/education-content.csv")

content_ed[22,1] <- "Mississippi"
content_ed[39,1] <- "Utah"

life_ed <- read_csv("data/education-life.csv")



#Initial wrangling for Gutmacher done in scraping file
#Joining education requirement data frames
education <- gen_ed %>%
  full_join(content_ed, by = "State") %>%
  full_join(life_ed, by = "State")


#Joining the dataframes, eliminating unneeded columns. 
#Note: no data available for STDs on Washington DC
health <- inner_join(STDs, teen_birth, by = "State") %>% 
  inner_join(hiv_data, by = "State") %>%
  rename("LYR" = "Last year's rank") %>%
  select(-c(X1.x, Rank.x, X1.y, Rank.y, LYR))

health_names <- c("State", 
                  "Chlamydia",
                  "Syphilis",
                  "Gonorrhea",
                  "Married", 
                  "Grad", 
                  "STD", 
                  "Birth",
                  "HIV")
colnames(health) <- health_names


joinedAL <- full_join(education, health, by = "State") %>%
  filter(State != "Dist. of Columbia")

out_path <- "data"
write_csv(x = joinedAL, path = paste0(out_path,"/project_data.csv"))


