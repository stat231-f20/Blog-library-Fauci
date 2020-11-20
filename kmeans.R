#last minute attempt, not functioning, please ignore
#are there groupings for which states require sex ed?
#Code from: Aditi

library(tidyverse)
library(mdsr)
library(Lahman)
library(GGally)

set.seed(75)

clustering <- read.csv("data/new-project-data.csv") %>%
  mutate(edu_strategies = case_when(
    edu_strategies == "56-94" ~ 75,
    TRUE ~ as.numeric(edu_strategies)
    )) %>%
  mutate(std_edu = case_when(
    std_edu == "48-100" ~ 74,
    TRUE ~ as.numeric(std_edu)
    )) %>%
  select("State", "HIV_Mandated", "HIV_Condoms", "HIV_Abstinence", "HIV") %>%
  drop_na() 


#determine number of clusters
fig <- matrix(NA, nrow=10, ncol=2)

for (i in 1:10){
  fig[i,1] <- i
  fig[i,2] <- kmeans(clustering[,2:5]
                     , centers=i
                     , nstart=20)$tot.withinss
}

ggplot(data = as.data.frame(fig), aes(x = V1, y = V2)) +
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks=c(1:10)) +
  labs(x = "K", y = expression("Total W"[k]))

#standardize variables
vars_std <- c("Mandated_std", "Condoms_std", "Abstinence_std", "cases_std")
km3_out_std <- kmeans(clustering[,vars_std]
                      , centers=3
                      , nstart=20)

#add cluster assignments to dataframe
final_clust <- clustering %>%
  mutate(clust3_std = as.character(km3_out_std$cluster)) %>%
  arrange(clust3_std)

#extrapolate meaning from clusters
ggpairs(data = final_clust
        , aes(color = clust3_std) 
        , columns = vars_std
        , upper = list(continuous = "blank"))