#compare what high schools actually do to health outcomes
#Code From: Aditi
library(tidyverse)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)
library(pracma)
library(stringr)
library(shiny) 
library(DT)
library(tools)
library(magrittr)
library(viridis)
library(maps)
library(plotly)

data <- read.csv("data/new-project-data.csv") %>%
  mutate(edu_strategies = case_when(
    edu_strategies == "56-94" ~ 75,
    TRUE ~ as.numeric(edu_strategies)
  )) %>%
  mutate(std_edu = case_when(
    std_edu == "48-100" ~ 74,
    TRUE ~ as.numeric(std_edu)
  ))

gen_ed_txt <- read_csv("data/education-gen-text.csv") %>%
  rename(State = State_txt)

gen_ed_txt[15,1] <- "Indiana"
gen_ed_txt[25,1] <- "Mississippi"
gen_ed_txt[43,1] <- "Utah"

content_ed_txt <- read_csv("data/education-content-text.csv") %>%
  rename(State = State_txt)

content_ed_txt[22,1] <- "Mississippi"
content_ed_txt[39,1] <- "Utah"

life_ed_txt <- read_csv("data/education-life-text.csv") %>%
  rename(State = State_txt)

# define vectors for choice values and labels
# for selectInput, needs to be named list
x_choices <- as.list(names(data)[31:35])
x_data_names <- c(names(data)[31:35])
x_choice_names <- c("Provided Educators with Strategies",
                    "Taught how STDs are Transmitted",
                    "Increased Student Knowledge on Sexuality",
                    "Increased Student Knowledge on HIV Prevention",
                    "Increased Student Knowledge on STD Prevention")
names(x_choices) <- x_choice_names
print(x_choices)

y_choices <- as.list(names(data)[23:30])
y_choices <- append(y_choices, as.list(names(data)[36:38]), after = 8)
y_data_names <- c(names(data)[23:30])
y_data_names <- append(y_data_names, c(names(data)[36:38]), after = 8)
y_choice_names <- c("Chlamydia Per 100K",
                    "Syphilis Per 100K",
                    "Gonorrhea Per 100K",
                    "Married Household %",
                    "% HS Grad or Higher",
                    "STD Index",
                    "Births Per 1K Girls",
                    "HIV Per 100K",
                    "% Students with Four or more partners",
                    "% Students Intoxicated During Sex",
                    "% Students Used Condom During Last Sexual Intercourse")
names(y_choices) <- y_choice_names

ui <- navbarPage(
                 
                 tabPanel("School policies, teacher effort, and health outcomes",
                          sidebarPanel(
                            selectInput(inputId = "x"
                                        , label = "Choose resource offered by high schools:"
                                        , choices = x_choices
                                        , selected = "HIV.Prevention"),
                            selectInput(inputId = "y"
                                        , label = "Choose a health outcome or student behavior:"
                                        , choices = y_choices
                                        , selected = "HIV")
                          ),
                          
                          mainPanel(
                            tabsetPanel(type = "tabs"
                                        , tabPanel("Scatterplot", 
                                                   plotOutput(outputId = "scatter")
                                                   ,tags$h6("* Yes, but only if county pregnancy rate is at least 19.5
                                                          or higher per 1K for girls aged 15—17")   
                                                   ,tags$h6("** Yes, Negative and mandated HIV education teaches that,
                                                          among other behaviors, “homosexual activity” is considered
                                                          to be “responsible for contact with the AIDS virus")     
                                        )))))

server <- function(input,output){
  
  use_data <- reactive({
    data <- data %>%
      select(State, input$x, input$y)  %>%
      left_join(gen_ed_txt, by = "State") %>%
      left_join(content_ed_txt, by = "State") %>%
      left_join(life_ed_txt, by = "State")
  })
  
  
  #Scatter Tab  -------
  
  output$scatter <- renderPlot({
      
      ggplot(data = use_data(), aes(x = get(input$x), y = get(input$y))) +
        geom_point() +
        geom_smooth(method = lm, se = TRUE, fullrange = TRUE) +
        theme(legend.position="bottom") +
        labs(x = paste("% Schools that",
                       names(x_choices)[x_choices == input$x],
                       sep = " "),
             y = names(y_choices)[y_choices == input$y],
             title = paste(paste(names(y_choices)[y_choices == input$y]),
                           "Compared to % Secondary Schools that", 
                           paste(names(x_choices)[x_choices == input$x])))
  })
}

# call to shinyApp
shinyApp(ui = ui, server = server)