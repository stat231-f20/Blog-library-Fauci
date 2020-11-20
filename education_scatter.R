#state education mandate vs school resources
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

definitions <- read.csv("data/practices_definitions.csv")

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
x_choices <- as.list(names(data)[2:22])
x_data_names <- c(names(data)[2:22])
x_choice_names <- c("Sex Ed Mandated",
                    "HIV Ed Mandated",
                    "Medically Accurate",
                    "Age Appropriate",
                    "Culturally Appropriate/Unbiased",
                    "Cannot Promote Religion",
                    "Parents Notified",
                    "Parental Consent",
                    "Opt-Out",
                    "Sex Ed Contraception",
                    "Sex Ed Abstinence",
                    "Sex Ed Marriage",
                    "Sex Ed Orientation",
                    "Sex Ed Negative Outcomes",
                    "HIV Ed Condoms",
                    "HIV Ed Abstinence",
                    "Healthy Relationships",
                    "Sexual decision-making and self-discipline",
                    "Refusal skills and personal boundaries",
                    "Consent",
                    "Dating and sexual violence prevention")
names(x_choices) <- x_choice_names
print(x_choices)

y_choices <- as.list(names(data)[31:35])
y_data_names <- c(names(data)[31:35])
y_choice_names <- c("Provided Educators with Strategies",
                    "Taught how STDs are Transmitted",
                    "Increased Student Knowledge on Sexuality",
                    "Increased Student Knowledge on HIV Prevention",
                    "Increased Student Knowledge on STD Prevention")
names(y_choices) <- y_choice_names

ui <- navbarPage("Sexual Education Mandates and Health Outcomes in the United States:",
                 
                 tabPanel("Mandate vs. Outcome By State",
                          sidebarPanel(
                            selectInput(inputId = "x"
                                        , label = "Choose an education mandate of interest:"
                                        , choices = x_choices
                                        , selected = "HIV_Mandated"),
                            selectInput(inputId = "y"
                                        , label = "Choose resource offered by high schools:"
                                        , choices = y_choices
                                        , selected = "HIV.Prevention")
                          ),
                          
                          mainPanel(
                            tabsetPanel(type = "tabs"
                                        , tabPanel("Scatterplot", 
                                                   verbatimTextOutput(outputId = "check"),
                                                   tableOutput(outputId = "table"),
                                                   plotOutput(outputId = "scatter")
                                                   
                                                   #,tags$h6("* Yes, but only if county pregnancy rate is at least 19.5
                                                          #or higher per 1K for girls aged 15—17")   
                                                   #,tags$h6("** Yes, Negative and mandated HIV education teaches that,
                                                          #among other behaviors, “homosexual activity” is considered
                                                          #to be “responsible for contact with the AIDS virus")     
                                        )))))

server <- function(input,output){
  
  use_data <- reactive({
    data <- data %>%
      select(State, input$x, input$y)  %>%
      left_join(gen_ed_txt, by = "State") %>%
      left_join(content_ed_txt, by = "State") %>%
      left_join(life_ed_txt, by = "State")
  })
  
  definitions_reactive <- reactive({
    definitions <- filter(definitions, String == input$y) %>%
      select(Variable, Definition)
  })
  
  
  #Scatter Tab  -------
  
  output$scatter <- renderPlot({
      ggplot(data = use_data(), aes(x = get(input$x), y = get(input$y), color = factor(get(input$x)))) +
        geom_point() +
        theme(legend.position="bottom") +
        labs(x = names(x_choices)[x_choices == paste(input$x, "txt", sep = "_")],
             y = names(y_choices)[y_choices == input$y],
             title = paste(paste(names(y_choices)[y_choices == input$y]),
                           "Compared to", 
                           paste(names(x_choices)[x_choices == input$x]),
                           "as Statewide Requirement"), 
             color = "Mandate Level")
  })
  
  output$table <- renderTable({
    definitions_reactive()
  })
  
}

# call to shinyApp
shinyApp(ui = ui, server = server)