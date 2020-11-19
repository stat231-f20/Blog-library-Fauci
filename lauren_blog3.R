#x-axis is student behavior, y-axis is health outcome

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

x_choices <- as.list(names(data)[36:38])
x_data_names <- c(names(data)[36:38])
x_choice_names <- c("% of students that have had 4+ sexual partners",
                    "% of students intoxicated in last sexual intercourse",
                    "% of students that used condom in last sexual intercourse")
names(x_choices) <- x_choice_names

y_choices <- as.list(names(data)[23:30])
y_data_names <- c(names(data)[23:30])
y_choice_names <- c("Chlamydia Per 100K",
                    "Syphilis Per 100K",
                    "Gonorrhea Per 100K",
                    "Married Household %",
                    "% HS Grad or Higher",
                    "STD Index",
                    "Births Per 1K Girls",
                    "HIV Per 100K")
names(y_choices) <- y_choice_names

ui <- navbarPage("Sexual Education Mandates and Health Outcomes in the United States:",
                 
                 tabPanel("Mandate vs. Outcome By State",
                          sidebarPanel(
                            selectInput(inputId = "x"
                                        , label = "Choose a student behavior:"
                                        , choices = x_choices
                                        , selected = "% of students that have had 4+ sexual partners"),
                            selectInput(inputId = "y"
                                        , label = "Choose a health outcome:"
                                        , choices = y_choices
                                        , selected = "Chlamydia Per 100K")
                          ),
                          
                          mainPanel(
                            tabsetPanel(type = "tabs"
                                        , tabPanel("Scatterplot", 
                                                   plotlyOutput(outputId = "bar")
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
  
  
  #Hist Tab  -------
  output$bar <- renderPlotly({
    ggplot(use_data(), aes(x = reorder(get(input$y)), get(input$x), y = get(input$y), x = get(input$x))) +
      geom_point(stat = "identity") +
      labs(y = y_choice_names[y_choices == input$y],
           title = paste(paste(names(y_choices)[y_choices == input$y]), "by State")) +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) #+
    #scale_fill_distiller(palette = "Set2", name = paste(paste(names(x_choices)[x_choices == input$x])))
  })
}


# call to shinyApp
shinyApp(ui = ui, server = server)