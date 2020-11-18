library(tidyverse)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)
library(pracma)
library(stringr)
library(shiny) 
#library(readxl) # to load the data into R
library(DT)
library(tools)
library(magrittr)
library(viridis)
library(maps)
library(plotly)
library(rsconnect)
<<<<<<< HEAD
 
data <- read.csv("data/new-project-data.csv") %>%
  mutate(edu_strategies = case_when(
    edu_strategies == "56-94" ~ 75,
    TRUE ~ as.numeric(edu_strategies)
  )) %>%
  mutate(std_edu = case_when(
    std_edu == "48-100" ~ 74,
=======

data <- read.csv("data/new-project-data.csv") %>%
  mutate(edu_strategies = case_when(
    edu_strategies == "56-94" ~ 56,
    TRUE ~ as.numeric(edu_strategies)
  )) %>%
  mutate(std_edu = case_when(
    std_edu == "48-100" ~ 48,
>>>>>>> 093412b62b1fe22c46669266746e3f2e57b89833
    TRUE ~ as.numeric(std_edu)
  ))

View(data)

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

y_choices <- as.list(names(data)[31:38])
y_data_names <- c(names(data)[31:38])
y_choice_names <- c("% schools that provide strategies to Sex Ed teachers",
                    "% of schools that teach STD transmission",
                    "% of schools that teach human sexuality",
                    "% of schools that teach HIV prevention",
                    "% of schools that teach STD prevention",
                    "% students that have had four or more partners",
                    "% students intoxicated during last sexual intercourse")
names(y_choices) <- y_choice_names

ui <- navbarPage("Sexual Education Mandates and Health Outcomes in the United States:",
                 
 tabPanel("School Practices and Student Behavior in States",
          sidebarPanel(
            selectInput(inputId = "y"
<<<<<<< HEAD
                        , label = "Choose a education practice or studentbehavior:"
=======
                        , label = "Choose a practice or behavior:"
>>>>>>> 093412b62b1fe22c46669266746e3f2e57b89833
                        , choices = y_choices
                        , selected = "Education Strategies")
          ),
            selectInput(inputId = "x"
                        , label = "Choose an education mandate to color by:"
                        , choices = x_choices
                        , selected = "Sex_Mandated"),
            
                          
mainPanel(
  tabsetPanel(type = "tabs"
              , tabPanel("Histogram", 
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
    ggplot(use_data(), aes(x = reorder(State, get(input$y)), y = get(input$y), fill = factor(get(paste(input$x))))) +
      geom_bar(stat = "identity") +
      labs(y = y_choice_names[y_choices == input$y],
           title = paste(paste(names(y_choices)[y_choices == input$y]), "by State")) +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) #+
    #scale_fill_distiller(palette = "Set2", name = paste(paste(names(x_choices)[x_choices == input$x])))
  })
}


# call to shinyApp
shinyApp(ui = ui, server = server)
