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


# setup -----
data <- read_csv("data/project_data.csv")
states_loc <-  map_data(map = "state"
                        , region = ".")

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

y_choices <- as.list(names(data)[36:38])
y_data_names <- c(names(data)[36:38])
y_choice_names <- c("four",
                    "intoxicated",
                    'condoms')
names(y_choices) <- y_choice_names

# ui -----
#For this section, citing this source to get code for different sidebars for different tabs: 
#https://community.rstudio.com/t/different-inputs-sidebars-for-each-tab/1937
ui <- navbarPage(
                 
       tabPanel("Relationship Between Different Outcomes",
                  sidebarPanel(
                            selectInput(inputId = "corr_x"
                                        , label = "Choose an outcome of interest"
                                        , choices = y_choices
                                        , selected = "Chlamydia"),
                            selectInput(inputId = "corr_y"
                                        , label = "Choose another outcome of interest"
                                        , choices = y_choices
                                        , selected = "Syphilis")
                          )),
                          
                          mainPanel(
                            #verbatimTextOutput(outputId = "check"),
                            plotlyOutput(outputId = "cor_plot"),
                            tableOutput(outputId = "cor")
                          )
            )
                 

# server  -----
server <- function(input,output){
  
  use_data <- reactive({
    data <- data %>%
      select(State, input$x, input$y) %>%
      left_join(gen_ed_txt, by = "State") %>%
      left_join(content_ed_txt, by = "State") %>%
      left_join(life_ed_txt, by = "State")
    
    
  })
  
  
  #Correlation Tab -------
  in_data <- reactive({ data %>% 
      select(y_data_names)})
  
  output$check <- renderPrint({input$corr_x })
  
  output$cor_plot <- renderPlotly({
    ggplot(data = in_data(), aes_string(x = input$corr_x, y = input$corr_y)) +
      geom_point() #+ 
    # labs(x = names(x_choices)[x_choices == input$x]
    #      , y = y_choice_names[y_choice_values == input$y]) +
    # geom_label(data = filter(hate_crimes2, state_abbrev == input$id_st)
    #            , aes(label = state_abbrev))
  })
  
  cor_data <- reactive({
    cor(in_data()) %>%
      as.table() %>%
      as.data.frame(stringsAsFactors = F) %>%
      filter(str_detect(Var2, input$corr_x)) %>%
      pivot_wider(names_from = Var1, values_from = Freq) %>%
      mutate(" " = paste("Correlation with ", input$corr_x))
  })
  
  output$cor <- renderTable({
    dplyr::select(cor_data(), " ", y_data_names)
    #in_data()
  })
  
}

# call to shinyApp
shinyApp(ui = ui, server = server)
