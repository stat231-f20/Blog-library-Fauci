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

View(data)

# define vectors for choice values and labels
# for selectInput, needs to be named list
x_choices <- as.list(names(data)[36:38])
x_data_names <- c(names(data)[36:38])
x_choice_names <- c("% Students with Four or more partners",
                    "% Students Intoxicated During Sex",
                    "% Students Used Condom During Last Sexual Intercourse")
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

# ui -----
#For this section, citing this source to get code for different sidebars for different tabs: 
#https://community.rstudio.com/t/different-inputs-sidebars-for-each-tab/1937
ui <- navbarPage(
                          
                          mainPanel(
                            tabsetPanel(type = "tabs", 
                 
                 tabPanel("Relationship Between Different Outcomes",
                          sidebarPanel(
                            selectInput(inputId = "corr_x"
                                        , label = "Choose a student behavior"
                                        , choices = x_choices
                                        , selected = "% Students with Four or more partners"),
                            selectInput(inputId = "corr_y"
                                        , label = "Choose a health outcome"
                                        , choices = y_choices
                                        , selected = "Syphilis")
                          ),
                          
                          mainPanel(
                            #verbatimTextOutput(outputId = "check"),
                            plotlyOutput(outputId = "cor_plot"),
                            tableOutput(outputId = "cor")
                          )
                 ))))

# server  -----
server <- function(input,output){
  
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
