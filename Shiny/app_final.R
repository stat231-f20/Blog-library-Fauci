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

map_data <- data %>%
    left_join(gen_ed_txt, by = "State") %>%
    left_join(content_ed_txt, by = "State") %>%
    left_join(life_ed_txt, by = "State") %>%
    mutate(state_lower = tolower(State)) %>%
    right_join(states_loc, by = c("state_lower" = "region")) #%>%
    #filter(!is.na(Syphilis))



# ggplot(map_data, aes_string(x = "long", y = "lat", group = "group"
#                             , fill = "Syphilis")) +
#     geom_polygon(data = states_loc, aes(x = long, y = lat, group = group)
#                  , color = "white", fill = "grey") +
#     geom_polygon(color = "white") +
#     theme_void() +
#     coord_fixed(ratio = 1.3) +
#     #labs(fill = y_choice_names[y_choices == input$y]) +
#     theme(legend.position="bottom") +
#     scale_fill_viridis(option = "magma", direction = -1)+
#     facet_wrap(~HIV_Mandated, ncol = 1)


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

# ui -----
#For this section, citing this source to get code for different sidebars for different tabs: 
#https://community.rstudio.com/t/different-inputs-sidebars-for-each-tab/1937
ui <- navbarPage("Sexual Education Mandates and Health Outcomes in the United States:",
                 # tabPanel("Introduction",
                 #          mainPanel(
                 #              )
                 #          
                 # ),
                          
                 tabPanel("Mandate vs. Outcome By State",
                          sidebarPanel(
                              selectInput(inputId = "x"
                                          , label = "Choose an education mandate of interest:"
                                          , choices = x_choices
                                          , selected = "Sex_Mandated"),
                              selectInput(inputId = "y"
                                          , label = "Choose an outcome of interest:"
                                          , choices = y_choices
                                          , selected = "Chlamydia")
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
                                                     )
                                          , tabPanel("Histogram", 
                                                     plotlyOutput(outputId = "bar")
                                                     ,tags$h6("* Yes, but only if county pregnancy rate is at least 19.5
                                                          or higher per 1K for girls aged 15—17")   
                                                     ,tags$h6("** Yes, Negative and mandated HIV education teaches that,
                                                          among other behaviors, “homosexual activity” is considered
                                                          to be “responsible for contact with the AIDS virus")     
                                                     )
                                          , tabPanel("Table", 
                                                     dataTableOutput(outputId = "table")
                                                     ,tags$h6("* Yes, but only if county pregnancy rate is at least 19.5
                                                          or higher per 1K for girls aged 15—17")   
                                                     ,tags$h6("** Yes, Negative and mandated HIV education teaches that,
                                                          among other behaviors, “homosexual activity” is considered
                                                          to be “responsible for contact with the AIDS virus")     
                                                     )
                                          , tabPanel("Map", 
                                                     tags$h6("* Yes, but only if county pregnancy rate is at least 19.5
                                                          or higher per 1K for girls aged 15—17")   
                                                     ,tags$h6("** Yes, Negative and mandated HIV education teaches that,
                                                          among other behaviors, “homosexual activity” is considered
                                                          to be “responsible for contact with the AIDS virus") 
                                                     ,plotOutput(outputId = "map",  width = "100%")
                                                     
                                                     )
                                               
                          ))
                          
                 ),

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
                          ),
                          
                          mainPanel(
                              #verbatimTextOutput(outputId = "check"),
                              plotlyOutput(outputId = "cor_plot"),
                              tableOutput(outputId = "cor")
                          )
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
    
    #Scatter Tab  -------
    
    output$scatter <- renderPlot({
        ggplot(data = use_data(), aes(x = get(input$x), y = get(input$y), color = factor(get(paste(input$x, "txt", sep = "_"))))) +
            geom_point() +
            theme(legend.position="bottom") +
            labs(x = names(x_choices)[x_choices == paste(input$x, "txt", sep = "_")],
                 y = names(y_choices)[y_choices == input$y],
                 title = paste(paste(names(y_choices)[y_choices == input$y]),
                               "Compared to", 
                               paste(names(x_choices)[x_choices == input$x]),
                               "as Statewide Requirement"), color = "Mandate Level")
            
          
    })
    
    #Hist Tab  -------
    output$bar <- renderPlotly({
        ggplot(use_data(), aes(x = reorder(State, get(input$y)), y = get(input$y), fill = factor(get(paste(input$x, "txt", sep = "_"))))) +
            geom_bar(stat = "identity") +
            labs(y = y_choice_names[y_choices == input$y],
                 title = paste(paste(names(y_choices)[y_choices == input$y]), "by State")) +
            theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
                  axis.ticks.x=element_blank()) #+
            #scale_fill_distiller(palette = "Set2", name = paste(paste(names(x_choices)[x_choices == input$x])))
    })
    
    output$table <- renderDataTable({
        select(use_data(), State, input$y, paste(input$x, "txt", sep = "_"))
    })
    #Location Tab  -------
    
    output$map <- renderPlot({
        ggplot(map_data, aes_string(x = "long", y = "lat", group = "group"
                                    , fill = input$y)) +
            geom_polygon(data = states_loc, aes(x = long, y = lat, group = group)
                         , color = "white", fill = "grey") +
            geom_polygon(color = "white") +
            theme_void() +
            coord_fixed(ratio = 1.3) +
            #labs(fill = y_choice_names[y_choices == input$y]) +
            theme(legend.position="top") +
            scale_fill_viridis(option = "magma", direction = -1)+
            facet_wrap(~get(paste(input$x, "txt", sep = "_")), ncol = 1)
        }, height = 1500, width = 800
    )
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
