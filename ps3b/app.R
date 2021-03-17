library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(fivethirtyeight)

Data <- fivethirtyeight::mad_men %>%
  mutate(status_correct = case_when(str_detect(status, "End") ~ "END"
                                    , TRUE ~ status))

#remove names of performers that are not string for my scatterplot to work
Data <- Data[-c(16,91,131,188,218), ]

#Bargraph tab1
bar_choice_values <- c("num_lead","num_support",
                       "num_shows")
bar_choice_names <- c("The number of leading roles in films the performer has appeared in",
                      "The number of supporting roles in films the performer has appeared in",
                      "Number of seasons actors appeared in atleast half of the episodes")
names(bar_choice_values) <- bar_choice_names

# for checkboxGroupInput for bargraph
Status_in_show <-  unique(Data$status_correct)

#Table tab2
show_choices <- unique(Data$show)

#scatterplot tab3
yaxis_values <- c("score", "num_shows", "num_support")
yaxis_names <- c("Score", "Number of shows", "Number of support")
names(yaxis_values) <- yaxis_names

# for selectizeInput choices for actor name, pull directly from data
name_choices <- unique(Data$performer)


#ui
ui <- navbarPage(
  
  title="Series and their actors",
  
  #bargraph
  tabPanel(
    title = "Bar Graph",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "barvar"
                    , label = "Choose a variable of interest to plot:"
                    , choices = bar_choice_values
                    , selected = "years_since"),
        checkboxGroupInput(inputId = "show"
                           , label = "Include the type of show:"
                           , choices = Status_in_show
                           , selected = Status_in_show
                           , inline = TRUE)
      ),
      mainPanel(
        plotOutput(outputId = "bar")
      )
    )
  ),
  
  #table
  tabPanel(
    title = "Table",
    
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "Show"
                       , label = "Select show of interest:"
                       , choices = show_choices
                       , selected = "Damages"
                       , multiple = FALSE)
      ),
      mainPanel(
        DT::dataTableOutput(outputId = "table")
      )
    )
  ),
  
  
  #scatterplot
  tabPanel(
    title = "Scatterplot",
    
    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId = "yaxis"
                     , label = "Relationship to the number of leading roles by:"
                     , choices = yaxis_values
                     , selected = "score"),
        selectizeInput(inputId = "id_name"
                       , label = "Identify actors in the scatterplot:"
                       , choices = name_choices
                       , selected = NULL
                       , multiple = TRUE)
      ),
      mainPanel(
        plotOutput(outputId = "scatter")
      )
    )
  )
)


# server   #
server <- function(input,output){
  
  # TAB 1: BARGRAPH
  data_for_bar <- reactive({
    data <- filter(Data, status_correct %in% input$show)
  })
  
  output$bar <- renderPlot({
    ggplot(data = data_for_bar(), aes_string(x = input$barvar)) +
      geom_bar()
  })
  
  # TAB 2: TABLE
  data_for_table <- reactive({
    data <- filter(Data, show %in% input$Show)
  })
  
  output$table <- DT::renderDataTable({ 
    data_for_table()
  })
  
  # TAB 3: SCATTERPLOT
  output$scatter <- renderPlot({
    Data %>%
      
      ggplot(aes_string(x="num_lead", y=input$yaxis)) +
      geom_point(color = "#2c7fb8") +
      labs(x = "Number of leading roles", y = input$yaxis
           , title = "Scatterplot") +
      geom_label_repel(data = filter(Data, performer %in% input$id_name)
                       , aes(label = performer)) +
      facet_grid(~status_correct) 
  })
}

# call to shinyApp #
shinyApp(ui = ui, server = server)


