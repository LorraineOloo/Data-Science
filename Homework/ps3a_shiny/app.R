#- copy the code from the `02-two-outputs.R` file (in the `two-outputs` code chunk below)
#- add a `textInput` widget that allows the user to change the title of the histogram 
#(following code in `01-two-inputs.R`).  
#Update the code in the server() function appropriately. 
#Run the app to make sure it works as you expect.
#- update the layout of the app to use a `navlistPanel` structure (
#following the code in `06-navlist.R`).  
#Hint: put `navlistPanel` around the output objects only.



library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  textInput(inputId = "title", 
            label = "Write a title",
            value = "Write a title here"),
  verbatimTextOutput("stats"),
  
  navlistPanel( 
    tabPanel(title = "Normal data",
             plotOutput("norm")
    ),
    tabPanel(title = "Uniform data",
             plotOutput("unif")
    ),
    tabPanel(title = "Chi Squared data",
             plotOutput("chisq")
    )
 )
)

server <- function(input, output) {
  
  rv <- reactiveValues(
    norm = rnorm(25), 
    unif = runif(25),
    chisq = rchisq(25, 2))
  
  observeEvent(input$num, { rv$norm <- rnorm(input$num) })
  observeEvent(input$num, { rv$unif <- runif(input$num) })
  observeEvent(input$num, { rv$chisq <- rchisq(input$num, 2) })
  
  output$norm <- renderPlot({
    hist(rv$norm, breaks = 30, col = "grey", border = "white",
         main = input$title)
  })
  output$unif <- renderPlot({
    hist(rv$unif, breaks = 30, col = "grey", border = "white",
         main = input$title)
  })
  output$chisq <- renderPlot({
    hist(rv$chisq, breaks = 30, col = "grey", border = "white",
         main = input$title)
  })
  
  output$stats <- renderPrint({
    summary(rnorm(input$num))
  })
}


shinyApp(server = server, ui = ui)