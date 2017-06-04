
test_shiny_event <- function(ihm, event = c("click","hover","selected","relayout")){
  
  if (!requireNamespace("shiny",quietly = TRUE))
    stop("shiny not installed")
  event <- match.arg(event)
  
  ui <- shiny::fluidPage(
    
    # Application title
    shiny::titlePanel("Heatmap Example"),
    
    shiny::fluidRow(
      shiny::column(8, iheatmaprOutput("heat")), 
      shiny::column(4, shiny::verbatimTextOutput("cl"))
    )
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
    output$heat <- renderIheatmap({
      ihm
    })
    
    output$cl <- shiny::renderPrint({
      s <- iheatmapr_event(ihm,event)
      if (is.null(s) == TRUE)
        return(paste0(event, " to see output"))
      utils::str(s)
    })
    
  }
  
  # Run the application 
  shiny::shinyApp(ui = ui, server = server)
  
}