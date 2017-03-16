
test_shiny_click <- function(ihm){
  
  if (!requireNamespace("shiny",quietly = TRUE))
    stop("shiny not installed")
  
  ui <- shiny::fluidPage(
    
    # Application title
    shiny::titlePanel("Heatmap Example"),
    
    shiny::fluidRow(
      shiny::column(8, plotly::plotlyOutput("heat")),
      shiny::column(4, shiny::verbatimTextOutput("cl"))
    )
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
    output$heat <- plotly::renderPlotly({
      as_plotly(ihm, source = "iheatmap")
    })
    
    output$cl <- shiny::renderPrint({
      s <- plotly::event_data("plotly_click", source="iheatmap")
      if (is.null(s) == T) return("Click to see output")
      utils::str(s)
    })
    
  }
  
  # Run the application 
  shiny::shinyApp(ui = ui, server = server)
  
}