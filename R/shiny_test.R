
#' test_iheatmapr_event
#'
#' @param ihm Iheatmap object
#' @param event name of event, either "click","hover", or "relayout"
#'
#' @return shiny app
#' @export
#'
#' @examples
#' 
#' \dontrun{
#'   mat <- matrix(rnorm(20), ncol = 5, nrow = 4)  
#'   hm <- main_heatmap(mat) 
#'   test_iheatmapr_event(hm, "click")
#' }
test_iheatmapr_event <- function(ihm, event = c("click","hover","relayout")){
  
  if (!requireNamespace("shiny",quietly = TRUE))
    stop("shiny not installed")
  event <- match.arg(event)
  
  # Define UI
  ui <- shiny::fluidPage(
    
    shiny::titlePanel("iheatmapr event example"),
    
    shiny::fluidRow(
      shiny::column(8, iheatmaprOutput("heat")), 
      shiny::column(4, shiny::verbatimTextOutput("cl"))
    )
  )
  
  # Define server logic 
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