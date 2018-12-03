#' Shiny bindings for iheatmapr
#' 
#' Output and render functions for using iheatmapr within Shiny 
#' 
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This 
#'   is useful if you want to save an expression in a variable.
#'   
#' @importFrom htmlwidgets shinyWidgetOutput shinyRenderWidget
#' @name iheatmapr-shiny
#'
#' @export
iheatmaprOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "iheatmapr", width, height, 
                                 package = "iheatmapr")
}

#' @param expr An expression that generates an Iheatmap object
#' @param env The environment in which to evaluate \code{expr}.
#' @rdname iheatmapr-shiny
#' @export
renderIheatmap <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  expr <- call("to_widget", expr)
  htmlwidgets::shinyRenderWidget(expr, iheatmaprOutput, env, quoted = TRUE)
}


#' Access iheatmapr user input event data in shiny
#' 
#' This function must be called within a reactive shiny context.
#' 
#' @param object \code{\link{Iheatmap-class}} object
#' @param event The type of plotly event. Currently 'plotly_hover',
#' 'plotly_click', 'plotly_selected', and 'plotly_relayout' are supported.
#' @param session a shiny session object (the default should almost always be used).
#' @export
#' @examples \dontrun{
#' shiny::runApp(system.file("examples", "shiny_example", package = "iheatmapr"))
#' }
iheatmapr_event <- function(object,
                            event = c("hover", "click", "relayout"), 
                            session = shiny::getDefaultReactiveDomain()) 
{
  if (is.null(session)) {
    stop("No reactive domain detected. This function can only be called \n", 
         "from within a reactive shiny context.")
  }
  source <- object@source
  event <- match.arg(event)
  event <- paste0("iheatmapr_", event)
  src <- sprintf(".clientValue-%s-%s", event, source)
  val <- session$rootScope()$input[[src]]
  if (is.null(val)) {
    out <- val
  } else if (event == "iheatmapr_hover" || event == "iheatmapr_click"){
    raw <- jsonlite::fromJSON(val)
    out <- list(raw = raw)
    curve <- names(plots(object))[raw$curveNumber + 1]
    xname <- xaxis_name(plots(object)[[curve]])
    yname <- yaxis_name(plots(object)[[curve]])
    if (is(xaxes(object)[[xname]], "IheatmapMainAxis")){
      co <- axis_order(xaxes(object)[[xname]])
      out$col <- co[raw$x]
    } else{
      out$x <- raw$x
    }
    if (is(yaxes(object)[[yname]], "IheatmapMainAxis")){
      ro <- axis_order(yaxes(object)[[yname]])
      out$row <- ro[raw$y]
    } else{
      out$y <- raw$y
    }
    if (is(plots(object)[[curve]],"MainHeatmap")){
      out$value <- raw$z
    } 
  } else if (event == "iheatmapr_relayout"){
    out <- jsonlite::fromJSON(val)
    #out <- list(raw = raw)
  } 
  return(out)
}

