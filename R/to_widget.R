to_plotly_list <- function(p){
  traces <- unname(lapply(p@plots,
                          make_trace,
                          xaxes = xaxes(p),
                          yaxes = yaxes(p),
                          colorbars = p@colorbars,
                          colorbar_grid = p@colorbar_grid))
  shapes <- unlist(unname(lapply(p@shapes,
                                 make_shapes,
                                 xaxes = xaxes(p),
                                 yaxes = yaxes(p))),
                   recursive = FALSE, use.names = FALSE)
  annotations <- unlist(unname(lapply(p@annotations,
                                      make_annotations,
                                      xaxes = xaxes(p),
                                      yaxes = yaxes(p))),
                        recursive = FALSE)
  layout_setting <- c(get_layout(p@xaxes),
                      get_layout(p@yaxes),
                      p@layout)
  if (length(shapes) && !is.null(unlist(shapes))){
    layout_setting$shapes <- shapes
  }
  if (length(annotations) && !is.null(unlist(annotations))){
    layout_setting$annotations <- annotations
  }
  if (is.null(layout_setting$legend$x)){
    layout_setting$legend$x <- get_legend_position(p)
    layout_setting$legend$xanchor <- "left"
  }
  out <- list(data = traces,
              layout = layout_setting,
              source = p@source,
              config = list(modeBarButtonsToRemove = 
                              c("sendDataToCloud",
                                "autoScale2d")))
  attr(out, "TOJSON_FUNC") <- function(x, ...) {
    toJSON(x, digits = 50, auto_unbox = TRUE, force = TRUE,
           null = "null", na = "null", ...)
  }
  out
}

#' to_widget
#' 
#' Function to convert \code{link{Iheatmap-class}} object to widget object
#' 
#' @param p \code{\link{Iheatmap-class}} object to convert
#' @return htmlwidgets object
#' @seealso \code{\link{iheatmap}}, \code{\link{main_heatmap}}
#' @export
#' @rdname to_widget
#' @name to_widget
#' @aliases to_widget,Iheatmap-method
#' @author Alicia Schep
#' @importFrom htmlwidgets sizingPolicy createWidget
#' @importFrom jsonlite toJSON
#' @examples 
#' 
#' mat <- matrix(rnorm(24), nrow = 6)
#' hm <- iheatmap(mat) %>% to_widget()
#' class(hm)
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod(to_widget,
          signature = c("Iheatmap"),
          function(p){
            out <- to_plotly_list(p)
            createWidget(name = "iheatmapr",
                         x = out,
                         width = out$layout$width,
                         height = out$layout$height,
                         sizingPolicy = sizingPolicy(browser.fill = TRUE,
                                                     defaultWidth = "100%",
                                                     defaultHeight = 400))
          })



setMethod("show", "Iheatmap",
          function(object){
            print(to_widget(object))
          })

#' knit_print.Iheatmap
#' 
#' @param x Iheatmap object
#' @param options knitr options
#' @keywords internal
#' @export
#' @importFrom knitr knit_print
knit_print.Iheatmap <- function(x, options){
  knit_print(to_widget(x), options = options)
}


#' save_iheatmap
#' 
#' save an \code{link{Iheatmap-class}} object, either as standalone HTML or as static 
#' pdf/png/jpeg
#' @param p \code{link{Iheatmap-class}} object
#' @param filename name of file
#' @param ... additional arguments to \code{\link[htmlwidgets]{saveWidget}} for 
#' saving as html or \code{\link[webshot]{webshot}} for saving as pdf/png/jpeg
#' @export
#' @rdname save_iheatmap
#' @name save_iheatmap
#' @aliases save_iheatmap,Iheatmap,character-method
#' @importFrom htmlwidgets saveWidget
#' @author Alicia Schep
#' @examples
#' mat <- matrix(rnorm(24), nrow = 6)
#' hm <- iheatmap(mat)
#' \dontrun{
#' save_iheatmap(hm)
#' }
setMethod(save_iheatmap, c("Iheatmap","character"),
          function(p, filename, ...){
            
            fileType <- tolower(tools::file_ext(filename))
            if (!fileType %in% c('jpeg', 'png', 'html','pdf')) {
              stop("File type ", fileType, " not supported", call. = FALSE)
            }          
            if (fileType == "html"){
              saveWidget(to_widget(p), filename, ...)
            } else{
              if (!requireNamespace("webshot",quietly = TRUE))
                stop('Please install the webshot package for saving static plot')
              f <- basename(tempfile('iheatmapr', '.', '.html'))
              on.exit(unlink(f), add = TRUE)
              html <- saveWidget(to_widget(p), f, selfcontained = TRUE)
              webshot::webshot(f, filename, ...)
            }
          })




