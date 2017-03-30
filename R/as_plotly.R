#' as_plotly
#' 
#' Function to convert \code{link{Iheatmap-class}} object to plotly widget object
#' 
#' @param p \code{\link{Iheatmap-class}} object to convert
#' @param source source name for plot, for use in shiny
#' @return plotly htmlwidgets object
#' @seealso \code{\link{iheatmap}}, \code{\link{main_heatmap}}
#' @export
#' @rdname as_plotly
#' @name as_plotly
#' @aliases as_plotly,Iheatmap-method
#' @author Alicia Schep
#' @importFrom htmlwidgets sizingPolicy createWidget
#' @importFrom jsonlite toJSON
#' @examples 
#' 
#' mat <- matrix(rnorm(24), nrow = 6)
#' hm <- iheatmap(mat) %>% as_plotly()
#' class(hm)
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod(as_plotly,
          signature = c("Iheatmap"),
          function(p, source = "Iheatmap"){
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
                        source = source,
                        config = list(modeBarButtonsToRemove = 
                                        c("sendDataToCloud",
                                          "autoScale2d")))
            attr(out, "TOJSON_FUNC") <- function(x, ...) {
              toJSON(x, digits = 50, auto_unbox = TRUE, force = TRUE,
                     null = "null", na = "null", ...)
            }
            createWidget(name = "plotly",
                         x = out,
                         width = out$layout$width,
                         height = out$layout$height,
                         sizingPolicy = sizingPolicy(browser.fill = TRUE,
                                                     defaultWidth = "100%",
                                                     defaultHeight = 400),
                         preRenderHook = plotly_build)
          })


setMethod("show", "Iheatmap",
          function(object){
            print(as_plotly(object))
          })

#' knit_print.Iheatmap
#' 
#' @param x Iheatmap object
#' @param options knitr options
#' @keywords internal
#' @export
#' @importFrom knitr knit_print
knit_print.Iheatmap <- function(x, options){
  knit_print(as_plotly(x), options = options)
}


#' save_iheatmap
#' 
#' save an \code{link{Iheatmap-class}} object, either as standalone HTML or as static 
#' pdf/png/jpeg
#' @param p \code{link{Iheatmap-class}} object
#' @param filename name of file
#' @param ... additional arguments to \code{\link[htmlwidgets]{saveWidget}} for 
#' saving as html or \code{\link[plotly]{export}} for saving as pdf/png/jpeg
#' @export
#' @rdname save_iheatmap
#' @name save_iheatmap
#' @aliases save_iheatmap,Iheatmap,character-method
#' @importFrom plotly export
#' @importFrom htmlwidgets saveWidget
#' @author Alicia Schep
setMethod(save_iheatmap, c("Iheatmap","character"),
          function(p, filename, ...){
  if (grepl(".html$", filename)){
    saveWidget(as_plotly(p), filename, ...)
  } else{
    export(as_plotly(p) , filename, ...)
  }
})
