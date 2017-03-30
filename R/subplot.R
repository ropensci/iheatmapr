

setMethod("make_trace", signature = c(x = "GenericPlot"),
          function(x, xaxes, yaxes, colorbars, colorbar_grid, ...){
            
            xa <- xaxes[[xaxis_name(x)]]
            ya <- yaxes[[yaxis_name(x)]]
            
            trace <- modifyList(get_data(x),
                                list(xaxis = id(xa),
                                     yaxis = id(ya)))
            return(trace)
          })

#' add_subplot
#' 
#' Adds an arbitrary subplot to iheatmap
#' @param p iheatmap object
#' @param ... arguments to pass to plotly trace, see plotly.js documentation at
#' \url{https://plot.ly/javascript/reference/}
#' @param side which side of the current plot to add this heatmap? "right", 
#' "left","top", or "bottom"
#' @param layout axis layout parameters (list)
#' @param size relative size of plot.  size relative to first heatmap
#' @param buffer amount of space to leave empty before this plot, relative to 
#' size of first heatmap
#' @param xname internal name of xaxis
#' @param yname internal name of yaxis
#' @param pname internal name of plot
#' 
#' @seealso \code{\link{iheatmap}} 
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @rdname add_subplot
#' @name add_subplot
#' @aliases add_subplot,Iheatmap-method
#' @author Alicia Schep
#' 
#' mat <- matrix(rnorm(24, ncol = 6))
#' hm <- iheatmap(mat) %>% add_subplot(x = 1:5, y=1:5, side = "top")
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod("add_subplot", c(p = "Iheatmap"),
          function(p,
                   ...,
                   side = c("top","bottom","right","left"),
                   layout = list(),
                   size = 1,
                   buffer = 0.1,
                   xname = if (side %in% c("top","bottom")) 
                     current_xaxis(p) else NULL,
                   yname = if (side %in% c("left","right")) 
                     current_yaxis(p) else NULL,
                   pname = "subplot"){
            side <- match.arg(side)
            if (side %in% c("left","right")){
              p <- add_subplot_horizontal(p,
                                          ..., 
                                          xname = xname,
                                          yname = yname,
                                          size = size, 
                                          buffer = buffer,
                                          side = side,
                                          layout = layout,
                                          pname = pname)
            } else{
              p <- add_subplot_vertical(p,
                                        ..., 
                                        xname = xname,
                                        yname = yname,
                                        size = size, 
                                        buffer = buffer,
                                        side = side,
                                        layout = layout,
                                        pname = pname)
            }
            validObject(p)
            p
            
          })


setMethod("add_subplot_horizontal", c(p = "Iheatmap"),
          function(p,
                   ...,
                   xname = NULL,
                   yname = current_yaxis(p),
                   pname = "subplot",
                   size = 1,
                   buffer = 0.1,
                   side = c("right","left"),
                   layout = list()){
            
            side <- match.arg(side)
            
            new_x <- new_xaxis(p, yname, layout = layout)
            
            if (is.null(xname)) xname <- id(new_x)
            
            new_plot <- new("GenericPlot",
                            xaxis = xname,
                            yaxis = yname,
                            data = list(...))
            
            p <- add_axis(p,
                          new_x,
                           yname = yname,
                           xname = xname,
                           size = size,
                           buffer = buffer,
                           side = side) 
            p <- add_plot(p, new_plot, pname)
            validObject(p)
            p
            
          })


setMethod("add_subplot_vertical", c(p = "Iheatmap"),
          function(p,
                   ...,
                   xname = current_xaxis(p),
                   yname = NULL,
                   pname = "subplot",
                   size = 1,
                   buffer = 0.1,
                   side = c("top","bottom"),
                   layout = list()){
            
            side <- match.arg(side)
            
            new_y <- new_yaxis(p, xname, layout = layout)
            
            if (is.null(yname)) yname <- id(new_y)
            
            new_plot <- new("GenericPlot",
                            xaxis = xname,
                            yaxis = yname,
                            data = list(...))
            
            p <- add_axis(p,
                          new_y,
                           yname = yname,
                           xname = xname,
                           size = size,
                           buffer = buffer,
                           side = side) 
            
            p <- add_plot(p, new_plot, pname)
            validObject(p)
            p
            
          })