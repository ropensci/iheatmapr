#' add_col_barplot
#' 
#' Add bar plot with one bar per column above or below a main heatmap
#' @param p iheatmap object
#' @param y y axis values
#' @param ... additional arguments to add to plotly scatter trace, see
#' \url{https://plot.ly/javascript/reference/#scatter}
#' @param color color of bars
#' @param tracename name of trace (for legend and hover)
#' @param showlegend show in legend?
#' @param side side of plot on which to add subplot
#' @param layout yaxis layout list
#' @param size relative size of subplot relative to main heatmap
#' @param buffer amount of space to leave empty before this plot, relative to 
#' size of first heatmap
#' @param xname internal name of xaxis
#' @param yname internal name of yaxis
#' @param pname internal name of plot
#' 
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @rdname add_col_barplot
#' @name add_col_barplot
#' @aliases add_col_barplot,Iheatmap-method
#' @seealso \code{\link{add_col_signal}}, \code{\link{iheatmap}}, 
#' \code{\link{add_col_plot}}
#' @author Alicia Schep
#' @examples 
#' 
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)  
#' hm <- iheatmap(mat) %>% add_col_barplot(y = 1:5, tracename = "Strength")
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod(add_col_barplot, c(p = "Iheatmap"),
          function(p,
                   y,
                   ...,
                   color = NULL, 
                   tracename = NA_character_,
                   showlegend = !is.na(tracename),
                   side = c("top","bottom"), 
                   layout = list(),
                   size = 0.2,
                   buffer = 0.02,
                   xname = current_xaxis(p),
                   yname = NULL,
                   pname = if (!is.na(tracename)) tracename else "col_barplot"){
            
            side <- match.arg(side)
            
            additional_data <- modifyList(list(orientation = 'v'), list(...))
            
            if (is.null(color) && !isTRUE(showlegend)){
              color = "gray"
            }
            if (!is.null(color)){
              color_list = list(marker = list(color = color))
              additional_data <- modifyList(color_list, additional_data)
            }
            
            new_y <- new_yaxis(p, xname, layout = layout)
            
            if (is.null(yname)) yname <- id(new_y)
            
            new_plot <- new("ColumnPlot",
                            xaxis = xname,
                            yaxis = yname,
                            data = y,
                            showlegend = showlegend,
                            type = "bar",
                            name = tracename,
                            additional = additional_data)
            
            p <- add_axis(p,
                          new_y,
                           yname = yname,
                           xname = xname,
                           size = size,
                           buffer = buffer,
                           side = side) 
            
            p <-  add_plot(p, new_plot, pname)
            validObject(p)
            p
            
          })

#' add_row_barplot
#' @param p iheatmap object
#' @param x x axis values
#' @param ... additional arguments to add to plotly scatter trace, see
#' \url{https://plot.ly/javascript/reference/#scatter}
#' @param color color of bars
#' @param tracename name of trace (for legend and hover)
#' @param showlegend show in legend?
#' @param side side of plot on which to add subplot
#' @param layout yaxis layout list
#' @param size relative size of subplot relative to main heatmap
#' @param buffer amount of space to leave empty before this plot, relative to 
#' size of first heatmap
#' @param xname internal name of xaxis
#' @param yname internal name of yaxis
#' @param pname internal name of plot
#' 
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @rdname add_row_barplot
#' @name add_row_barplot
#' @aliases add_row_barplot,Iheatmap-method
#' @export
#' @seealso \code{\link{add_row_signal}}, \code{\link{iheatmap}}, 
#' \code{\link{add_row_plot}}
#' @author Alicia Schep
#' @examples 
#' 
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)  
#' hm <- iheatmap(mat) %>% add_row_barplot(x = 1:4, tracename = "Strength")
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod(add_row_barplot, c(p = "Iheatmap"),
          function(p,
                   x,
                   ...,
                   color = NULL, 
                   tracename = NA_character_,
                   showlegend = !is.na(tracename),
                   side = c("right","left"), 
                   layout = list(),
                   size = 0.2,
                   buffer = 0.02,
                   xname = NULL,
                   yname = current_yaxis(p),
                   pname = if (!is.na(tracename)) tracename else "row_barplot"){
            
            side <- match.arg(side)
            
            additional_data <- modifyList(list(orientation = 'h'), list(...))
            
            if (is.null(color) && !isTRUE(showlegend)){
              color = "gray"
            }
            if (!is.null(color)){
              color_list = list(marker = list(color = color))
              additional_data <- modifyList(color_list, additional_data)
            }
            
            
            new_x <- new_xaxis(p, yname,
                               layout = layout)
            
            if (is.null(xname)) xname <- id(new_x)
            
            new_plot <- new("RowPlot",
                            xaxis = xname,
                            yaxis = yname,
                            data = x,
                            showlegend = showlegend,
                            type = "bar",
                            name = tracename,
                            additional = additional_data)
            
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