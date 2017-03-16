setMethod("make_trace", signature = c(x = "RowPlot"),
          function(x, xaxes, yaxes, colorbars, colorbar_grid, ...){
            
            xa <- xaxes[[xaxis_name(x)]]
            ya <- yaxes[[yaxis_name(x)]]
            
            xvals <- get_data(x)[axis_order(ya)]
            
            txt = paste(if (!is.na(x@name)) x@name else NULL,
                        paste("Row:", axis_text(ya, ordered = TRUE)),
                        paste("Value:", xvals), sep = "<br>")
            
            trace <- modifyList(x@additional,
                                list(y = axis_values(ya),
                                     x = xvals,
                                     text = txt, 
                                     hoverinfo = "text",
                                     xaxis = id(xa),
                                     yaxis = id(ya),
                                     showlegend = x@showlegend,
                                     type = x@type,
                                     name = if (!is.na(x@name)) 
                                       x@name else NULL))
            return(trace)
          })


setMethod("make_trace", signature = c(x = "ColumnPlot"),
          function(x, xaxes, yaxes, colorbars, colorbar_grid, ...){
            
            xa <- xaxes[[xaxis_name(x)]]
            ya <- yaxes[[yaxis_name(x)]]
            
            yvals <- get_data(x)[axis_order(xa)]
            
            txt = paste(if (!is.na(x@name)) x@name else NULL,
                        paste("Column:", axis_text(xa, ordered = TRUE)),
                        paste("Value:", yvals), sep = "<br>")
            
            trace <- modifyList(x@additional,
                                list(x = axis_values(xa),
                                     y = yvals,
                                     text = txt, 
                                     hoverinfo = "text",
                                     xaxis = id(xa),
                                     yaxis = id(ya),
                                     showlegend = x@showlegend,
                                     type = x@type,
                                     name = if (!is.na(x@name)) 
                                       x@name else NULL))
            return(trace)
          })


#' add_col_plot
#' 
#' Add a scatter or line plot with one point per column of the main heatmap
#' @param p iheatmap object
#' @param y y axis values
#' @param ... additional arguments to add to plotly scatter trace, see
#' \url{https://plot.ly/javascript/reference/#scatter}
#' @param mode mode of plot -- one of "lines+markers","lines", or "markers"
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
#' @seealso \code{\link{add_col_signal}}, \code{\link{iheatmap}}, 
#' \code{\link{add_col_barplot}}
#' @rdname add_col_plot
#' @name add_col_plot
#' @aliases add_col_plot,Iheatmap-method
#' @author Alicia Schep
#' @examples 
#' 
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)  
#' iheatmap(mat) %>% add_col_plot(y = 1:5, tracename = "Strength")
setMethod(add_col_plot, c(p = "Iheatmap"),
          function(p,
                   y,
                   ...,
                   mode = c("lines+markers","lines","markers"),
                   color = NULL, 
                   tracename = NA_character_,
                   showlegend = !is.na(tracename),
                   side = c("top","bottom"), 
                   layout = list(),
                   size = 0.2,
                   buffer = 0.02,
                   xname = current_xaxis(p),
                   yname = NULL,
                   pname = if (!is.na(tracename)) tracename else "col_plot"){
            
            side <- match.arg(side)
            mode <- match.arg(mode)
            
            additional_data <- modifyList(list(mode = mode), list(...))
            
            if (is.null(color) && !isTRUE(showlegend)){
              if (mode == "lines" || mode == "lines+markers"){
                color_list = list(line = list(color = "black"))
              } else{
                color_list = list(marker = list(color = "black"))
              }
              additional_data <- modifyList(color_list, additional_data)
            } else if (!is.null(color)){
              if (mode == "lines" || mode == "lines+markers"){
                color_list = list(line = list(color = color))
              } else{
                color_list = list(marker = list(color = color))
              }
              additional_data <- modifyList(color_list, additional_data)
            }
            
            new_y <- new_yaxis(p, xname, layout)
            
            if (is.null(yname)) yname <- id(new_y)
            
            new_plot <- new("ColumnPlot",
                            xaxis = xname,
                            yaxis = yname,
                            data = y,
                            showlegend = showlegend,
                            type = "scatter",
                            name = tracename,
                            additional = additional_data)
            
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

#' add_row_plot
#' 
#'  Add a scatter or line plot with one point per row of the main heatmap
#' @param p iheatmap object
#' @param x x axis values
#' @param ... additional arguments to add to plotly scatter trace, see
#' \url{https://plot.ly/javascript/reference/#scatter}
#' @param mode mode of plot -- one of "lines+markers","lines", or "markers"
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
#' @rdname add_row_plot
#' @name add_row_plot
#' @aliases add_row_plot,Iheatmap-method
#' @seealso \code{\link{add_row_signal}}, \code{\link{iheatmap}}, 
#' \code{\link{add_row_barplot}}
#' @author Alicia Schep
#' @examples 
#' 
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)  
#' iheatmap(mat) %>% add_row_plot(x = 1:4, tracename = "Strength")
setMethod(add_row_plot, c(p = "Iheatmap"),
          function(p,
                   x,
                   ...,
                   mode = c("lines+markers","lines","markers"),
                   color = NULL, 
                   tracename = NA_character_,
                   showlegend = !is.na(tracename),
                   side = c("right","left"), 
                   layout = list(),
                   size = 0.2,
                   buffer = 0.02,
                   xname = NULL,
                   yname = current_yaxis(p),
                   pname = if (!is.na(tracename)) tracename else "row_plot"){
            
            side <- match.arg(side)
            mode <- match.arg(mode)
            
            additional_data <- modifyList(list(mode = mode), list(...))
            
            if (is.null(color) && !isTRUE(showlegend)){
              if (mode == "lines" || mode == "lines+markers"){
                color_list = list(line = list(color = "black"))
              } else{
                color_list = list(marker = list(color = "black"))
              }
              additional_data <- modifyList(color_list, additional_data)
            } else if (!is.null(color)){
              if (mode == "lines" || mode == "lines+markers"){
                color_list = list(line = list(color = color))
              } else{
                color_list = list(marker = list(color = color))
              }
              additional_data <- modifyList(color_list, additional_data)
            }
            
            new_x <- new_xaxis(p, yname, layout)
            
            if (is.null(xname)) xname <- id(new_x)
            
            new_plot <- new("RowPlot",
                            xaxis = xname,
                            yaxis = yname,
                            data = x,
                            showlegend = showlegend,
                            type = "scatter",
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