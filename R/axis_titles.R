
setMethod("make_annotations", signature = c(x = "ColumnTitle"),
          function(x, xaxes, yaxes, ...){
            
            xa <- xaxes[[xaxis_name(x)]]
            ya <- yaxes[[yaxis_name(x)]]
            side <- x@side
            
            tickvals <- axis_values(xa)
            
            a <- list(list(text = get_data(x), 
                           x = min(tickvals) + 
                             (max(tickvals) - min(tickvals)) / 2,
                           y = ifelse(side == "bottom", 1, -1),
                           textangle = x@textangle,
                           showarrow = FALSE,
                           yanchor = ifelse(side == "top", "bottom","top"),
                           xanchor = "center",
                           font = x@font,
                           xref = id(xa),
                           yref = id(ya)))
            
            a
          })

setMethod("make_annotations", signature = c(x = "RowTitle"),
          function(x, xaxes, yaxes, ...){
            
            xa <- xaxes[[xaxis_name(x)]]
            ya <- yaxes[[yaxis_name(x)]]
            side <- x@side
            
            tickvals <- axis_values(ya)
            
            a <- list(list(text = get_data(x), 
                           y = min(tickvals) + (max(tickvals) - 
                                                  min(tickvals)) /  2,
                           x = ifelse(side == "left", 1, -1),
                           textangle = x@textangle,
                           showarrow = FALSE,
                           xanchor = ifelse(side == "left", "right","left"),
                           yanchor = "center",
                           font = x@font,
                           xref = id(xa),
                           yref = id(ya)))
            
            a
          })


#' add_col_title
#' 
#' Add x axis title to plot
#' 
#' @param p iheatmap object
#' @param title title of axis
#' @param textangle angle of text
#' @param font list of plotly font attributes, see 
#' \url{https://plot.ly/javascript/reference/#layout-font}
#' @param side side of plot on which to add subplot 
#' @param size relative size of subplot relative to main heatmap
#' @param buffer amount of space to leave empty before this plot, relative to 
#' size of first heatmap
#' @param xname name for xaxis
#' @param yname name for yaxis
#' 
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @author Alicia Schep
#' @rdname add_col_title
#' @name add_col_title
#' @aliases add_col_title,Iheatmap-method
#' @seealso \code{\link{add_col_labels}}, \code{\link{iheatmap}}, 
#' \code{\link{add_row_title}}
#' @examples 
#' 
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)  
#' hm <- iheatmap(mat) %>% add_col_title("My x-axis")
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod(add_col_title, c(p = "Iheatmap"),
          function(p,
                   title,
                   textangle = 0,
                   font = get_layout(p)$font,
                   side = c("bottom","top"),
                   size = 0.1,
                   buffer = 0.01,
                   xname = current_xaxis(p),
                   yname = NULL){
            
            side <- match.arg(side)
            new_y <- new_yaxis(p, xname,
                               layout =  c(no_axis,
                                     list(range = c(-1,1),
                                          fixedrange = TRUE)))
            
            if (is.null(yname)) yname <- id(new_y)
            
            new_anno <- new("ColumnTitle",
                             xaxis = xname,
                             yaxis = yname,
                             data = title,
                            side = side,
                            textangle = textangle,
                            font = font)

            p <- add_axis(p,
                          new_y,
                           xname = xname,
                           yname = yname,
                           size = size,
                           buffer = buffer,
                           side = side) 
            
            p <- add_annotation(p, new_anno, "col_title")
            
            validObject(p)
            p
            
})

#' add_row_title
#' 
#' Add y axis title to plot
#' @param p iheatmap object
#' @param title title of axis
#' @param textangle angle of text
#' @param font list of plotly font attributes, see 
#' \url{https://plot.ly/javascript/reference/#layout-font}
#' @param side side of plot on which to add subplot 
#' @param size relative size of subplot relative to main heatmap
#' @param buffer amount of space to leave empty before this plot, relative to 
#' size of first heatmap
#' @param xname internal name for xaxis
#' @param yname internal name for yaxis

#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @rdname add_row_title
#' @name add_row_title
#' @aliases add_row_title,Iheatmap-method
#' @seealso \code{\link{add_col_title}}, \code{\link{iheatmap}}, 
#' \code{\link{add_row_labels}}
#' @author Alicia Schep
#' @examples 
#' 
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)  
#' hm <- iheatmap(mat) %>% add_row_title("Samples")
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod(add_row_title, c(p = "Iheatmap"),
          function(p,
                   title,
                   textangle = ifelse(side == "left", -90,90),
                   font = get_layout(p)$font,
                   side = c("left","right"),
                   size = 0.1,
                   buffer = 0.01,
                   xname = NULL,
                   yname = current_yaxis(p)){
            
            side <- match.arg(side)
            new_x <- new_xaxis(p, yname,
                         layout =  c(no_axis,
                                     list(range = c(-1,1),
                                          fixedrange = TRUE)))
            
            if (is.null(xname)) xname <- id(new_x)
            
            new_anno <- new("RowTitle",
                            xaxis = xname,
                            yaxis = yname,
                            data = title,
                            side = side,
                            textangle = textangle,
                            font = font)
            
            p <- add_axis(p,
                          new_x,
                           xname = xname,
                           yname = yname,
                           size = size,
                           buffer = buffer,
                           side = side) 
            p <-  add_annotation(p, new_anno, "row_title")
            validObject(p)
            p
            
            
          })
