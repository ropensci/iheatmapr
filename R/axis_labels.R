categorical_ticks <- function(ax, tickvals, ticktext){
  
  if (is.null(tickvals)){
    tickvals <- axis_values(ax)
    if (is.null(ticktext)){
      ticktext <- axis_text(ax)
    } else{
      ticktext <- as.character(ticktext)
      if (all(ticktext %in% axis_text(ax))){
        ix <- sapply(ticktext, function(x) which(axis_text(ax) == x))
        tickvals <- tickvals[ix]
      } else if (length(ticktext) != length(tickvals))
        stop(paste("Provided ticktext is invalid,",
                   "must match expected values or be same length as number of",
                   "rows",
                   sep=" "))
    }
  } else{
    if (is.null(ticktext)){
      if (!all(tickvals %in% axis_values(ax)))
        stop("Provided tick values invalid, must match positions or rows")
      ticktext <- axis_text(ax)
      ix <- sapply(tickvals, function(x) which(axis_values(ax) == x))
      ticktext <- ticktext[ix]
    } else{
      ticktext <- as.character(ticktext)
      if (length(ticktext) != length(tickvals))
        stop(paste("Provided ticktext invalid, must be same length as provided", 
                   "tickvals",sep =" "))
    }
  }
  return(list(ticktext = ticktext, tickvals = tickvals))
}

isbetween <- function(x, left, right){
  as.logical((x >= left) * (x <= right))
}

continuous_ticks <- function(ax, tickvals, ticktext){
  if (is.null(tickvals)){
    if (is.null(ticktext)){
      tickvals <- axis_values(ax)
      ticktext <- axis_text(ax)
      if (length(ticktext) > 8){
        new_tickvals = pretty(tickvals,6)
        tickvals = new_tickvals[which(isbetween(new_tickvals,min(tickvals),
                                                max(tickvals)))]
        ticktext = as.character(tickvals)
      }
    } else{
      ticktext <- as.character(ticktext)
      tickvals <- axis_values(ax)
      if (all(ticktext %in% axis_text(ax))){
        ix <- sapply(ticktext,
                     function(x) which(axis_text(ax) == x))
        tickvals <- tickvals[ix]
      } else if (length(ticktext) != length(tickvals))
        stop(paste("Provided ticktext is invalid,",
                   "must match expected values or be same length as number of", 
                   "rows",
                   sep=" "))
    }
  } else{
    if (is.null(ticktext)){
     if (!all(tickvals %in% axis_values(ax)))
        stop("Provided tick values invalid, must match positions or rows")
      ticktext <- axis_text(ax)
      ix <- sapply(tickvals, function(x) which(axis_values(ax) == x))
      ticktext <- ticktext[ix]
    } else{
      ticktext <- as.character(ticktext)
      if (length(ticktext) != length(tickvals))
        stop(paste("Provided ticktext invalid, must be same length as provided", 
                   "tickvals", sep =" "))
    }
  }
  return(list(ticktext = ticktext, tickvals = tickvals))
}

categorical_row_labels <- function(p, tickvals, ticktext, xname, yname,
                                           side, textangle, font){

  ya = yaxes(p)[[yname]]
  ticks <- categorical_ticks(ya, tickvals, ticktext)

  new("RowLabels",
      xaxis = xname,
      yaxis = yname,
      data = ticks$ticktext,
      positions = ticks$tickvals,
      side = side,
      textangle = textangle,
      font = font)
}

continuous_row_labels <- function(p, tickvals, ticktext, xname, yname,
                                           side, textangle, font){

  ya = yaxes(p)[[yname]]
  ticks <- continuous_ticks(ya, tickvals, ticktext)
  
  new("RowLabels",
      xaxis = xname,
      yaxis = yname,
      data = ticks$ticktext,
      positions = ticks$tickvals,
      side = side,
      textangle = textangle,
      font = font)
}


categorical_col_labels <- function(p, tickvals, ticktext, xname, yname,
                                   side, textangle, font){
  
  xa = xaxes(p)[[xname]]
  ticks <- categorical_ticks(xa, tickvals, ticktext)
  
  new("ColumnLabels",
      xaxis = xname,
      yaxis = yname,
      data = ticks$ticktext,
      positions = ticks$tickvals,
      side = side,
      textangle = textangle,
      font = font)
}

continuous_col_labels <- function(p, tickvals, ticktext, xname, yname,
                                  side, textangle, font){
  
  xa = xaxes(p)[[xname]]
  ticks <- continuous_ticks(xa, tickvals, ticktext)
  
  new("ColumnLabels",
      xaxis = xname,
      yaxis = yname,
      data = ticks$ticktext,
      positions = ticks$tickvals,
      side = side,
      textangle = textangle,
      font = font)
}

setMethod("make_annotations", signature = c(x = "RowLabels"),
          function(x, xaxes, yaxes, ...){

            xa <- xaxes[[xaxis_name(x)]]
            ya <- yaxes[[yaxis_name(x)]]
            side <- x@side

            tickvals <- x@positions
            ticktext <- get_data(x)
            
            if (ya@categorical){
              tickvals <- sapply(tickvals,
                                 function(x) which(ya@order == x)[1])
            }

            a <-  lapply(seq_along(tickvals),
                         function(i) {
                           list(text = ticktext[i],
                                y = tickvals[i],
                                x = if (side == "left") 1 else -1,
                                textangle = x@textangle,
                                showarrow = FALSE,
                                xref = id(xa),
                                yref = id(ya),
                                font = x@font,
                                xanchor = if (side == "left") 
                                  "right" else "left")
                         })

            a
          })

setMethod("make_annotations", signature = c(x = "ColumnLabels"),
          function(x, xaxes, yaxes, ...){
            
            xa <- xaxes[[xaxis_name(x)]]
            ya <- yaxes[[yaxis_name(x)]]
            side <- x@side
            
            tickvals <- x@positions
            ticktext <- get_data(x)
            
            if (xa@categorical){
              tickvals <- sapply(tickvals,
                                 function(x) which(xa@order == x)[1])
            }
            
            a <-  lapply(seq_along(tickvals),
                         function(i) {
                           list(text = ticktext[i],
                                x = tickvals[i],
                                y = if (side == "bottom") 1 else -1,
                                textangle = x@textangle,
                                showarrow = FALSE,
                                xref = id(xa),
                                yref = id(ya),
                                font = x@font,
                                yanchor = if (side == "bottom") 
                                  "top" else "bottom")
                         })
            
            a
          })



#' add_row_labels
#' 
#' Add y axis labels to plot
#' @param p \code{\link{Iheatmap-class}} object
#' @param tickvals row indices at which to place axis tick labels
#' @param ticktext text for axis tick labels
#' @param textangle angle for ticktext
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
#' @rdname add_row_labels
#' @name add_row_labels
#' @aliases add_row_labels,Iheatmap-method
#' @seealso \code{\link{add_row_title}}, \code{\link{iheatmap}}, 
#' \code{\link{add_col_labels}}
#' @author Alicia Schep
#' @examples 
#' 
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)  
#' iheatmap(mat) %>% add_row_labels()
#' iheatmap(mat) %>% add_row_labels(ticktext = letters[23:26])
setMethod(add_row_labels, c(p = "Iheatmap"),
          function(p,
                   tickvals = NULL,
                   ticktext = NULL,
                   textangle = 0,
                   font = list(),
                   side = c("left","right"),
                   size = 0.1,
                   buffer = 0.005,
                   xname = NULL,
                   yname = current_yaxis(p)){
            
            side <- match.arg(side)
            
            new_x <- new_xaxis(p, yname, 
                               layout =  c(no_axis,
                                     list(range = c(-1,1),
                                          fixedrange = TRUE)))
            
            if (is.null(xname)) xname <- id(new_x)
              
            if (yaxes(p)[[yname]]@categorical){
              new_anno <- categorical_row_labels(p, tickvals, ticktext,xname, 
                                               yname, side, textangle, font)
            } else{
              new_anno <- continuous_row_labels(p, tickvals, ticktext,xname, 
                                                 yname, side, textangle,font)
            }
            
            p <- add_axis(p, 
                          new_x,
                          xname = xname,
                          yname = yname,
                          size = size,
                          buffer = buffer,
                          side = side) 
            
            p <- add_annotation(p, new_anno, "row_labels")
            
            validObject(p)
            p
            
          })

#' add_col_labels
#' 
#' Add x axis labels to plot
#' @param p \code{link{Iheatmap-class}} object
#' @param tickvals column indices at which to place axis tick labels
#' @param ticktext text for axis tick labels
#' @param textangle angle for ticktext
#' @param font list of plotly font attributes, see 
#' \url{https://plot.ly/javascript/reference/#layout-font}
#' @param side side of plot on which to add subplot 
#' @param size relative size of subplot relative to main heatmap
#' @param buffer amount of space to leave empty before this plot, relative to 
#' size of first heatmap
#' @param xname name for xaxis
#' @param yname name for yaxis
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @rdname add_col_labels
#' @name add_col_labels
#' @aliases add_col_labels,Iheatmap-method
#' @seealso \code{\link{add_row_title}}, \code{\link{iheatmap}}, 
#' \code{\link{add_col_labels}}
#' @author Alicia Schep
#' @examples 
#' 
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)  
#' iheatmap(mat) %>% add_col_labels()
#' iheatmap(mat) %>% add_col_labels(ticktext = letters[22:26])
setMethod(add_col_labels, c(p = "Iheatmap"),
          function(p,
                   tickvals = NULL,
                   ticktext = NULL,
                   textangle = -90,
                   font = list(),
                   side = c("bottom","top"),
                   size = 0.1,
                   buffer = 0.005,
                   xname = current_xaxis(p),
                   yname = NULL){
            
            side <- match.arg(side)
            
            new_y <- new_yaxis(p,xname,
                               layout =  c(no_axis,
                                           list(range = c(-1,1),
                                                fixedrange = TRUE)))
            
            if (is.null(yname)) yname <- id(new_y)
            
            if (xaxes(p)[[xname]]@categorical){
              new_anno <- categorical_col_labels(p, tickvals, ticktext,xname, 
                                                 yname, side, textangle, font)
            } else{
              new_anno <- continuous_col_labels(p, tickvals, ticktext,xname, 
                                                yname, side, textangle,font)
            }
            
            
            p <- add_axis(p, new_y,
                           xname = xname,
                           yname = yname,
                           size = size,
                           buffer = buffer,
                           side = side)
            
            p <- add_annotation(p, new_anno, "col_labels")
            
            validObject(p)
            p
            
          })