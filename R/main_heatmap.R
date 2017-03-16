new_plots <- function(object,
                      name){
  out <- new("IheatmapPlots")
  out[[name]] <- object
  out
}

new_axes <- function(object,
                     name,
                     axis = c("x","y")){
  out <- new("IheatmapAxes",
             axis = match.arg(axis))
  out[[name]] <- object
  out
}

new_colorbars <- function(object, name){
  out <- new("IheatmapColorbars")
  out[[name]] <- object
  out
}

new_iheatmap <- function(plot,
                         xaxis,
                         yaxis,
                         pname = "Signal",
                         xname = "x",
                         yname = "y",
                         orientation = c("horizontal","vertical"),
                         colorbar_grid = setup_colorbar_grid(),
                         colorbar = NULL,
                         colorbar_name = NULL,
                         font = list(),
                         ...){
  orientation = match.arg(orientation)
  if (orientation == "horizontal"){
    new("IheatmapHorizontal",
        plots = new_plots(plot, pname),
        xaxes = new_axes(xaxis, xname, axis = "x"),
        yaxes = new_axes(yaxis, yname, axis = "y"),
        colorbar_grid = colorbar_grid,
        current_xaxis = xname,
        current_yaxis = yname,
        colorbars = new_colorbars(colorbar, colorbar_name),
        layout = list(hovermode = 'closest',
                      margin = list(b = 50,
                                    t = 50,
                                    l = 50,
                                    r= 50),
                      font = font))
  } else{
    new("IheatmapVertical",
        plots = new_plots(plot, pname),
        xaxes = new_axes(xaxis, xname, axis = "x"),
        yaxes = new_axes(yaxis, yname, axis = "y"),
        colorbar_grid = colorbar_grid,
        current_xaxis = xname,
        current_yaxis = yname,
        colorbars = new_colorbars(colorbar, colorbar_name),
        layout = list(hovermode = 'closest',
                      margin = list(b = 50,
                                    t = 50,
                                    l = 50,
                                    r= 50),
                      font = font))
  }
}

#' main_heatmap
#' 
#' Plots initial heatmap, creates Iheatmap object
#' @param data matrix
#' @param name name of colorbar
#' @param x x axis labels (by default rownames of data)
#' @param y y axis labels (by default colnames of data)
#' @param colors color palette or vector of colors
#' @param colorbar_grid colorbar grid parameters, should be result from 
#' \code{\link{setup_colorbar_grid}} 
#' @param colorbar_position colorbar placement, should be positive integer
#' @param zmin minimum for colorscale
#' @param zmax maximum for colorscale
#' @param zmid midpoint for colorscale
#' @param orientation should new main plots be added horizontally or vertically?
#' @param x_categorical is x categorical?  will guess if not provided
#' @param y_categorical is y categorical?  will guess if not provided
#' @param row_order row ordering for this heatmap-- will be used for all 
#' subsequent elements sharing y axis
#' @param col_order column ordering for this heatmap-- will be used for all 
#' subsequent elements sharing x axis
#' @param font list of font attributes to pass to plotly, eg. list(size = 15)
#' @param xname internal name for xaxis
#' @param yname internal name for yaxis
#' @param pname internal plot name
#' 
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @seealso \code{\link{add_iheatmap}}, \code{\link{as_plotly}},  
#' \code{\link{iheatmap}}, \code{\link{Iheatmap-class}}
#' @export
#' @author Alicia Schep
#' @rdname main_heatmap
#' @name main_heatmap
#' @aliases main_heatmap,matrix-method
#' @examples 
#' 
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)  
#' main_heatmap(mat) 
setMethod(main_heatmap, "matrix",
          function(data,
                   name = "Signal",
                   x = default_x(data),
                   y = default_y(data),
                   colors = pick_continuous_colors(zmid, zmin, zmax),
                   colorbar_grid = setup_colorbar_grid(),
                   colorbar_position = 1,
                   zmid = 0,
                   zmin = min(data, na.rm = TRUE),
                   zmax = max(data, na.rm = TRUE),
                   orientation = c("horizontal", "vertical"),
                   x_categorical = NULL,
                   y_categorical = NULL,
                   row_order = seq_len(nrow(data)),
                   col_order = seq_len(ncol(data)),
                   font = list(),
                   xname = "x",
                   yname = "y",
                   pname = name){
            
            iheatmap_argument_checks(data, row_order, col_order, x, y)
            orientation <- match.arg(orientation)
            if (is.null(x_categorical)) 
              x_categorical <- is_categorical(x, col_order)
            if (is.null(y_categorical)) 
              y_categorical <- is_categorical(y, row_order)
            
            new_plot <- new("MainHeatmap",
                            xaxis = "x",
                            yaxis = "y",
                            colorbar = name,
                            show_colorbar = TRUE,
                            data = data)
            
            if (x_categorical){
              xrange <- c(0.5,length(col_order)+0.5)
            } else{
              x <- as.numeric(x)
              xdiff <- diff(sort(x))
              xrange <- c(min(x) - xdiff[1] * 0.5, 
                          max(x) + xdiff[length(xdiff)] * 0.5)
            }
            new_x <- new("IheatmapMainX",
                         id = "x",
                         domain_start = 0,
                         domain_end = 1,
                         categorical = x_categorical,
                         order = col_order,
                         text = x,
                         anchor = "y",
                         layout = list(ticks = "",
                                       zeroline = FALSE,
                                       showline = FALSE,
                                       showgrid = FALSE,
                                       showticklabels = FALSE,
                                       range = xrange))
            
            if (y_categorical){
              yrange <- c(0.5,length(row_order)+0.5)
            } else{
              y <- as.numeric(y)
              ydiff <- diff(sort(y))
              yrange <- c(min(y) - (ydiff[1] * 0.5), 
                          max(y) + (ydiff[length(ydiff)] * 0.5))
            }
            new_y <- new("IheatmapMainY",
                         id = "y",
                         domain_start = 0,
                         domain_end = 1,
                         categorical = y_categorical,
                         order = row_order,
                         text = y,
                         anchor = "x",
                         layout = list(ticks = "",
                                       zeroline = FALSE,
                                       showline = FALSE,
                                       showgrid = FALSE,
                                       showticklabels = FALSE,
                                       range = yrange))
            
            new_colorbar <- continuous_colorbar(name = name,
                       position = colorbar_position,
                       colors = colors,
                       zmid = zmid,
                       zmin = zmin,
                       zmax = zmax)

  p <- new_iheatmap(plot = new_plot,
                      xaxis = new_x,
                      yaxis = new_y,
                      xname = xname,
                      yname = yname,
                      pname = pname,
                      orientation = orientation,
                      colorbar_grid = colorbar_grid,
                      colorbar = new_colorbar,
                      colorbar_name = name,
                      font = font)

  validObject(p)
  p
  
})


#' add_main_heatmap
#' 
#' Adds an additional main heatmap to an iheatmap object
#' @param p \code{\link{Iheatmap-class}} object
#' @param data matrix
#' @param name name of colorbar, will determine if colorbar is shared with 
#' existing plot
#' @param x x axis labels (by default rownames of data); only used if 
#' orientation is horizontal
#' @param y y axis labels (by default colnames of data); only used if 
#' orientation is vertical
#' @param colors color pallette name or vector of colors
#' @param colorbar_position colorbar placement
#' @param show_colorbar display the colorbar?
#' @param zmin minimum for colorscale
#' @param zmax maximum for colorscale
#' @param zmid midpoint for scale
#' @param x_categorical is x categorical?  will guess if not provided
#' @param y_categorical is y categorical?  will guess if not provided
#' @param col_order column ordering for this heatmap; only used if orientation 
#' is horizontal
#' @param row_order row ordering for this heatmap; only used if orientation is 
#' vertical
#' @param side which side of the current plot to add this heatmap? 
#' @param size relative size of plot.  size relative to first heatmap
#' @param buffer amount of space to leave empty before this plot, relative to 
#' size of first heatmap
#' @param xname internal name for x axis
#' @param yname internal name for y axis
#' @param pname internal name for plot
#' 
#' @seealso \code{\link{iheatmap}}, \code{\link{main_heatmap}}
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @author Alicia Schep
#' @rdname add_main_heatmap
#' @name add_main_heatmap
#' @aliases add_main_heatmap,IheatmapHorizontal,matrix-method 
#' add_main_heatmap,IheatmapVertical,matrix-method
#' @examples 
#' 
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4) 
#' mat2 <-  matrix(rnorm(24), ncol = 6, nrow = 4) 
#' iheatmap(mat) %>% add_main_heatmap(mat2)
setMethod("add_main_heatmap", c(p = "IheatmapHorizontal", data = "matrix"),
          function(p,
                   data,
                   name = "Signal",
                   x = default_x(data),
                   colors = pick_continuous_colors(zmid,zmin,zmax, p),
                   colorbar_position = get_colorbar_position(p),
                   show_colorbar = TRUE,
                   zmin = min(data, na.rm = TRUE),
                   zmax = max(data, na.rm = TRUE),
                   zmid = 0,
                   col_order = NULL,
                   x_categorical = NULL,
                   side = c("right","left"),
                   size = 1,
                   buffer = 0.04,
                   xname = NULL,
                   pname = name){
            
            side <- match.arg(side)
            
            if (is.null(col_order)) col_order <- seq_len(ncol(data))
            if (is.null(x_categorical)) 
              x_categorical <- is_categorical(x, col_order)
            stopifnot(is.logical(x_categorical))
            if (x_categorical){
              xrange <- c(0.5,length(col_order)+0.5)
            } else{
              x <- as.numeric(x)
              xdiff <- diff(sort(x))
              xrange <- c(min(x) - (xdiff[1] * 0.5), 
                          max(x) + (xdiff[length(xdiff)] * 0.5))
            }
            new_x <- 
              new("IheatmapMainX",
                  id = paste0("x", length(xaxes(p)) + 1),
                  domain_start = 0,
                  domain_end = 1,
                  categorical = x_categorical,
                  order = as.integer(col_order),
                  text = x,
                  anchor = "y",
                  layout = list(ticks = "",
                                zeroline = FALSE,
                                showline = FALSE,
                                showgrid = FALSE,
                                showticklabels = FALSE,
                                range = xrange))
            
            
            if (is.null(xname)) xname <- id(new_x)
            
            current_xaxis(p) <- xname
            
            new_plot <- new("MainHeatmap",
                            xaxis = xname,
                            yaxis = "y",
                            colorbar = name,
                            show_colorbar = TRUE,
                            data = data)
            
            new_colorbar <- continuous_colorbar(name, colorbar_position, 
                                                colors, zmid, zmin, zmax)
            
            p <- add_axis(p, new_x, xname, size, buffer, side) 
            
            p <- add_plot(p, new_plot, pname)
            
            p <- add_colorbar(p, new_colorbar)
            
            validObject(p)
            p
            
          })


#' @rdname add_main_heatmap
#' @export
setMethod("add_main_heatmap", c(p = "IheatmapVertical", data = "matrix"),
          function(p,
                   data,
                   name = "Signal",
                   y = default_y(data),
                   colors = pick_continuous_colors(zmid,zmin,zmax, p),
                   colorbar_position = get_colorbar_position(p),
                   show_colorbar = TRUE,
                   zmin = min(data, na.rm = TRUE),
                   zmax = max(data, na.rm = TRUE),
                   zmid = 0,
                   row_order = NULL,
                   y_categorical = NULL,
                   side = c("bottom","top"),
                   size = 1,
                   buffer = 0.04,
                   yname = NULL,
                   pname = name){
            
            side <- match.arg(side)
            
            if (is.null(row_order)) row_order <- seq_len(nrow(data))
            if (is.null(y_categorical)) 
              y_categorical <- is_categorical(y, row_order)
            stopifnot(is.logical(y_categorical))
            if (y_categorical){
              yrange <- c(0.5,length(row_order)+0.5)
            } else{
              y <- as.numeric(y)
              ydiff <- diff(sort(y))
              yrange <- c(min(y) - (ydiff[1] * 0.5), 
                          max(y) + (ydiff[length(ydiff)] * 0.5))
            }
            new_y <- 
              new("IheatmapMainY",
                  id = paste0("y", length(yaxes(p)) + 1),
                  domain_start = 0,
                  domain_end = 1,
                  categorical = y_categorical,
                  order = as.integer(row_order),
                  text = y,
                  anchor = "x",
                  layout = list(ticks = "",
                                zeroline = FALSE,
                                showline = FALSE,
                                showgrid = FALSE,
                                showticklabels = FALSE,
                                range = yrange))
            
            
            if (is.null(yname)) yname <- id(new_y)
            
            current_yaxis(p) <- yname
            
            new_plot <- new("MainHeatmap",
                            xaxis = "x",
                            yaxis = yname,
                            colorbar = name,
                            show_colorbar = TRUE,
                            data = data)
            
            new_colorbar <- continuous_colorbar(name, colorbar_position, 
                                                colors, zmid, zmin, zmax)
            
            p <- add_axis(p, new_y, yname, size, buffer, side)
            p <- add_plot(p, new_plot, pname)
            p <- add_colorbar(p, new_colorbar)
            validObject(p)
            p
            
          })




setMethod("make_trace", signature = c(x = "MainHeatmap"),
          function(x, xaxes, yaxes, colorbars, colorbar_grid, ...){
            
            cb <- colorbars[[x@colorbar]]
            xa <- xaxes[[xaxis_name(x)]]
            ya <- yaxes[[yaxis_name(x)]]
            
            txt <- make_text_matrix(get_data(x), axis_text(xa), axis_text(ya), 
                                    axis_order(ya), axis_order(xa))
            colorscale <- colorscale(cb, get_data(x))
            
            out <- list(z = get_data(x)[axis_order(ya), 
                                        axis_order(xa), 
                                        drop = FALSE],
                        x = axis_values(xa),
                        y = axis_values(ya),
                        type="heatmap",
                        text = txt,
                        colorscale = colorscale,
                        xaxis = id(xa),
                        yaxis = id(ya),
                        zmin = cb@zmin,
                        zmax = cb@zmax,
                        hoverinfo = "text",
                        showscale = x@show_colorbar,
                        colorbar = make_colorbar(cb, colorbar_grid))

            return(out)
          })



make_text_matrix <- function(mat, x, y, row_order, col_order){
  
  txt <- outer(paste("Row:", y[row_order]), paste("Column:", x[col_order]),
               FUN = paste, sep = "<br>")
  
  txt <- matrix(paste(txt, paste("Value:", mat[row_order, col_order]), 
                      sep = "<br>"),
                nrow = length(row_order), ncol = length(col_order))
  
  return(txt)
}

iheatmap_argument_checks <- function(mat, row_order, col_order, x, y){
  if (!inherits(mat,"matrix")) stop("data must be a matrix!")
  if (any(row_order %ni% seq_len(nrow(mat)))) 
    stop("Row order contains invalid indices")
  if (any(col_order %ni% seq_len(ncol(mat)))) 
    stop("Col order contains invalid indices")
  if (length(y) != nrow(mat)) 
    stop("y does not match number of rows of matrix")
  if (length(x) != ncol(mat)) 
    stop("x does not match number of columns of matrix")
  return(TRUE)
}
