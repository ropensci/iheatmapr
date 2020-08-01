setMethod("make_trace", signature = c(x = "RowAnnotation"),
          function(x, xaxes, yaxes, colorbars, colorbar_grid, ...){
            
            cb <- colorbars[[colorbar(x)]]
            xa <- xaxes[[xaxis_name(x)]]
            ya <- yaxes[[yaxis_name(x)]]
            
            mat <- matrix(get_data(x), ncol=1)
            txt <- make_text_matrix(matrix(x@text,ncol=1), get_title(x), axis_text(ya), 
                                    axis_order(ya), 1, x@tooltip)
            
            colorscale <- colorscale(cb, mat)
            
            if (is(cb, "DiscreteColorbar")){
              mat <- matrix(as.numeric(factor(mat, levels = cb@ticktext, 
                                              ordered = TRUE)),
                            nrow = nrow(mat), ncol = ncol(mat))
            }
            
            z <- mat[axis_order(ya), 1, drop = FALSE]
            
            out <- list(z = I(z),
                        y = I(axis_values(ya)),
                        x = c(1),
                        type= "heatmap",
                        text = I(txt),
                        colorscale = colorscale,
                        xaxis = id(xa),
                        yaxis = id(ya),
                        zmin = zmin(cb),
                        zmax = zmax(cb),
                        hoverinfo = "text",
                        showscale = x@show_colorbar,
                        colorbar = make_colorbar(cb, colorbar_grid))

            return(out)
          })

setMethod("make_trace", signature = c(x = "ColumnAnnotation"),
          function(x, xaxes, yaxes, colorbars, colorbar_grid, ...){

            cb <- colorbars[[colorbar(x)]]
            xa <- xaxes[[xaxis_name(x)]]
            ya <- yaxes[[yaxis_name(x)]]
            
            mat <- matrix(get_data(x), nrow=1)
            txt <- make_text_matrix(matrix(x@text, nrow = 1), 
                                    axis_text(xa), 
                                    get_title(x), 
                                    1, 
                                    axis_order(xa),
                                    x@tooltip)
            
            colorscale <- colorscale(cb, mat)
            
            if (is(cb, "DiscreteColorbar")){
              mat <- matrix(as.numeric(factor(mat, levels = cb@ticktext, 
                                              ordered = TRUE)),
                            nrow = nrow(mat), ncol = ncol(mat))
            }
            
            
            z <- mat[1, axis_order(xa), drop = FALSE]
            
            out <- list(z = I(z),
                        x = I(axis_values(xa)),
                        y = c(1),
                        type= "heatmap",
                        text = I(txt),
                        colorscale = colorscale,
                        xaxis = id(xa),
                        yaxis = id(ya),
                        zmin = zmin(cb),
                        zmax = zmax(cb),
                        hoverinfo = "text",
                        showscale = x@show_colorbar,
                        colorbar = make_colorbar(cb, colorbar_grid))

            return(out)
          })


row_annotation_heatmap_layout <- function(title, anchor, layout, show_title){
  modifyList(list(anchor = anchor,
                  title = "",
                  zeroline = FALSE,
                  showline = FALSE,
                  tickvals = list(0),
                  ticktext = if (show_title) list(title) else list(""),
                  showgrid = FALSE,
                  fixedrange = TRUE,
                  ticks = "",
                  tickangle = -90), layout)
}

col_annotation_heatmap_layout <- function(title, anchor, layout, show_title){
  modifyList(list(anchor = anchor,
                  title = "",
                  zeroline = FALSE,
                  showline = FALSE,
                  tickvals = list(0),
                  ticktext = if (show_title) list(title) else list(""),
                  showgrid = FALSE,
                  fixedrange = TRUE,
                  ticks = ""), layout)
}

#' add_row_signal
#' 
#' Adds single column heatmap to iheatmap object
#' @param p iheatmap object
#' @param signal vector of signal
#' @param colors color palette or vector of colors
#' @param colorbar_position colorbar placement
#' @param name name of colorbar
#' @param title label for x axis
#' @param xname internal name of xaxis
#' @param yname internal name of yaxis
#' @param pname internal name of plot
#' @param zmin minimum for colorscale
#' @param zmax maximum for colorscale
#' @param zmid midpoint for colorscale
#' @param side side of plot on which to add dendro
#' @param size relative size of dendrogram (relative to the main heatmap)
#' @param buffer amount of space to leave empty before this plot, relative to 
#' size of first heatmap
#' @param text text of value to display for data
#' @param tooltip tooltip options, see \code{\link{setup_tooltip_options}}
#' @param show_colorbar show the colorbar?
#' @param show_title show title as axis label
#' @param layout list of x axis layout parameters
#' @seealso \code{\link{iheatmap}}, \code{\link{add_col_groups}}
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @rdname add_row_signal
#' @name add_row_signal
#' @aliases add_row_signal,Iheatmap-method
#' @author Alicia Schep
#' @seealso \code{\link{add_col_signal}}, \code{\link{iheatmap}}, 
#' \code{\link{add_row_annotation}}
#' @examples 
#' 
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)  
#' hm <- iheatmap(mat) %>% add_row_signal(signal = 1:4, name = "Strength")
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod(add_row_signal, c(p = "Iheatmap", signal = "ANY"),
          function(p,
                   signal,
                   name,
                   title = name,
                   xname = NULL,
                   yname = current_yaxis(p),
                   pname = name,
                   colorbar_position = get_colorbar_position(p),
                   colors = pick_continuous_colors(zmid, zmin, zmax, p = p),
                   zmin = min(signal, na.rm = TRUE),
                   zmax = max(signal, na.rm = TRUE),
                   zmid = 0,
                   side = c("right","left"),
                   size = 0.05,
                   buffer = 0.015,
                   text = signif(signal, digits = 3),
                   tooltip = setup_tooltip_options(),
                   show_colorbar = TRUE,
                   show_title = TRUE,
                   layout = list()){

            side <- match.arg(side)
            new_layout <- row_annotation_heatmap_layout(title, yname, layout, 
                                                        show_title)
            new_x <- new_xaxis(p, yname, layout = new_layout)

            if (is.null(xname)) xname <- id(new_x)

            new_plot <- new("RowAnnotation",
                            xaxis = xname,
                            yaxis = yname,
                            colorbar = name,
                            show_colorbar = show_colorbar,
                            data = signal,
                            title = title,
                            text = text,
                            tooltip = tooltip)

            new_colorbar <- continuous_colorbar(name,
                                                colorbar_position,
                                                colors,
                                                zmid,
                                                zmin,
                                                zmax)

            p <- add_axis(p,
                          new_x,
                          xname = xname,
                          yname = yname,
                          size = size,
                          buffer = buffer,
                          side = side) 
            p <- add_plot(p, new_plot, pname)
            p <- add_colorbar(p, new_colorbar)
            validObject(p)
            p
            
          })

#' add_col_signal
#' 
#' Adds column signal to iheatmap object
#' @param p iheatmap object
#' @param signal vector of signal
#' @param xname internal name of xaxis
#' @param yname internal name of yaxis
#' @param pname internal name of plot
#' @param colors palette or vector of colors to use
#' @param colorbar_position colorbar placement
#' @param name name of colorbar
#' @param zmin minimum for colorscale
#' @param zmax maximum for colorscale
#' @param zmid midpoint for colorscale
#' @param title label for y axis
#' @param side side of plot on which to add groups
#' @param size relative size of dendrogram (relative to the main heatmap)
#' @param buffer amount of space to leave empty before this plot, relative to size 
#' of first heatmap
#' @param text text of value to display for data
#' @param tooltip tooltip options, see \code{\link{setup_tooltip_options}}
#' @param show_colorbar show the colorbar?
#' @param show_title show title as axis label
#' @param layout y axis layout parameters to use
#' @seealso \code{\link{iheatmap}}, \code{\link{add_row_groups}}
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @rdname add_col_signal
#' @name add_col_signal
#' @aliases add_col_signal,Iheatmap-method
#' @export
#' @author Alicia Schep
#' @seealso \code{\link{add_row_signal}}, \code{\link{iheatmap}}, 
#' \code{\link{add_col_annotation}}
#' @examples 
#' 
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)  
#' hm <- iheatmap(mat) %>% add_col_signal(signal = 1:5, name = "Strength")
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod(add_col_signal, c(p = "Iheatmap", signal = "ANY"),
          function(p,
                   signal,
                   name,
                   title = name,
                   yname = NULL,
                   xname = current_xaxis(p),
                   pname = name,
                   colorbar_position = get_colorbar_position(p),
                   colors = pick_continuous_colors(zmid, zmin, zmax, p = p),
                   zmin = min(signal, na.rm = TRUE),
                   zmax = max(signal, na.rm = TRUE),
                   zmid = 0,
                   side = c("top","bottom"),
                   size = 0.05,
                   buffer = 0.015,
                   text = signif(signal, digits = 3),
                   tooltip = setup_tooltip_options(),
                   show_colorbar = TRUE,
                   show_title = TRUE,
                   layout = list()){

            side <- match.arg(side)
            new_layout <- col_annotation_heatmap_layout(title, xname, layout, 
                                                        show_title)
            new_y <- new_yaxis(p, xname, layout = new_layout)

            if (is.null(yname)) yname <- id(new_y)

            new_plot <- new("ColumnAnnotation",
                            xaxis = xname,
                            yaxis = yname,
                            colorbar = name,
                            show_colorbar = show_colorbar,
                            data = signal,
                            title = title,
                            text = text,
                            tooltip = tooltip)

            new_colorbar <- continuous_colorbar(name,
                                                colorbar_position,
                                                colors,
                                                zmid,
                                                zmin,
                                                zmax)
            
            p <- add_axis(p,
                          new_y,
                          yname = yname,
                          xname = xname,
                          size = size,
                          buffer = buffer,
                          side = side) 
            p <- add_plot(p, new_plot, pname)
            p <- add_colorbar(p, new_colorbar)
            validObject(p)
            p
            
          })

