#' add_row_groups
#'
#' Adds annotation to heatmap indicating what group every row of main heatmap
#' belongs to
#' 
#' @param p \code{\link{Iheatmap-class}} object
#' @param groups vector of group names
#' @param name name of colorbar
#' @param title name of x axis label
#' @param colors palette name or vector of colors
#' @param colorbar_position colorbar placement
#' @param show_colorbar show the colorbar?
#' @param show_title show title as axis label
#' @param side side of plot on which to groups annotation
#' @param layout list of layout parameters for x axis
#' @param size relative size of dendrogram (relative to the main heatmap)
#' @param buffer amount of space to leave empty before this plot, relative to 
#' size of first heatmap
#' @param tooltip tooltip options, see \code{\link{setup_tooltip_options}}
#' @param xname internal name of xaxis
#' @param yname internal name of yaxis
#' @param pname internal name of plot
#' 
#' @seealso \code{\link{iheatmap}}, \code{\link{add_col_groups}}
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @rdname add_row_groups
#' @name add_row_groups
#' @aliases add_row_groups,Iheatmap-method
#' @export
#' @author Alicia Schep
#'
#' @examples
#'
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)
#' row_groups <- c("A","A","B","D")
#' hm <- iheatmap(mat) %>% add_row_groups(row_groups, name = "My Groups")
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod(add_row_groups,
          c(p = "Iheatmap", groups = "ANY"),
          function(p,
                   groups,
                   name = "Row<br>Groups",
                   title = "Groups",
                   colors = pick_discrete_colors(groups,p),
                   colorbar_position = get_colorbar_position(p),
                   show_colorbar = TRUE,
                   show_title = TRUE,
                   side = c("right","left"),
                   layout = list(),
                   size = 0.05,
                   buffer = 0.005,
                   tooltip = setup_tooltip_options(),
                   xname = NULL,
                   yname = current_yaxis(p),
                   pname = name){
            
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
                            data = groups,
                            title = title,
                            text = groups,
                            tooltip = tooltip)
            
            color_levels <- levels(as.factor(groups))
            new_colorbar <- 
              discrete_colorbar(name,
                                colorbar_position,
                                colors,
                                ticktext = color_levels,
                                tickvals = seq_along(color_levels))
            
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


#' add_col_groups
#'
#' Adds annotation to heatmap indicating what group every column of main heatmap
#' belongs to
#' 
#' @param p \code{\link{Iheatmap-class}} object
#' @param groups vector of group names
#' @param name name of colorbar
#' @param title name of x axis label
#' @param colors palette name or vector of colors
#' @param colorbar_position colorbar placement
#' @param show_colorbar show the colorbar?
#' @param show_title show title as axis label
#' @param side side of plot on which to groups annotation
#' @param layout list of layout parameters for x axis
#' @param size relative size of dendrogram (relative to the main heatmap)
#' @param buffer amount of space to leave empty before this plot, relative to 
#' size of first heatmap
#' @param tooltip tooltip options, see \code{\link{setup_tooltip_options}}
#' @param xname internal name of xaxis
#' @param yname internal name of yaxis
#' @param pname internal name of plot
#' 
#' @seealso \code{\link{iheatmap}}, \code{\link{add_row_groups}}
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @rdname add_col_groups
#' @name add_col_groups
#' @aliases add_col_groups,Iheatmap-method
#' @author Alicia Schep
#' @examples
#'
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)
#' col_groups <- c("A","A","B","D","B")
#' hm <- iheatmap(mat) %>% add_col_groups(col_groups, name = "My Groups")
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod(add_col_groups,
          c(p = "Iheatmap", groups = "ANY"),
          function(p,
                   groups,
                   name = "Column<br>Groups",
                   title = "Groups",
                   colors = pick_discrete_colors(groups,p),
                   colorbar_position = get_colorbar_position(p),
                   show_colorbar = TRUE,
                   show_title = TRUE,
                   side = c("top","bottom"),
                   layout = list(),
                   size = 0.05,
                   buffer = 0.015,
                   tooltip = setup_tooltip_options(),
                   xname = current_xaxis(p),
                   yname = NULL,
                   pname = name){
            
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
                            data = groups,
                            title = title,
                            text = groups,
                            tooltip = tooltip)
            
            color_levels <- levels(as.factor(groups))
            new_colorbar <- 
              discrete_colorbar(name,
                                colorbar_position,
                                colors,
                                ticktext = color_levels,
                                tickvals = seq_along(color_levels))
            
            
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
