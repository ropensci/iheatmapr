#' add_row_summary
#' 
#' Adds a line plot summarizing the values across rows
#' @param p \code{\link{Iheatmap-class}} object
#' @param groups vector of group labels, name of groups colorbar, or TRUE -- 
#' see Details
#' @param heatmap_name name of a heatmap within the \code{\link{Iheatmap-class}}
#' object
#' @param colors vector of colors or RColorBrewer palette name
#' @param tracename name of trace
#' @param showlegend show legend?
#' @param side side of plot on which to add subplot
#' @param layout xaxis layout list
#' @param size relative size of subplot relative to main heatmap
#' @param buffer amount of space to leave empty before this plot, relative to 
#' size of first heatmap
#' @param xname internal name of xaxis
#' @param yname internal name of yaxis
#' @param type scatter or bar?
#' @param summary_function summary function to use, default is mean, options 
#' are mean, median, sd, var, mad, max, min, and sum
#' @param ... additional arguments to \code{\link{add_row_plot}} or 
#' \code{\link{add_row_barplot}}
#' 
#' @details 
#' If adding the row summary to a horizontally oriented heatmap, the summary 
#' will be based on the right-most heatmap if side is "right" and based on the 
#' left heatmap if side is "left" unless a "heatmap_name" is specified. The 
#' heatmap_name should match the "pname" argument given to a previously added
#' heatmap.
#' 
#' The row summary is based on specific columns if a "groups" argument
#' is given. The groups argument can either be a vector of group assignments for 
#' each row, the "pname" for an existing set of groups incorporated into the 
#' plot using \code{\link{add_col_groups}}, \code{\link{add_col_annotation}}, 
#' \code{\link{add_col_clusters}}, or \code{\link{add_col_clustering}}.  If 
#' groups is set to TRUE, then the function will use an existing set of column 
#' groups added to the plot.  
#' 
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @rdname add_row_summary
#' @name add_row_summary
#' @aliases add_row_summary,Iheatmap-method
#' @seealso \code{\link{add_col_summary}}, \code{\link{iheatmap}}, 
#' \code{\link{add_row_plot}}
#' @author Alicia Schep
#' @examples 
#' 
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)  
#' hm1 <- iheatmap(mat) %>% add_row_summary()
#' hm2 <- iheatmap(mat) %>% add_row_summary(groups = c("A","A","B","B","B"))
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm1
#' if (interactive()) hm2
setMethod(add_row_summary, c(p = "Iheatmap"),
          function(p,
                   groups = NULL,
                   heatmap_name = NULL,
                   colors = NULL,
                   tracename = "Row Summary",
                   showlegend = FALSE,
                   side = c("right","left"), 
                   layout = list(),
                   size = 0.3,
                   buffer = 0.02,
                   xname = NULL,
                   yname = current_yaxis(p),
                   type = c("scatter","bar"),
                   summary_function = c("mean","median","sd","var","mad","max","min","sum"),
                   ...){
            
            side <- match.arg(side)
            type <- match.arg(type)
            summary_function <- match.arg(summary_function)
            
            if (is.null(heatmap_name)){
              hm <- get_heatmap(p, xname = xname, yname = yname, side = side)
            } else{
              if (heatmap_name %ni% names(plots(p)))
                stop(paste("Invalide heatmap_name.", 
                           "Should match pname given for previous main_heatmap",
                           sep = "\n"))
              hm <- plots(p)[[heatmap_name]]
              if (!is(hm,"MainHeatmap"))
                stop("Plot specified by heatmap_name is not a MainHeatmap!")
              if (yaxis_name(hm) != yname){
                warning("yname argument doesn't match y axis of heatmap", 
                               " specified by heatmap_name argument.\nUsing ",
                               "yaxis name from heatmap.")
                yname <- yaxis_name
              }
            }
            
            mat <- get_data(hm)
            
            row_summary_without_groups <- function(p){
              x <- apply(mat, 1, summary_function, na.rm = TRUE)
              p <- if (type == "scatter"){
                add_row_plot(p,
                                x = x, 
                                color = colors[1], 
                                name = tracename, 
                                showlegend = showlegend, 
                                xname = xname, 
                                yname = yname,
                                layout = layout, 
                                size = size, 
                                buffer = buffer,
                                side = side, ...)
              } else{
                add_row_barplot(p,
                             x = x, 
                             color = colors[1], 
                             name = tracename, 
                             showlegend = showlegend, 
                             xname = xname, 
                             yname = yname,
                             layout = layout, 
                             size = size, 
                             buffer = buffer,
                             side = side, ...)
              }
              p
            }
            
            row_summary_with_groups <- function(p, groups, colors){
              for (i in seq_along(levels(groups))){
                sel <- which(groups == levels(groups)[i])
                x <- apply(mat[,sel,drop = FALSE], 1, summary_function, na.rm = TRUE)
                p <- if (type == "scatter"){
                  add_row_plot(p,
                                  x = x, color = colors[i], 
                                  tracename = levels(groups)[i], 
                                  showlegend = showlegend, 
                                  xname = xname, 
                                  yname = yname,
                                  layout = layout, 
                                  size = size, 
                                  buffer = buffer,
                                  side = side, ...)
                } else{
                  add_row_barplot(p,
                               x = x, color = colors[i], 
                               tracename = levels(groups)[i], 
                               showlegend = showlegend, 
                               xname = xname, 
                               yname = yname,
                               layout = layout, 
                               size = size, 
                               buffer = buffer,
                               side = side, ...)
                }
              }
              p
            }
            if (is.null(xname)) xname <- paste0("x", length(xaxes(p)) + 1)
            if (is.null(groups)){
              p <- row_summary_without_groups(p)
            } else if (isTRUE(groups)){
              col_grp_plots <- get_col_groups(p, xname = xaxis_name(hm))
              if (length(col_grp_plots) >= 1){
                if (length(col_grp_plots) > 1) 
                  warning("More than one set of row groups. Selecting first")
                col_grp_plot <- col_grp_plots[[1]]
                cb <- colorbars(p)[[colorbar(col_grp_plot)]]
                groups <- factor(get_data(col_grp_plot), cb@ticktext, 
                                 ordered = TRUE)
                colors <- discrete_colors(nlevels(groups), palette = cb@colors)
                p <- row_summary_with_groups(p, groups, colors)
              } else{
                warning("No column groups found")
                p <- row_summary_without_groups(p)
              }
            } else if (length(groups) == 1 && nrow(mat)!=1){
              col_grp_plots <- get_col_groups(p, xname = xaxis_name(hm))
              if (groups %ni% names(col_grp_plots)){
                warning("No row groups matching given name found")
                p <- row_summary_without_groups(p)
              } else{
                col_grp_plot <- col_grp_plots[[groups]]
                cb <- colorbars(p)[[colorbar(col_grp_plot)]]
                groups <- factor(get_data(col_grp_plot), 
                                 cb@ticktext, ordered = TRUE)
                colors <- discrete_colors(nlevels(groups), palette = cb@colors)
                p <- row_summary_with_groups(p, groups, colors)
              }
            } else{
              groups <- as.factor(groups)
              if (is.null(colors)){ 
                colors <- pick_row_summary_colors(p, groups, xname, yname, side)
              } 
              colors <- discrete_colors(nlevels(groups), palette = colors)
              p <- row_summary_with_groups(p, groups, colors)
            }
            validObject(p)
            p
            
          })

#' add_col_summary
#' 
#' Adds a line plot summarizing the values across columns
#' @param p \code{\link{Iheatmap-class}} object
#' @param groups vector of group labels, name of groups colorbar, or TRUE -- 
#' see Details
#' @param heatmap_name name of a heatmap within the plot 
#' @param colors vector of colors or RColorBrewer palette name
#' @param tracename name of trace
#' @param showlegend show legend?
#' @param side side of plot on which to add subplot
#' @param layout xaxis layout list
#' @param size relative size of subplot relative to main heatmap
#' @param buffer amount of space to leave empty before this plot, relative to 
#' size of first heatmap
#' @param xname internal name of xaxis
#' @param yname internal name of yaxis
#' @param type scatter or bar?
#' @param summary_function summary function to use, default is mean, options 
#' are mean, median, sd, var, mad, max, min, and sum
#' @param ... additional arguments to \code{\link{add_col_plot}} or 
#' \code{\link{add_col_barplot}}
#' 
#' @details 
#' If adding the column summary to a vertically oriented heatmap, the summary 
#' will be based on the topmost heatmap if side is "top" and based on the bottom
#' heatmap if side is "bottom" unless a "heatmap_name" is specified. The 
#' heatmap_name should match the "pname" argument given to a previously added
#' heatmap.
#' 
#' The column summary is based on specific rows if a "groups" argument
#' is given. The groups argument can either be a vector of group assignments for 
#' each row, the "pname" for an existing set of groups incorporated into the 
#' plot using \code{\link{add_row_groups}}, \code{\link{add_row_annotation}}, 
#' \code{\link{add_row_clusters}}, or \code{\link{add_row_clustering}}.  If 
#' groups is set to TRUE, then the function will use an existing set of row 
#' groups added to the plot.  
#' 
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @rdname add_col_summary
#' @name add_col_summary
#' @aliases add_col_summary,Iheatmap-method
#' @export
#' @author Alicia Schep
#' @seealso \code{\link{add_row_summary}}, \code{\link{iheatmap}}, 
#' \code{\link{add_col_plot}}
#' @examples 
#' 
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)  
#' hm1 <- iheatmap(mat) %>% add_col_summary()
#' hm2 <- iheatmap(mat) %>% add_col_summary(groups = c("A","A","B","B"))
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm1
#' if (interactive()) hm2
setMethod(add_col_summary, c(p = "Iheatmap"),
          function(p,
                   groups = NULL,
                   heatmap_name = NULL,
                   colors = NULL,
                   tracename = "Col Summary",
                   showlegend = FALSE,
                   side = c("top","bottom"), 
                   layout = list(),
                   size = 0.3,
                   buffer = 0.02,
                   xname = current_xaxis(p),
                   yname = NULL,
                   type = c("scatter","bar"),
                   summary_function = c("mean","median","sd","var","mad","max","min","sum"),
                   ...){
            
            side <- match.arg(side)
            type <- match.arg(type)
            summary_function <- match.arg(summary_function)
            
            if (is.null(heatmap_name)){
              hm <- get_heatmap(p, xname = xname, yname = yname, side = side)
            } else{
              if (heatmap_name %ni% names(plots(p)))
                stop(paste("Invalide heatmap_name.", 
                           "Should match pname given for previous main_heatmap",
                           sep = "\n"))
              hm <- plots(p)[[heatmap_name]]
              if (!is(hm,"MainHeatmap"))
                stop("Plot specified by heatmap_name is not a MainHeatmap!")
              if (xaxis_name(hm) != xname){
                warning("xname argument doesn't match x axis of ",
                               "heatmap specified by heatmap_name argument.",
                               "\nUsing xaxis name from heatmap.")
                xname <- xaxis_name
              }
            }
            
            mat <- get_data(hm)
            
            col_summary_without_groups <- function(p){
              y <- apply(mat, 2, summary_function, na.rm = TRUE)
              
              p <- if (type == "scatter"){
                add_col_plot(p,
                                y = y, 
                                color = colors[1], 
                                name = tracename, 
                                showlegend = showlegend, 
                                xname = xname, 
                                yname = yname,
                                layout = layout, 
                                size = size, 
                                buffer = buffer,
                                side = side, ...)
              } else if (type == "bar"){
                add_col_barplot(p,
                             y = y, 
                             color = colors[1], 
                             name = tracename, 
                             showlegend = showlegend, 
                             xname = xname, 
                             yname = yname,
                             layout = layout, 
                             size = size, 
                             buffer = buffer,
                             side = side, ...)
              }
              p
            }
            
            col_summary_with_groups <- function(p, groups, colors){
              for (i in seq_along(levels(groups))){
                sel <- which(groups == levels(groups)[i])
                y <- apply(mat[sel,,drop = FALSE], 2, summary_function, na.rm = TRUE)
                p <- if (type == "scatter"){
                  add_col_plot(p,
                                  y = y, 
                                  color = colors[i], 
                                  tracename = levels(groups)[i], 
                                  showlegend = showlegend, 
                                  xname = xname, 
                                  yname = yname,
                                  layout = layout, 
                                  size = size, 
                                  buffer = buffer,
                                  side = side, ...)
                } else{
                  add_col_barplot(p,
                               y = y, 
                               color = colors[i], 
                               tracename = levels(groups)[i], 
                               showlegend = showlegend, 
                               xname = xname, 
                               yname = yname,
                               layout = layout, 
                               size = size, 
                               buffer = buffer,
                               side = side, ...)
                }
              }
              p
            }
            if (is.null(yname)) yname <- paste0("y", length(yaxes(p)) + 1)
            if (is.null(groups)){
              p <- col_summary_without_groups(p)
            } else if (isTRUE(groups)){
              row_grp_plots <- get_row_groups(p, yname = yaxis_name(hm))
              if (length(row_grp_plots) >= 1){
                if (length(row_grp_plots) > 1) 
                  warning("More than one set of row groups. Selecting first")
                row_grp_plot <- row_grp_plots[[1]]
                cb <- colorbars(p)[[colorbar(row_grp_plot)]]
                groups <- factor(get_data(row_grp_plot), cb@ticktext, 
                                 ordered = TRUE)
                colors <- discrete_colors(nlevels(groups), palette = cb@colors)
                p <- col_summary_with_groups(p, groups, colors)
              } else{
                warning("No row groups found")
                p <-col_summary_without_groups(p)
              }
            } else if (length(groups) == 1 && nrow(mat)!=1){
              row_grp_plots <- get_row_groups(p, yname = yaxis_name(hm))
              if (groups %ni% names(row_grp_plots)){
                warning("No row groups matching given name found")
                p <- col_summary_without_groups(p)
              } else{
                row_grp_plot <- row_grp_plots[[groups]]
                cb <- colorbars(p)[[colorbar(row_grp_plot)]]
                groups <- factor(get_data(row_grp_plot), 
                                 cb@ticktext, ordered = TRUE)
                colors <- discrete_colors(nlevels(groups), palette = cb@colors)
                p <- col_summary_with_groups(p, groups, colors)
              }
            } else{
              groups <- as.factor(groups)
              if (is.null(colors)){ 
                colors <- pick_col_summary_colors(p, groups, xname, yname, side)
              } 
              colors <- discrete_colors(nlevels(groups), palette = colors)
              p <- col_summary_with_groups(p, groups, colors)
            }
            validObject(p)
            p
            
            
})









summary_colors_helper <- function(p, groups, cands){
  if (length(cands) == 0){
    return(pick_discrete_colors(groups, p))
  } else if (length(cands) == 1){
    if (all(as.character(groups) == as.character(get_data(cands[[1]]))) &&
        all(levels(groups) == 
            levels(colorbars(p)[[cands[[1]]@colorbar]]@ticktext))){
      return(colorbars(p)[[cands[[1]]@colorbar]]@colors)
    } else{
      return(pick_discrete_colors(groups, p))
    }
  } else{
    cands <- cands[vapply(cands, function(x){
      all(as.character(groups) == as.character(get_data(x))) &&
        all(levels(groups) == levels(colorbars(p)[[x@colorbar]]@ticktext))
    }, FALSE)]
    if (length(cands == 1)){
      return(colorbars(p)[[cands[[1]]@colorbar]]@colors)
    } else{
      return(pick_discrete_colors(groups, p))
    }
  }
}

pick_row_summary_colors <- function(p, groups, xname, yname, side){
  
  hm <- get_heatmap(p, xname = xname, yname = yname, side = side)
  
  cands <- get_col_groups(p, xname = xaxis_name(hm))
  
  summary_colors_helper(p, groups, cands)
}


pick_col_summary_colors <- function(p, groups, xname, yname, side){
  
  hm <- get_heatmap(p, xname = xname, yname = yname, side = side)
  
  cands <- get_row_groups(p, yname = yaxis_name(hm))
  
  summary_colors_helper(p, groups, cands)
}



