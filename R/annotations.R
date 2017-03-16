#' add_row_annotation
#'
#' Adds annotation heatmaps for one or more qualitative or quantitative 
#' annotations for each row of a main heatmap.  
#' @param p \code{link{Iheatmap-class}} object
#' @param annotation data.frame or object that can be converted to data frame
#' @param colors list of color palettes, with one color per annotation column 
#' name
#' @param side side of plot on which to add row annotation
#' @param size relative size of each row annotation
#' @param buffer relative size of buffer between previous subplot and row 
#' annotion
#' @param inner_buffer relative size of buffer between each annotation
#' @param layout layout properties for new x axis
#'
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @rdname add_row_annotation
#' @name add_row_annotation
#' @aliases add_row_annotation,Iheatmap-method
#' @author Alicia Schep
#' @seealso \code{\link{iheatmap}}, \code{\link{add_row_annotation}}, 
#' \code{\link{add_col_signal}}, \code{\link{add_col_groups}}
#' @examples
#'
#' mat <- matrix(rnorm(24), nrow = 6)
#' annotation <- data.frame(gender = c(rep("M", 3),rep("F",3)),
#'                         age = c(20,34,27,19,23,30))
#' hm <- iheatmap(mat) %>% add_row_annotation(annotation)
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod(add_row_annotation,
          c(p = "Iheatmap"),
          function(p,
                   annotation,
                   colors = NULL,
                   side = c("right","left"),
                   size = 0.05,
                   buffer = 0.015,
                   inner_buffer = buffer / 2,
                   layout = list()){
            
            side = match.arg(side)
            # Convert to data.frame
            x <- as.data.frame(annotation)
            
            for (i in seq_len(ncol(x))){
              if (is.character(x[,i]) || is.factor(x[,i]) || is.logical(x[,i])){
                if (!is.null(colors) && colnames(x)[i] %in% names(colors)){
                  tmp_colors <- colors[[colnames(x)[i]]]
                } else{
                  tmp_colors <- pick_discrete_colors(as.factor(x[,i]), p)
                }
                p <- add_row_groups(p, x[,i],
                                    name = colnames(x)[i],
                                    title = colnames(x)[i],
                                    colors = tmp_colors,
                                    side = side,
                                    size = size,
                                    buffer = if (i == 1) 
                                      buffer else inner_buffer,
                                    layout = layout,
                                    show_title = TRUE)
              } else if (is.numeric(x[,i])){
                if (!is.null(colors) && colnames(x)[i] %in% names(colors)){
                  tmp_colors <- colors[[colnames(x)[i]]]
                } else{
                  tmp_colors <- pick_continuous_colors(zmid = 0, 
                                                       zmin = min(x[,i]),
                                                       zmax = max(x[,i]), p)
                }
                p <- add_row_signal(p, 
                                    x[,i],
                                    name = colnames(x)[i],
                                    colors = tmp_colors,
                                    side = side,
                                    size = size,
                                    buffer = if (i == 1) 
                                      buffer else inner_buffer,
                                    layout = layout,
                                    show_title = TRUE)
              } else{
                stop("Input should be character, factor, logical, or numeric")
              }
            }
            return(p)
          })

#' add_col_annotation
#'
#' Adds annotation heatmaps for one or more qualitative or quantitative 
#' annotations for each column of a main heatmap.  
#' @param p \code{link{Iheatmap-class}} object
#' @param annotation data.frame or object that can be converted to data frame
#' @param colors list of color palettes, with one color per annotation column 
#' name
#' @param side side of plot on which to add column annotation
#' @param size relative size of each row annotation
#' @param buffer relative size of buffer between previous subplot and column 
#' annotion
#' @param inner_buffer relative size of buffer between each annotation
#' @param layout layout properties for new y axis
#'
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @rdname add_col_annotation
#' @name add_col_annotation
#' @aliases add_col_annotation,Iheatmap-method
#' @seealso \code{\link{iheatmap}}, \code{\link{add_row_annotation}}, 
#' \code{\link{add_col_signal}}, \code{\link{add_col_groups}}
#' @author Alicia Schep
#' @examples
#'
#' mat <- matrix(rnorm(24), ncol = 6)
#' annotation <- data.frame(gender = c(rep("M", 3),rep("F",3)),
#'                         age = c(20,34,27,19,23,30))
#' hm <- iheatmap(mat) %>% add_col_annotation(annotation)
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod(add_col_annotation,
          c(p = "Iheatmap"),
          function(p,
                   annotation,
                   colors = NULL,
                   side = c("top","bottom"),
                   size = 0.05,
                   buffer = 0.015,
                   inner_buffer = buffer / 2,
                   layout = list()){
            
            side = match.arg(side)
            # Convert to data.frame
            x <- as.data.frame(annotation)
            
            for (i in seq_len(ncol(x))){
              if (is.character(x[,i]) || is.factor(x[,i]) || is.logical(x[,i])){
                if (!is.null(colors) && colnames(x)[i] %in% names(colors)){
                  tmp_colors <- colors[[colnames(x)[i]]]
                } else{
                  tmp_colors <- pick_discrete_colors(as.factor(x[,i]), p)
                }
                p <- add_col_groups(p, 
                                    x[,i],
                                    name = colnames(x)[i],
                                    title = colnames(x)[i],
                                    colors = tmp_colors,
                                    side = side,
                                    size = size,
                                    buffer = if (i == 1)
                                      buffer else inner_buffer,
                                    layout = layout,
                                    show_title = TRUE)
              } else if (is.numeric(x[,i])){
                if (!is.null(colors) && colnames(x)[i] %in% names(colors)){
                  tmp_colors <- colors[[colnames(x)[i]]]
                } else{
                  tmp_colors <- pick_continuous_colors(zmid = 0, 
                                                       zmin = min(x[,i]),
                                                       zmax = max(x[,i]), p)
                }
                p <- add_col_signal(p, 
                                    x[,i],
                                    name = colnames(x)[i],
                                    colors = tmp_colors,
                                    side = side,
                                    size = size,
                                    buffer = if (i == 1)
                                      buffer else inner_buffer,
                                    layout = layout,
                                    show_title = TRUE)
              } else{
                stop("Input should be character, factor, logical, or numeric")
              }
            }
            return(p)
          })
