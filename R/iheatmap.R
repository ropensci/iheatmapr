#' iheatmap
#' 
#' Make a farily standard interactive heatmap with optional clustering and 
#' row and column annotations.  For more flexibility and options, see the
#' \code{\link{main_heatmap}} function and other modular functions as described
#'  in vignette.
#' @param data matrix of values to be plotted as heatmap
#' @param x x xaxis labels, by default colnames of data
#' @param y y axis labels, by default rownames of data
#' @param cluster_rows "none","hclust", or "k-means" for no clustering, 
#' hierarchical clustering, and k-means clustering of rows respectively
#' @param cluster_cols "none","hclust", or "k-means" for no clustering, 
#' hierarchical clustering, and k-means clustering of columnsrespectively
#' @param row_clusters vector of pre-determined row cluster assignment
#' @param col_clusters vector of pre-determined column cluster assignment
#' @param row_k number of clusters for rows, needed if cluster_rows is kmeans or 
#' optional if hclust
#' @param col_k number of clusters for columns, needed if cluster_rows is kmeans
#'  or optional if hclust
#' @param row_clust_dist distance function to use for row clustering if 
#' hierarchical clustering
#' @param col_clust_dist distance function to use for column clustering 
#' if hierarchical clustering
#' @param name Name for colorbar
#' @param scale scale matrix by rows, cols or none
#' @param scale_method what method to use for scaling, either none, standardize, 
#' center, normalize
#' @param colors name of RColorBrewer palette or vector of colors for main 
#' heatmap
#' @param row_annotation row annotation data.frame
#' @param col_annotation column annotation data.frame
#' @param col_clusters_colors colors for col clusters annotation heatmap
#' @param row_clusters_colors colors for row clusters annotation heatmap
#' @param row_clusters_name name for row clusters colorbar
#' @param col_clusters_name name for col clusters colorbar
#' @param show_row_clusters_colorbar show the colorbar for row clusters?
#' @param show_col_clusters_colorbar show the colorbar for column clusters?
#' @param col_annotation_colors list of colors for col annotations heatmap
#' @param row_annotation_colors list of colors for row annotations heatmap
#' @param colorbar_grid colorbar grid parameters, should be result from 
#' \code{\link{setup_colorbar_grid}} 
#' @param layout list of layout attributes to pass to plotly, 
#' eg. list(font = list(size = 15))
#' @param row_labels axis labels for y axis 
#' @param col_labels axis labels for x axis 
#' @param row_title x axis title
#' @param col_title y axis title
#' @param source source name for use with shiny
#' @param ... additional argument to iheatmap
#' @details 
#' By default, no scaling is done of rows or columns. This can be changed by 
#' specifying the 'scale' argument.  There are three options for scaling 
#' methods. "standardize" subtracts the mean and divides by standard deviation,
#' "center" just subtracts the mean, and "normalize" divides by the sum of the 
#' values.  "normalize" should only be used for data that is all positive!  If 
#' alternative scaling is desired, the scaling should be done prior to calling 
#' the iheatmap function.
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @author Alicia Schep
#' 
#' @seealso \code{\link{iheatmap}}, \code{\link{add_iheatmap}}, 
#' \code{\link{to_widget}}
#' @include main_heatmap.R
#' @rdname iheatmap
#' @name iheatmap
#' @aliases iheatmap,matrix-method
#' @examples 
#' mat <- matrix(rnorm(24), nrow = 6)
#' annotation = data.frame(gender = c(rep("M", 3),rep("F",3)),
#'  age = c(20,34,27,19,23,30))
#' hm <- iheatmap(mat, 
#'  cluster_rows = "hclust",
#'  cluster_cols = "kmeans", 
#'  col_k = 3, 
#'  row_annotation = annotation)
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod("iheatmap", c(data = "matrix"),
          function(data, 
                   x = default_x(data),
                   y = default_y(data),                   
                   cluster_rows = c("none","hclust","kmeans"),
                   cluster_cols = c("none","hclust","kmeans"),
                   row_clusters = NULL,
                   col_clusters = NULL,
                   row_k = NULL,
                   col_k = NULL,
                   row_clust_dist = stats::dist,
                   col_clust_dist = stats::dist,
                   name = "Signal",
                   scale = c("none","rows","cols"),
                   scale_method = c("standardize","center","normalize"),
                   colors = NULL,
                   col_clusters_colors = NULL,
                   col_clusters_name = "Col<br>Clusters",
                   row_clusters_colors = NULL,
                   row_clusters_name = "Row<br>Clusters",
                   show_row_clusters_colorbar = TRUE,
                   show_col_clusters_colorbar = TRUE,
                   row_annotation = NULL,
                   col_annotation = NULL,
                   row_annotation_colors = NULL,
                   col_annotation_colors = NULL,
                   row_labels = NULL,
                   col_labels = NULL,
                   row_title = NULL,
                   col_title = NULL,
                   colorbar_grid = setup_colorbar_grid(),
                   layout = list(),
                   source = "iheatmapr",
                   ...){
            
            cluster_rows <- match.arg(cluster_rows)
            cluster_cols <- match.arg(cluster_cols)
            scale <- match.arg(scale)
            scale_method <- match.arg(scale_method)
            
            if (scale != "none") data <- scale_mat(data, 
                                                   scale = scale, 
                                                   scale_method = scale_method)
            
            ## Build plot
            if (is.null(colors)) 
              colors <- pick_continuous_colors(0, 
                                               min(data, na.rm = TRUE), 
                                               max(data, na.rm = TRUE))
            
            p <- main_heatmap(data, 
                              x = x, 
                          y = y, 
                          name = name, 
                          colors = colors,
                          colorbar_grid = colorbar_grid,
                          layout = layout,
                          source = source,
                          ...) 
            
            if (cluster_rows != "none"){
              p <- 
                add_row_clustering(p,
                                   method = cluster_rows, 
                                   name = row_clusters_name, 
                                   k = row_k, 
                                   groups = row_clusters, 
                                   colors = row_clusters_colors,
                                   show_colorbar = show_col_clusters_colorbar)
            }
            
            if (cluster_cols != "none"){
              p <- 
                add_col_clustering(p,
                                   method = cluster_cols, 
                                   name = col_clusters_name,
                                   k = col_k, 
                                   groups = col_clusters, 
                                   colors = col_clusters_colors,
                                   show_colorbar = show_col_clusters_colorbar)
            }
            
            if (!is.null(row_annotation)){
              p <- 
                add_row_annotation(p,
                                   row_annotation, 
                                   colors = row_annotation_colors)
            }
            
            if (!is.null(col_annotation)){
              p <- 
                add_col_annotation(p,
                                   col_annotation, 
                                   colors = col_annotation_colors, 
                                   side = "bottom")
            }
            
            if (!is.null(row_labels)){
              if (isTRUE(row_labels)){
                p <- add_row_labels(p, side = "right")
              } else if (row_labels) {
                p <- add_row_labels(p, ticktext = row_labels, 
                                          side = "right")
              }
            }
            if (!is.null(col_labels)){
              if (isTRUE(col_labels)){
                p <- add_col_labels(p)
              } else if (col_labels) {
                p <- add_col_labels(p, ticktext = col_labels)
              }
            }
            if (!is.null(col_title)){
              p <- add_col_title(p, col_title)
            }
            if (!is.null(row_title)){
              p <- add_row_title(p, row_title, side = "left")
            }
            validObject(p)
            p
          })

#' add_iheatmap
#' 
#' @param p iheatmap object
#' @param data matrix of values to be plotted as heatmap
#' @param x x xaxis labels, by default colnames of data
#' @param y y axis labels, by default rownames of data
#' @param cluster_rows "none","hclust", or "k-means" for no clustering, 
#' hierarchical clustering, and k-means clustering of rows respectively
#' @param cluster_cols "none","hclust", or "k-means" for no clustering, 
#' hierarchical clustering, and k-means clustering of columnsrespectively
#' @param row_clusters vector of pre-determined row cluster assignment
#' @param col_clusters vector of pre-determined column cluster assignment
#' @param row_k number of clusters for rows, needed if cluster_rows is kmeans or 
#' optional if hclust
#' @param col_k number of clusters for columns, needed if cluster_rows is kmeans 
#' or optional if hclust
#' @param row_clust_dist distance function to use for row clustering if 
#' hierarchical clustering
#' @param col_clust_dist distance function to use for column clustering if 
#' hierarchical clustering
#' @param name Name for colorbar
#' @param scale scale matrix by rows, cols or none
#' @param scale_method what method to use for scaling, either standardize, 
#' center, normalize
#' @param colors name of RColorBrewer palette or vector of colors for main heatmap
#' @param row_annotation row annotation data.frame
#' @param col_annotation column annotation data.frame
#' @param col_clusters_colors colors for col clusters annotation heatmap
#' @param row_clusters_colors colors for row clusters annotation heatmap
#' @param row_clusters_name name for row clusters colorbar
#' @param col_clusters_name name for col clusters colorbar
#' @param show_row_clusters_colorbar show the colorbar for row clusters?
#' @param show_col_clusters_colorbar show the colorbar for column clusters?
#' @param col_annotation_colors list of colors for col annotations heatmap
#' @param row_annotation_colors list of colors for row annotations heatmap
#' @param row_labels axis labels for y axis 
#' @param col_labels axis labels for x axis 
#' @param row_title x axis title
#' @param col_title y axis title
#' @param buffer amount of space to leave empty before this plot, relative to 
#' size of first heatmap
#' @param ... additional argument to add_iheatmap
#' @export
#' @details 
#' By default, no scaling is done of rows or columns. This can be changed by 
#' specifying the 'scale' argument.  There are three options for scaling 
#' methods. "standardize" subtracts the mean and divides by standard deviation,
#' "center" just subtracts the mean, and "normalize" divides by the sum of the 
#' values.  "normalize" should only be used for data that is all positive!  If 
#' alternative scaling is desired, the scaling should be done prior to calling 
#' the iheatmap function.
#' @author Alicia Schep
#' @rdname add_iheatmap
#' @name add_iheatmap
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @aliases add_iheatmap,IheatmapHorizontal,matrix-method 
#' add_iheatmap,IheatmapVertical,matrix-method
#' @seealso \code{\link{iheatmap}}, \code{\link{main_heatmap}}
#' @examples 
#' 
#' mat <- matrix(rnorm(24), nrow = 6)
#' mat2 <- matrix(rnorm(24), nrow = 6)
#' annotation = data.frame(gender = c(rep("M", 3),rep("F",3)),
#'                         age = c(20,34,27,19,23,30))
#' hm <- iheatmap(mat, 
#'  cluster_rows = "hclust", 
#'  cluster_cols = "hclust", 
#'  col_k = 3) %>%
#' add_iheatmap(mat2, 
#'  cluster_cols = "hclust", 
#'  col_k = 3, 
#'  row_annotation = annotation)
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod("add_iheatmap", c(p = "IheatmapHorizontal", data = "matrix"),
          function(p,
                   data, 
                   x = default_x(data),
                   cluster_cols = c("none","hclust","kmeans","groups"),
                   col_clusters = NULL,
                   col_k = NULL,
                   col_clust_dist = stats::dist,
                   name = "Signal",
                   scale = c("none","rows","cols"),
                   scale_method = c("standardize","center","normalize"),
                   colors = NULL,
                   col_clusters_colors = NULL,
                   col_clusters_name = "Col<br>Clusters",
                   show_col_clusters_colorbar = TRUE,
                   row_annotation = NULL,
                   col_annotation = NULL,
                   row_annotation_colors = NULL,
                   col_annotation_colors = NULL,
                   row_labels = NULL,
                   col_labels = NULL,
                   row_title = NULL,
                   col_title = NULL,
                   buffer = 0.2,
                   ...){
            
            
            cluster_cols <- match.arg(cluster_cols)
            scale <- match.arg(scale)
            scale_method <- match.arg(scale_method)
            
            if (scale != "none") data <- scale_mat(data, scale = scale, 
                                                   scale_method = scale_method)
            
            ##  Add plot items
            if (is.null(colors)) 
              colors <- pick_continuous_colors(0, min(data, na.rm = TRUE), 
                                               max(data, na.rm = TRUE), p)
            
            p <- add_main_heatmap(p,
                                  data, 
                                  x = x, 
                                  name = name, 
                                  buffer = buffer,
                                  colors = colors,
                                  ...) 
            
            if (cluster_cols != "none"){
              p <- 
                add_col_clustering(p,
                                   method = cluster_cols, 
                                   name = col_clusters_name, 
                                   k = col_k, 
                                   groups = col_clusters,
                                   colors = col_clusters_colors,
                                   show_colorbar = show_col_clusters_colorbar)
            }
            
            if (!is.null(row_annotation)){
              p <- add_row_annotation(p,
                                      row_annotation, 
                                      colors = row_annotation_colors)
            }
            
            if (!is.null(col_annotation)){
              p <- add_col_annotation(p,
                                      col_annotation, 
                                      colors = col_annotation_colors, 
                                      side = "bottom")
            }
            
            if (!is.null(row_labels)){
              if (isTRUE(row_labels)){
                p <- add_row_labels(p, side = "right")
              } else if (row_labels) {
                p <- add_row_labels(p, ticktext = row_labels,
                                          side = "right")
              }
            }
            if (!is.null(col_labels)){
              if (isTRUE(col_labels)){
                p <- add_col_labels(p)
              } else if (col_labels) {
                p <- add_col_labels(p, ticktext = col_labels)
              }
            }
            if (!is.null(col_title)){
              p <- add_col_title(p, col_title)
            }
            if (!is.null(row_title)){
              p <- add_row_title(p, row_title, side = "left")
            }  
            validObject(p)
            p
          })

#' @rdname add_iheatmap
setMethod("add_iheatmap", c(p = "IheatmapVertical", data = "matrix"),
          function(p,
                   data, 
                   y = default_y(data),
                   cluster_rows = c("none","hclust","kmeans","groups"),
                   row_clusters = NULL,
                   row_k = NULL,
                   row_clust_dist = stats::dist,
                   name = "Signal",
                   scale = c("none","rows","cols"),
                   scale_method = c("standardize","center","normalize"),
                   colors = NULL,
                   row_clusters_colors = NULL,
                   row_clusters_name = "Col<br>Clusters",
                   show_row_clusters_colorbar = TRUE,
                   row_annotation = NULL,
                   col_annotation = NULL,
                   row_annotation_colors = NULL,
                   col_annotation_colors = NULL,
                   row_labels = NULL,
                   col_labels = NULL,
                   row_title = NULL,
                   col_title = NULL,
                   buffer = 0.2,
                   ...){
            
            
            cluster_rows <- match.arg(cluster_rows)
            scale <- match.arg(scale)
            scale_method <- match.arg(scale_method)
            
            if (scale != "none") data <- scale_mat(data, scale = scale, 
                                                   scale_method = scale_method)
            
            ##  Add plot items
            if (is.null(colors)) 
              colors <- pick_continuous_colors(0, min(data, na.rm = TRUE), 
                                               max(data, na.rm = TRUE), p)
            
            p <- add_main_heatmap(p,
                                  data, 
                                  y = y, 
                                  name = name, 
                                  buffer = buffer,
                                  colors = colors,
                                  ...) 
            
            if (cluster_rows != "none"){
              p <-  
                add_row_clustering(p,
                                   method = cluster_rows, 
                                   name = row_clusters_name, 
                                   k = row_k, 
                                   groups = row_clusters,
                                   colors = row_clusters_colors,
                                   show_colorbar = show_row_clusters_colorbar)
            }
            
            if (!is.null(row_annotation)){
              p <- add_row_annotation(p,
                                      row_annotation, 
                                      colors = row_annotation_colors)
            }
            
            if (!is.null(col_annotation)){
              p <- add_col_annotation(p,
                                      col_annotation, 
                                      colors = col_annotation_colors, 
                                      side = "bottom")
            }
            
            if (!is.null(row_labels)){
              if (isTRUE(row_labels)){
                p <- add_row_labels(p, side = "right")
              } else if (row_labels) {
                p <- add_row_labels(p, ticktext = row_labels, 
                                          side = "right")
              }
            }
            if (!is.null(col_labels)){
              if (isTRUE(col_labels)){
                p <- add_col_labels(p)
              } else if (col_labels) {
                p <- add_col_labels(p, ticktext = col_labels)
              }
            }
            if (!is.null(col_title)){
              p <- add_col_title(p, col_title)
            }
            if (!is.null(row_title)){
              p <- add_row_title(p, row_title, side = "left")
            }  
            validObject(p)
            p
            
          })