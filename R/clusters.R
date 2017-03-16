#' add_row_clusters
#'
#' Add row groups and order rows based on groups
#' @param p iheatmap object
#' @param clusters cluster assignments, should be vector of integers, 
#' characters, or factors
#' @param name name of colorbar indicating cluster membership
#' @param reorder reorder rows based on clusters? default is TRUE
#' @param side side of plot on which to add subplot
#' @param yname name of yaxis
#' @param ... additional arguments to pass to \code{\link{add_row_groups}} for
#'  creation of annotation
#' heatmap indicating cluster membership
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @author Alicia Schep
#' @rdname add_row_clusters
#' @name add_row_clusters
#' @aliases add_row_clusters,Iheatmap-method
#' @seealso \code{\link{add_row_clustering}}, \code{\link{add_col_clusters}}, 
#' \code{\link{iheatmap}}
#' @examples
#'
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)
#' clusters <- c("A","B","A","B")
#' iheatmap(mat)
#' iheatmap(mat) %>% add_row_clusters(clusters)
setMethod(add_row_clusters,
          c(p = "Iheatmap", clusters = "ANY"),
          function(p,
                   clusters,
                   name = "Row<br>Clusters",
                   reorder = TRUE,
                   side = c("left","right"),
                   yname = current_yaxis(p),
                   ...){
            
            side <- match.arg(side)
            
            if (reorder) axis_order(yaxes(p)[[yname]]) <- order(clusters)
            
            p <- add_row_groups(p,
                                clusters,
                                name = name,
                                side = side,
                                yname = yname, 
                                show_title = FALSE,
                                ...)
            validObject(p)
            p
            
          })

#' add_col_clusters
#'
#' Add column groups and order columns based on groups
#' @param p iheatmap object
#' @param clusters cluster assignments, should be vector of integers, 
#' characters, or factors
#' @param name name of colorbar indicating cluster membership
#' @param reorder reorder rows based on clusters? default is TRUE
#' @param side side of plot on which to add subplot
#' @param xname name of xaxis
#' @param ... additional arguments to pass to \code{\link{add_col_groups}} for 
#' creation of annotation heatmap indicating cluster membership
#' 
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @author Alicia Schep
#' @seealso \code{\link{add_row_clusters}}, \code{\link{add_col_clustering}}, 
#' \code{\link{iheatmap}}
#' @rdname add_col_clusters
#' @name add_col_clusters
#' @aliases add_col_clusters,Iheatmap-method
#' @examples
#'
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)
#' clusters <- c("A","B","A","B","A")
#' iheatmap(mat)
#' iheatmap(mat) %>% add_col_clusters(clusters)
setMethod(add_col_clusters,
          c(p = "Iheatmap", clusters = "ANY"),
          function(p,
                   clusters,
                   name = "Col<br>Clusters",
                   reorder = TRUE,
                   side = c("top","bottom"),
                   xname = current_xaxis(p),
                   ...){
            
            side <- match.arg(side)
            
            if (reorder) axis_order(xaxes(p)[[xname]]) <- order(clusters)
            
            p <- add_col_groups(p,
                                clusters,
                                name = name,
                                side = side,
                                xname = xname,
                                show_title = FALSE,
                                ...)
            
            validObject(p)
            p
            
          })

