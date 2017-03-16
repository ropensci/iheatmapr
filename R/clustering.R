#' add_row_clustering
#'
#' @param p iheatmap object
#' @param method "hclust" or "kmeans" for heirarchical or k-means clustering, 
#' respectively
#' @param k number of clusters for rows, needed if order is kmeans or optional 
#' if hclust
#' @param clust_dist distance function to use for clustering if hierarchical 
#' clustering
#' @param groups vector of group assignments
#' @param colors colors to use for annotation of grouping, can be RColorBrewer 
#' palette name or
#' vector of colors
#' @param name name of colorbar indicating cluster membership
#' @param show_colorbar show the colorbar for the heatmap indicating cluster 
#' membership
#' @param xname name of xaxis
#' @param yname name of yaxis
#' @param side side of plot on which to add subplot
#' 
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @rdname add_row_clustering
#' @name add_row_clustering
#' @aliases add_row_clustering,Iheatmap-method
#' @author Alicia Schep
#' @seealso \code{\link{add_col_clustering}}, \code{\link{iheatmap}}
#' @examples
#'
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)
#' iheatmap(mat) %>% add_row_clustering(method = "hclust", k = 2)
setMethod(add_row_clustering,
          c(p = "Iheatmap"),
          function(p,
                   method = c("hclust","kmeans","groups"),
                   name = "Row<br>Clusters",
                   k = NULL,
                   groups = NULL,
                   clust_dist = stats::dist,
                   colors = NULL,
                   show_colorbar = TRUE,
                   side = c("left","right"),
                   xname = NULL,
                   yname = current_yaxis(p)){
            
            method <- match.arg(method)
            side <- match.arg(side)
            
            if (!yaxes(p)[[yname]]@categorical)
              stop("Cannot cluster continuous axis!")
            
            hm <- get_heatmap(p, xname = xname, yname = yname, side = side)
            mat <- get_data(hm)
            
            if (method == "hclust"){
              dendro = fastcluster::hclust(clust_dist(mat))
              if (!is.null(k)){
                groups = stats::cutree(dendro, k = k)
                if (is.null(colors)) colors <- pick_discrete_colors(groups, p)
                p <- add_row_groups(p,
                                    groups, 
                                    name = name, 
                                    colors = colors, 
                                    side = side,
                                    show_colorbar = show_colorbar, 
                                    show_title = FALSE)
              }
              p <- add_row_dendro(p, dendro, side = side)
            } else if (method == "kmeans"){
              if (is.null(k)) stop("No k provided")
              groups = stats::kmeans(mat, centers = k)$cluster
              if (is.null(colors)) colors <- pick_discrete_colors(groups, p)
              p <- add_row_clusters(p,
                                    groups, 
                                    colors = colors, 
                                    name = name,
                                    side = side, 
                                    show_colorbar = show_colorbar)
            } else if (method == "groups"){
              if (is.null(colors)) colors <- pick_discrete_colors(groups, p)
              p <- add_row_clusters(p,
                                    groups, 
                                    colors = colors, 
                                    name = name,
                                    side = side, 
                                    show_colorbar = show_colorbar)
              
            }
            validObject(p)
            p
          })

#' add_col_clustering
#'
#' @param p iheatmap object
#' @param method "hclust" or "kmeans" for heirarchical or k-means clustering, 
#' respectively
#' @param k number of clusters for rows, needed if order is kmeans or optional 
#' if hclust
#' @param clust_dist distance function to use for clustering if hierarchical 
#' clustering
#' @param groups vector of group assignments
#' @param colors colors to use for annotation of grouping, can be RColorBrewer 
#' palette name or
#' vector of colors
#' @param name name of colorbar indicating cluster membership
#' @param show_colorbar show the colorbar for the heatmap indicating cluster 
#' membership
#' @param side side of plot on which to add subplot
#' @param xname name of xaxis
#' @param yname name of yaxis
#' 
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @rdname add_col_clustering
#' @name add_col_clustering
#' @aliases add_col_clustering,Iheatmap-method
#' @author Alicia Schep
#' @seealso \code{\link{add_row_clustering}}, \code{\link{iheatmap}}
#'
#' @examples
#'
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)
#' iheatmap(mat) %>% add_col_clustering(method = "hclust", k = 2)
setMethod(add_col_clustering,
          c(p = "Iheatmap"),
          function(p,
                   method = c("hclust","kmeans","groups"),
                   name = "Col<br>Clusters",
                   k = NULL,
                   groups = NULL,
                   clust_dist = stats::dist,
                   colors = NULL,
                   show_colorbar = TRUE,
                   side = c("top","bottom"),
                   yname = NULL,
                   xname = current_xaxis(p)){
            
            method <- match.arg(method)
            side <- match.arg(side)
            
            if (!xaxes(p)[[xname]]@categorical)
              stop("Cannot cluster continuous axis!")
            
            hm <- get_heatmap(p, xname = xname, yname = yname, side = side)
            mat <- get_data(hm)
            
            if (method == "hclust"){
              dendro = fastcluster::hclust(clust_dist(t(mat)))
              if (!is.null(k)){
                groups = stats::cutree(dendro, k = k)
                if (is.null(colors)) colors <- pick_discrete_colors(groups, p)
                p <- add_col_groups(p,
                                    groups, 
                                    name = name, 
                                    colors = colors, 
                                    side = side, 
                                    show_colorbar = show_colorbar,
                                    show_title = FALSE)
              }
              p <- add_col_dendro(p, dendro, side = side)
            } else if (method == "kmeans"){
              if (is.null(k)) stop("No k provided")
              groups = stats::kmeans(t(mat), centers = k)$cluster
              if (is.null(colors)) colors <- pick_discrete_colors(groups, p)
              p <- add_col_clusters(p,
                                    groups,
                                    name = name,
                                    colors = colors,
                                    side = side,
                                    show_colorbar = show_colorbar)
            } else if (method == "groups"){
              if (is.null(colors)) colors <- pick_discrete_colors(groups, p)
              p <- add_col_clusters(p,
                                    groups,
                                    name = name,
                                    colors = colors,
                                    side = side,
                                    show_colorbar = show_colorbar)
            }
            validObject(p)
            p
          })
