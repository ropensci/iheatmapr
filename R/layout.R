

#' modify_layout
#'
#' @param x Iheatmap
#' @param new_layout list of new layout parameter
#'
#' @return modified Iheatmap object
#' @export
#' @rdname modify_layout
#' @name modify_layout
#' @aliases modify_layout,Iheatmap-method
#' @examples
#' 
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)  
#' hm <- main_heatmap(mat) %>% modify_layout(list(margin = list(b = 120))) 
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod("modify_layout", signature = list(x = "Iheatmap"),
          function(x, new_layout){
            x@layout <- modifyList(x@layout, new_layout)
            return(x)
          })


setMethod("get_layout", signature = list(x = "Iheatmap"),
          function(x){
            return(x@layout)
          })

setMethod("get_layout", signature = list(x = "IheatmapAxes"),
          function(x){
            out <- lapply(x, get_layout)
            names(out) <- gsub(x@axis,paste0(x@axis,"axis"), id(x))
            return(out)
          })

setMethod("get_layout", signature = list(x = "IheatmapAxis"),
          function(x){
            c(list("domain" = unname(c(domain_start(x),
                                domain_end(x))),
                   "anchor" = x@anchor),
              x@layout)
          })


