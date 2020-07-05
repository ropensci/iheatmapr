#' S4 List Utils for Iheatmap classes
#' 
#' These are utility methods for list-like classes in the package.
#' @name iheatmap_list_utils
#' @rdname iheatmap_list_utils
#' @param x input
#' @docType methods
#' @aliases length,IheatmapList-method
#' as.list,IheatmapList-method
#' names,IheatmapList-method
#' `names<-`,IheatmapList-method
#' `$`,IheatmapList-method
#' `$<-`,IheatmapList-method
#' `[`,IheatmapList-method
#' `[<-`,IheatmapList-method
#' `[[`,IheatmapList-method
#' `[[<-`,IheatmapList-method
#' lapply,IheatmapList-method
#' vapply,IheatmapList-method
#' @importFrom stats setNames
#' @keywords internal

setMethod("length", "IheatmapList", function(x) length(x@listData))

setAs("IheatmapList", "list", function(from) as.list(from))

#' @rdname iheatmap_list_utils
setMethod("as.list", "IheatmapList", function(x) {
  x@listData
})

#' @rdname iheatmap_list_utils
setMethod("[", "IheatmapList", function(x, i) {
  x_subset <- x
  x_subset@listData <- x_subset@listData[i]
  x_subset
})

#' @rdname iheatmap_list_utils
setReplaceMethod("[", "IheatmapList", function(x, i, value) {
  x@listData[i] <- value
  x
})

#' @rdname iheatmap_list_utils
setMethod("[[", "IheatmapList", function(x, i) {
  x@listData[[i]]
})

#' @rdname iheatmap_list_utils
setReplaceMethod("[[", "IheatmapList", function(x, i, value) {
  x@listData[[i]] <- value
  x
})

#' @rdname iheatmap_list_utils
setMethod("$", "IheatmapList", function(x, name) x[[name, exact=FALSE]])

#' @rdname iheatmap_list_utils
setReplaceMethod("$", "IheatmapList",
                 function(x, name, value) {
                   x[[name]] <- value
                   x
                 })

#' @rdname iheatmap_list_utils
setMethod("names", "IheatmapList", function(x) names(x@listData))

#' @rdname iheatmap_list_utils
setReplaceMethod("names", "IheatmapList",
                 function(x, value) {
                   names(x@listData) <- value
                   x
                 })

#' @rdname iheatmap_list_utils
#' @param FUN function to apply to each element of x
setMethod("lapply", "IheatmapList",
          function(X, FUN, ...)
          {
            FUN <- match.fun(FUN)
            ii <- setNames(seq_along(X), names(X))
            lapply(ii, function(i) FUN(X[[i]], ...))
          }
)

#' @rdname iheatmap_list_utils
#' @param FUN.VALUE template for return value from FUN
#' @param USE.NAMES logical, use names?
#' @param ... additional arguments
setMethod("vapply", "IheatmapList",
          function(X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE)
          {
            FUN <- match.fun(FUN)
            ii <- setNames(seq_along(X), names(X))
            vapply(ii, function(i) do.call(FUN, c(X[[i]], list(...))), FUN.VALUE, USE.NAMES = USE.NAMES)
          }
)



