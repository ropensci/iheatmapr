setMethod("length", "IheatmapList", function(x) length(x@listData))

setAs("IheatmapList", "list", function(from) as.list(from))

setMethod("as.list", "IheatmapList", function(x) {
  x@listData
})

setMethod("[", "IheatmapList", function(x, i) {
  x_subset <- x
  x_subset@listData <- x_subset@listData[i]
  x_subset
})

setReplaceMethod("[", "IheatmapList", function(x, i, value) {
  x@listData[i] <- value
  x
})

setMethod("[[", "IheatmapList", function(x, i) {
  x@listData[[i]]
})

setReplaceMethod("[[", "IheatmapList", function(x, i, value) {
  x@listData[[i]] <- value
  x
})


setMethod("$", "IheatmapList", function(x, name) x[[name, exact=FALSE]])

setReplaceMethod("$", "IheatmapList",
                 function(x, name, value) {
                   x[[name]] <- value
                   x
                 })

setMethod("names", "IheatmapList", function(x) names(x@listData))

setReplaceMethod("names", "IheatmapList",
                 function(x, value) {
                   names(x@listData) <- value
                   x
                 })


setMethod("lapply", "IheatmapList",
          function(X, FUN, ...)
          {
            FUN <- match.fun(FUN)
            ii <- setNames(seq_along(X), names(X))
            lapply(ii, function(i) FUN(X[[i]], ...))
          }
)

setMethod("vapply", "IheatmapList",
          function(X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE)
          {
            FUN <- match.fun(FUN)
            #browser()
            ii <- setNames(seq_along(X), names(X))
            vapply(ii, function(i) do.call(FUN, c(X[[i]], list(...))), FUN.VALUE, USE.NAMES = USE.NAMES)
          }
)



