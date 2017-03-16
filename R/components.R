#' Access subcomponents of Iheatmap object
#' 
#' These are methods for accessing subcomponents of the Iheatmap object
#' @name access_component
#' @rdname access_component
#' @param x \code{\link{Iheatmap-class}} object
#' @param xaxis name of xaxis
#' @param yaxis name of yaxis
#' @docType methods
#' @aliases yaxes,Iheatmap-method xaxes,Iheatmap-method plots,Iheatmap-method
#' shapes,Iheatmap-method, annotations,Iheatmap-method yaxes xaxes plots shapes
#' annotations colorbars colorbars,Iheatmap-method 
#' colorbars,IheatmapColorbars-method colorbars,IheatmapPlots-method
#' @keywords internal
NULL


# x and y axes -----------------------------------------------------------------


#' @rdname access_component
#' @export
setMethod('yaxes', c(p = "Iheatmap"),
          function(p, xaxis = NULL){
            if (is.null(xaxis)){
              p@yaxes
            } else{
              ids = unique(yaxis_name(p)[which(xaxis_name(p) %in% xaxis)])
              p@yaxes[ids]
            }
          })

#' @rdname access_component
#' @export
setMethod('xaxes', c(p = "Iheatmap"),
          function(p, yaxis = NULL){
            if (is.null(yaxis)){
              p@xaxes
            } else{
              ids = unique(xaxis_name(p)[which(yaxis_name(p) %in% yaxis)])
              p@xaxes[ids]
            }
          })


setReplaceMethod("xaxes", c(p = "Iheatmap"),
                 function(p, value){p@xaxes <- value; p})


setReplaceMethod("yaxes", c(p = "Iheatmap"),
                 function(p, value){p@yaxes <- value; p})


# plots ------------------------------------------------------------------------

#' @rdname access_component
#' @export
setMethod(plots, c(x = "Iheatmap"),
          function(x){x@plots})

setReplaceMethod("plots", c(x = "Iheatmap"),
                 function(x, value){x@plots <- value; x})

setMethod(add_plot, c(p = "Iheatmap", new_plot = "IheatmapPlot"),
          function(p, new_plot, name){
            name <- pname_check(name, p)
            plots(p)[[name]] <- new_plot
            p
          })

setMethod(get_data, c(x = "IheatmapPanel"),
          function(x) x@data)

setMethod(get_title, c(x = "RowAnnotation"),
          function(x) x@title)

setMethod(get_title, c(x = "ColumnAnnotation"),
          function(x) x@title)

# shapes -----------------------------------------------------------------------

#' @rdname access_component
#' @export
setMethod(shapes, c(x = "Iheatmap"),
          function(x){x@shapes})

setReplaceMethod("shapes", c(x = "Iheatmap"),
                 function(x, value){x@shapes <- value; x})

setMethod(add_shape, c(p = "Iheatmap", new_shape = "IheatmapShape"),
          function(p, new_shape, name){
            name <- sname_check(name, p )
            shapes(p)[[name]] <- new_shape
            p
          })

# annotations ------------------------------------------------------------------

#' @rdname access_component
#' @export
setMethod(annotations, c(x = "Iheatmap"),
          function(x){x@annotations})

setReplaceMethod("annotations", c(x = "Iheatmap"),
                 function(x, value){x@annotations <- value; x})

setMethod(add_annotation, c(p = "Iheatmap", new_anno = "IheatmapAnnotation"),
          function(p, new_anno, aname){
            aname <- aname_check(aname, p)
            annotations(p)[[aname]] <- new_anno
            p
          })