## Utility methods for adding components to plot


#' Adding plot components to iheatmapr
#' 
#' These are generic methods for adding new plot components to an 
#' \code{link{Iheatmap-class}} object. Not intended for end users; exported for
#' developers seeking to create new Iheatmap subplots.
#' @name add_component
#' @rdname add_component
#' @param p \code{\link{Iheatmap-class}} object
#' @param name internal name
#' @docType methods
#' @aliases add_annotation,Iheatmap,IheatmapAnnotation-method
#' add_axis,IheatmapHorizontal,IheatmapX-method
#' add_axis,IheatmapHorizontal,IheatmapY-method
#' add_axis,IheatmapVertical,IheatmapX-method
#' add_axis,IheatmapVertical,IheatmapY-method
#' add_colorbar,Iheatmap,ContinuousColorbar-method
#' add_colorbar,Iheatmap,DiscreteColorbar-method
#' add_plot,Iheatmap,IheatmapPlot-method
#' add_shape,Iheatmap,IheatmapShape-method
#' @keywords internal
NULL

#' @rdname add_component
#' @param new_axis new \code{\link{IheatmapAxis-class}} object
#' @export
setGeneric("add_axis", function(p, new_axis, ...) standardGeneric("add_axis"))

#' @rdname add_component
#' @param new_colorbar new \code{\link{IheatmapColorbar-class}} object
#' @export
setGeneric("add_colorbar", function(p, new_colorbar, ...) 
           standardGeneric("add_colorbar"))

#' @rdname add_component
#' @param new_plot new \code{\link{IheatmapPlot-class}} object
#' @export
setGeneric("add_plot", function(p, new_plot, ...) standardGeneric("add_plot"))

#' @rdname add_component
#' @param new_shape new \code{\link{IheatmapShape-class}} object
#' @export
setGeneric("add_shape", 
           function(p, new_shape, ...) standardGeneric("add_shape"))

#' @rdname add_component
#' @param new_anno new \code{\link{IheatmapAnnotation-class}} object
#' @export
setGeneric("add_annotation", 
           function(p, new_anno, ...) standardGeneric("add_annotation"))


### Adding New Sub-plots ------------------------------------------------------
### Documented in method defintions

setGeneric("iheatmap", function(data, ...) standardGeneric("iheatmap"))

setGeneric("add_iheatmap", 
           function(p, data, ...) standardGeneric("add_iheatmap"))


setGeneric("main_heatmap", function(data, ...) standardGeneric("main_heatmap"))

setGeneric("add_main_heatmap", 
           function(p, data, ...) standardGeneric("add_main_heatmap"))

setGeneric("add_row_signal", 
           function(p, signal, ...) standardGeneric("add_row_signal"))

setGeneric("add_col_signal", 
           function(p, signal, ...) standardGeneric("add_col_signal"))

setGeneric("add_row_groups", 
           function(p, groups, ...) standardGeneric("add_row_groups"))

setGeneric("add_col_groups", 
           function(p, groups, ...) standardGeneric("add_col_groups"))

setGeneric("add_row_clusters", 
           function(p, clusters, ...) standardGeneric("add_row_clusters"))

setGeneric("add_col_clusters", 
           function(p, clusters, ...) standardGeneric("add_col_clusters"))

setGeneric("add_row_clustering", 
           function(p, ...) standardGeneric("add_row_clustering"))

setGeneric("add_col_clustering", 
           function(p, ...) standardGeneric("add_col_clustering"))

setGeneric("add_row_annotation", 
           function(p, ...) standardGeneric("add_row_annotation"))

setGeneric("add_col_annotation", 
           function(p, ...) standardGeneric("add_col_annotation"))

setGeneric("add_row_dendro", 
           function(p, dendro, ...) standardGeneric("add_row_dendro"))

setGeneric("add_col_dendro", 
           function(p, dendro, ...) standardGeneric("add_col_dendro"))

setGeneric("add_row_plot", 
           function(p, ...) standardGeneric("add_row_plot"))

setGeneric("add_col_plot", 
           function(p, ...) standardGeneric("add_col_plot"))

setGeneric("add_row_barplot", 
           function(p, ...) standardGeneric("add_row_barplot"))

setGeneric("add_col_barplot", 
           function(p, ...) standardGeneric("add_col_barplot"))

setGeneric("add_row_summary", 
           function(p, ...) standardGeneric("add_row_summary"))

setGeneric("add_col_summary", 
           function(p, ...) standardGeneric("add_col_summary"))

setGeneric("add_col_title", function(p, ...) standardGeneric("add_col_title"))

setGeneric("add_row_title", function(p, ...) standardGeneric("add_row_title"))

setGeneric("add_col_labels", function(p, ...) standardGeneric("add_col_labels"))

setGeneric("add_row_labels", function(p, ...) standardGeneric("add_row_labels"))

setGeneric("add_row_labels", function(p, ...) standardGeneric("add_row_labels"))

setGeneric("add_subplot_horizontal", 
           function(p, ...) standardGeneric("add_subplot_horizontal"))

setGeneric("add_subplot_vertical", 
           function(p, ...) standardGeneric("add_subplot_vertical"))

setGeneric("add_subplot", function(p, ...) standardGeneric("add_subplot"))

setGeneric("reorder_rows", 
           function(p, row_order, ...) standardGeneric("reorder_rows"))

setGeneric("reorder_cols", 
           function(p, col_order,...) standardGeneric("reorder_cols"))


## Methods for converting to plotly object ------------------------------------

#' Convert iheatmapr subcomponents to plotly format
#' 
#' These are generic methods for converting \code{link{Iheatmap-class}}  plot 
#' components to plotly lists. Not intended for end users; exported for
#' developers seeking to create new Iheatmap subplots. Any new 
#' \code{link{IheatmapPlot}}, \code{link{IheatmapShape}},
#'  \code{link{IheatmapAnnotation}}, or \code{link{IheatmapColorbar}} child class
#'  should have one of these methods. 
#' @name make_component
#' @rdname make_component
#' @param x \code{\link{IheatmapPlot-class}}, \code{\link{IheatmapShape-class}},
#' or \code{\link{IheatmapAnnotation-class}} object
#' @param ... additional arguments specific to component
#' @docType methods
#' @aliases make_trace,MainHeatmap-method
#' make_trace,RowAnnotation-method
#' make_trace,ColumnAnnotation-method
#' make_trace,RowPlot-method
#' make_trace,ColumnPlot-method
#' make_trace,GenericPlot-method
#' make_shapes,Dendrogram-method
#' make_annotations,RowTitle-method
#' make_annotations,ColumnTitle-method
#' make_annotations,RowLabels-method
#' make_annotations,ColumnLabels-method
#' make_colorbar,ContinuousColorbar,IheatmapColorbarGrid-method
#' make_colorbar,DiscreteColorbar,IheatmapColorbarGrid-method
#' @keywords internal
NULL

#' @rdname make_component
#' @export
setGeneric("make_trace", function(x, ...) standardGeneric("make_trace"))

#' @rdname make_component
#' @export
setGeneric("make_shapes", function(x, ...) standardGeneric("make_shapes"))

#' @rdname make_component
#' @export
setGeneric("make_annotations", 
           function(x, ...) standardGeneric("make_annotations"))

#' @rdname make_component
#' @export
setGeneric("make_colorbar", function(cb, grid) standardGeneric("make_colorbar"))

setGeneric("get_layout", function(x, ...) standardGeneric("get_layout"))

setGeneric("modify_layout", function(x, ...) standardGeneric("modify_layout"))

#' @export
setGeneric("to_widget", function(p, ...) standardGeneric("to_widget"))

setGeneric("save_iheatmap", 
           function(p, filename, ...) standardGeneric("save_iheatmap"))


## Axis utility methods -------------------------------------------------------

setGeneric("domain_start", function(x) standardGeneric("domain_start"))

setGeneric("domain_end", function(x) standardGeneric("domain_end"))

setGeneric("id", function(x) standardGeneric("id"))

setGeneric("domain_start<-", 
           function(x, value) standardGeneric("domain_start<-"))

setGeneric("domain_end<-", function(x, value) standardGeneric("domain_end<-"))

setGeneric("yaxis_name", function(x, ...) standardGeneric("yaxis_name"))

setGeneric("xaxis_name", function(x, ...) standardGeneric("xaxis_name"))

setGeneric("axis_text", function(x, ...) standardGeneric("axis_text"))

setGeneric("axis_values", function(x, ...) standardGeneric("axis_values"))

setGeneric("axis_order", function(x, ...) standardGeneric("axis_order"))

setGeneric("axis_order<-", function(x, value) standardGeneric("axis_order<-"))

setGeneric("yaxes", function(p, ...) standardGeneric("yaxes"))

setGeneric("xaxes", function(p, ...) standardGeneric("xaxes"))

setGeneric("yaxes<-", function(p, value) standardGeneric("yaxes<-"))

setGeneric("xaxes<-", function(p, value) standardGeneric("xaxes<-"))

setGeneric("buffers", function(x) standardGeneric("buffers"))

setGeneric("current_xaxis", function(x) standardGeneric("current_xaxis"))

setGeneric("current_xaxis<-", 
           function(x, value) standardGeneric("current_xaxis<-"))

setGeneric("current_yaxis", function(x) standardGeneric("current_yaxis"))

setGeneric("current_yaxis<-", 
           function(x, value) standardGeneric("current_yaxis<-"))


## Plot utility methods -------------------------------------------------------

setGeneric("plots", function(x) standardGeneric("plots"))

setGeneric("plots<-", function(x, value) standardGeneric("plots<-"))

setGeneric("get_data", function(x, ...) standardGeneric("get_data"))

setGeneric("get_title", function(x, ...) standardGeneric("get_title"))

setGeneric("colorbar", function(x, ...) standardGeneric("colorbar"))

setGeneric("get_heatmap", function(p, ...) standardGeneric("get_heatmap"))

setGeneric("get_col_groups", function(p, ...) standardGeneric("get_col_groups"))

setGeneric("get_row_groups", function(p, ...) standardGeneric("get_row_groups"))

## Shapes utility methods ------------------------------------------------------

setGeneric("shapes", function(x) standardGeneric("shapes"))

setGeneric("shapes<-", function(x, value) standardGeneric("shapes<-"))

## Annotations utility methods -------------------------------------------------

setGeneric("annotations", function(x) standardGeneric("annotations"))

setGeneric("annotations<-", function(x, value) standardGeneric("annotations<-"))


## Colorbar Methods ----------------------------------------------------------

setGeneric("colorscale", function(colorbar, ...) standardGeneric("colorscale"))

setGeneric("colorbars", function(x, ...) standardGeneric("colorbars"))

setGeneric("colorbars<-", function(x, value) standardGeneric("colorbars<-"))

setGeneric("zmin", function(x) standardGeneric("zmin"))

setGeneric("zmax", function(x) standardGeneric("zmax"))

setGeneric("color_palette", function(x, ...) standardGeneric("color_palette"))

setGeneric("get_colorbar_position",
           function(x, ...) standardGeneric("get_colorbar_position"))

setGeneric("get_legend_position",
           function(x, ...) standardGeneric("get_legend_position"))



