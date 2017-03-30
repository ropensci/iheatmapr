## IheatmapPanel Class --------------------------------------------------------
## VIRTUAL class that will be basis of IheatmapPlot, IheatmapShape, and
## IheatmapAnnotation

setClass("IheatmapPanel",
         slots = list(xaxis = "character",
                      yaxis = "character",
                      data = "ANY"),
         contains = "VIRTUAL")

## IheatmapPanels Class -------------------------------------------------------
## List of IheatmapPanel
##
## VIRTUAL class that will be basis of IheatmapPlots, IheatmapShapes, and
## IheatmapAnnotations

setClass("IheatmapPanels",
         contains = "VIRTUAL")

## IheatmapPlot Class and children ---------------------------------------------

#' IheatmapPlot
#' 
#' Virtual class for storing plot objects
#' @slot xaxis name of xaxis
#' @slot yaxis name of yaxis
#' @slot data main plot data
#' @section SubClasses:
#' \itemize{
#' \item \code{\link{MainHeatmap-class}}
#' \item \code{\link{RowAnnotation-class}}
#' \item \code{\link{ColumnAnnotation-class}}
#' \item \code{\link{RowPlot-class}}
#' \item \code{\link{ColumnPlot-class}}
#' \item \code{\link{GenericPlot-class}}
#' }
#' @aliases IheatmapPlot
#' @export
#' @keywords internal
#' @author Alicia Schep
setClass("IheatmapPlot",
         contains = "IheatmapPanel")

#' MainHeatmap
#' 
#' Class for storing main heatmap
#' @slot xaxis name of xaxis
#' @slot yaxis name of yaxis
#' @slot data matrix of heatmap values
#' @slot colorbar name of colorbar
#' @slot show_colorbar show the colorbar?
#' @aliases MainHeatmap
#' @seealso \code{\link{main_heatmap}}
#' @keywords internal
#' @author Alicia Schep
setClass("MainHeatmap",
         slots = list(data = "matrix",
                      colorbar = "character",
                      show_colorbar = "logical"),
         contains = "IheatmapPlot")

#' RowAnnotation
#' 
#' Class for storing row annotation
#' @slot xaxis name of xaxis
#' @slot yaxis name of yaxis
#' @slot data vector of annotation values
#' @slot colorbar name of colorbar
#' @slot show_colorbar show the colorbar?
#' @aliases RowAnnotation
#' @seealso \code{\link{add_row_annotation}}, \code{\link{add_row_signal}},
#' \code{\link{add_row_groups}}
#' @keywords internal
#' @author Alicia Schep
setClass("RowAnnotation",
         slots = list(data = "vector",
                      title = "character",
                      colorbar = "character",
                      show_colorbar = "logical"),
         contains = "IheatmapPlot")

#' ColumnAnnotation
#' 
#' Class for storing row annotation
#' @slot xaxis name of xaxis
#' @slot yaxis name of yaxis
#' @slot data vector of annotation values
#' @slot colorbar name of colorbar
#' @slot show_colorbar show the colorbar?
#' @aliases ColumnAnnotation
#' @seealso \code{\link{add_col_annotation}}, \code{\link{add_col_signal}},
#' \code{\link{add_col_groups}}
#' @keywords internal
#' @author Alicia Schep
setClass("ColumnAnnotation",
         slots = list(data = "vector",
                      title = "character",
                      colorbar = "character",
                      show_colorbar = "logical"),
         contains = "IheatmapPlot")

#' ColumnPlot
#' 
#' Class for storing generic column plot
#' @slot xaxis name of xaxis
#' @slot yaxis name of yaxis
#' @slot data vector of values
#' @slot name tracename
#' @slot type trace type
#' @slot showlegend show the legend?
#' @slot additional additional plotly parameters
#' @aliases ColumnPlot
#' @seealso \code{\link{add_col_plot}}, \code{\link{add_col_barplot}},
#' \code{\link{add_col_summary}}
#' @keywords internal
#' @author Alicia Schep
setClass("ColumnPlot",
         slots = list(data = "vector",
                      name = "character",
                      showlegend = "logical",
                      type = "character",
                      additional = "list"),
         contains = "IheatmapPlot")

#' RowPlot
#' 
#' Class for storing generic row plot
#' @slot xaxis name of xaxis
#' @slot yaxis name of yaxis
#' @slot data vector of values
#' @slot name tracename
#' @slot type trace type
#' @slot showlegend show the legend?
#' @slot additional additional plotly parameters
#' @aliases RowPlot
#' @seealso \code{\link{add_row_plot}}, \code{\link{add_row_barplot}},
#' \code{\link{add_row_summary}}
#' @keywords internal
#' @author Alicia Schep
setClass("RowPlot",
         slots = list(data = "vector",
                      name = "character",
                      showlegend = "logical",
                      type = "character",
                      additional = "list"),
         contains = "IheatmapPlot")

#' GenericPlot
#' 
#' Class for storing an arbitrary subplot
#' @slot xaxis name of xaxis
#' @slot yaxis name of yaxis
#' @slot data list of plotly parameters
#' @aliases GenericPlot
#' @seealso \code{\link{add_subplot}}
#' @keywords internal
#' @author Alicia Schep
setClass("GenericPlot",
         slots = list(data = "list"),
         contains = "IheatmapPlot")

#' IheatmapPlots
#' 
#' Class for storing \code{\link{IheatmapPlot}} objects
#' @aliases IheatmapPlots
#' @seealso \code{\link{IheatmapPlot-class}}, \code{\link{Iheatmap-class}}
#' @keywords internal
#' @author Alicia Schep
setClass("IheatmapPlots",
         prototype = list(elementType = "IheatmapPlot"),
         contains = c("SimpleList","IheatmapPanels"))

## IheatmapShape Class and descendants-----------------------------------------

#' IheatmapShape
#' 
#' Virtual class for storing shape objects
#' @slot xaxis name of xaxis
#' @slot yaxis name of yaxis
#' @slot data main shape data
#' @section SubClasses:
#' \itemize{
#' \item \code{\link{Dendrogram-class}}
#' }
#' @aliases IheatmapShape
#' @export
#' @keywords internal
#' @author Alicia Schep
setClass("IheatmapShape",
         contains = "IheatmapPanel")

#' IheatmapShapes
#' 
#' Class for storing \code{\link{IheatmapShapes}} objects
#' @aliases IheatmapShapes
#' @seealso \code{\link{IheatmapShape-class}}, \code{\link{Iheatmap-class}}
#' @keywords internal
#' @author Alicia Schep
setClass("IheatmapShapes",
         prototype = list(elementType = "IheatmapShape"),
         contains = c("SimpleList","IheatmapPanels"))

setOldClass("hclust")

#' Dendrogram
#' 
#' Class for storing dendrogram
#' @slot xaxis name of xaxis
#' @slot yaxis name of yaxis
#' @slot data hclust object
#' @slot side side of plot on which dendrogram is positioned, controls 
#' orientation
#' @aliases Dendrogram
#' @seealso \code{\link{add_row_dendro}}, \code{\link{add_col_dendro}},
#' \code{\link{add_row_clustering}}, \code{\link{add_col_clustering}}
#' @keywords internal
#' @author Alicia Schep
setClass("Dendrogram",
         slots = list(data = "hclust",
                      side = "character"),
         contains = "IheatmapShape")



## IheatmapAnnotation Class and descendants------------------------------------

#' IheatmapAnnotation
#' 
#' Virtual class for storing annotation objects
#' @slot xaxis name of xaxis
#' @slot yaxis name of yaxis
#' @slot data main annotation data
#' @section SubClasses:
#' \itemize{
#' \item \code{\link{RowTitle-class}}
#' \item \code{\link{ColumnTitle-class}}
#' \item \code{\link{RowLabels-class}}
#' \item \code{\link{ColumnLabels-class}}
#' }
#' @aliases IheatmapAnnotation
#' @export
#' @keywords internal
#' @author Alicia Schep
setClass("IheatmapAnnotation",
         contains = "IheatmapPanel")

#' IheatmapAnnotations
#' 
#' Class for storing \code{\link{IheatmapAnnotation}} objects
#' @aliases IheatmapAnnotations
#' @seealso \code{\link{IheatmapAnnotation-class}}, \code{\link{Iheatmap-class}}
#' @keywords internal
setClass("IheatmapAnnotations",
         prototype = list(elementType = "IheatmapAnnotation") ,
         contains = c("SimpleList","IheatmapPanels"))

#' RowTitle
#' 
#' Class for storing row title
#' @slot xaxis name of xaxis
#' @slot yaxis name of yaxis
#' @slot data the title (character)
#' @slot side side of plot on which dendrogram is positioned, controls 
#' orientation
#' @slot textangle angle for text
#' @slot font list of font attributes
#' @aliases RowTitle
#' @seealso \code{\link{add_row_title}}
#' @keywords internal
#' @author Alicia Schep
setClass("RowTitle",
         slots = list(data = "character",
                      side = "character",
                      textangle = "numeric",
                      font = "list"),
         contains = "IheatmapAnnotation")

#' ColumnTitle
#' 
#' Class for storing column title
#' @slot xaxis name of xaxis
#' @slot yaxis name of yaxis
#' @slot data the title (character)
#' @slot side side of plot on which dendrogram is positioned, controls 
#' orientation
#' @slot textangle angle for text
#' @slot font list of font attributes
#' @aliases ColumnTitle
#' @seealso \code{\link{add_col_title}}
#' @keywords internal
#' @author Alicia Schep
setClass("ColumnTitle",
         slots = list(data = "character",
                      side = "character",
                      textangle = "numeric",
                      font = "list"),
         contains = "IheatmapAnnotation")

#' RowLabels
#' 
#' Class for storing row labels
#' @slot xaxis name of xaxis
#' @slot yaxis name of yaxis
#' @slot data the names of the tick labels
#' @slot positions the positions of the tick labels
#' @slot side side of plot on which dendrogram is positioned, controls 
#' orientation
#' @slot textangle angle for text
#' @slot font list of font attributes
#' @aliases RowLabels
#' @seealso \code{\link{add_row_labels}}
#' @keywords internal
#' @author Alicia Schep
setClass("RowLabels",
         slots = list(data = "character",
                      positions = "numeric",
                      side = "character",
                      textangle = "numeric",
                      font = "list"),
         contains = "IheatmapAnnotation")

#' ColumnLabels
#' 
#' Class for storing column labels
#' @slot xaxis name of xaxis
#' @slot yaxis name of yaxis
#' @slot data the names of the tick labels
#' @slot positions the positions of the tick labels
#' @slot side side of plot on which dendrogram is positioned, controls 
#' orientation
#' @slot textangle angle for text
#' @slot font list of font attributes
#' @aliases ColumnLabels
#' @seealso \code{\link{add_col_labels}}
#' @keywords internal
#' @author Alicia Schep
setClass("ColumnLabels",
         slots = list(data = "character",
                      positions = "numeric",
                      side = "character",
                      textangle = "numeric",
                      font = "list"),
         contains = "IheatmapAnnotation")


## ColorBar Classes -----------------------------------------------------------

#' IheatmapColorbar
#' 
#' Virtual class for storing colorbar objects
#' @slot title title for colorbar
#' @slot position integer indicating relative position of colorbar
#' @slot colors name of color palette or vector of colors 
#' @section SubClasses:
#' \itemize{
#' \item \code{\link{ContinuousColorbar-class}}
#' \item \code{\link{DiscreteColorbar-class}}
#' }
#' @aliases IheatmapColorbar
#' @export
#' @keywords internal
#' @author Alicia Schep
setClass("IheatmapColorbar",
         slots = list(title = "character",
                      position = "integer",
                      colors = "character"),
         contains = "VIRTUAL")

#' ContinuousColorbar
#' 
#' Class for storing continuous colorbar information
#' @slot title title for colorbar
#' @slot position integer indicating relative position of colorbar
#' @slot colors name of color palette or vector of colors 
#' @slot zmid midpoint of colorbar
#' @slot zmin min of colorbar
#' @slot zmax max of colorbar
#' @aliases ContinuousColorbar
#' @keywords internal
#' @author Alicia Schep
setClass("ContinuousColorbar",
         slots = list(zmid = "numeric",
                      zmax = "numeric",
                      zmin = "numeric"),
         contains = "IheatmapColorbar")

#' DiscreteColorbar
#' 
#' Class for storing discrete colorbar information
#' @slot title title for colorbar
#' @slot position integer indicating relative position of colorbar
#' @slot colors name of color palette or vector of colors 
#' @slot ticktext labels for categories
#' @slot tickvals integer values for categories
#' @aliases DiscreteColorbar
#' @keywords internal
#' @author Alicia Schep
setClass("DiscreteColorbar",
         slots = list(ticktext = "character",
                      tickvals = "integer"),
         contains = "IheatmapColorbar")

#' IheatmapColorbars
#' 
#' Class for storing \code{\link{IheatmapColorbar}} objects
#' @aliases IheatmapColorbars
#' @seealso \code{\link{IheatmapColorbar-class}}, \code{\link{Iheatmap-class}}
#' @keywords internal
#' @author Alicia Schep
setClass("IheatmapColorbars",
         prototype = list(elementType = "IheatmapColorbar"),
         contains = "SimpleList")

#' IheatmapColorbarGrid
#' 
#' Parameters for setting up colorbars
#' @slot nrows number of rows
#' @slot x_spacing spacing between colorbars horizontally
#' @slot y_spacing spacing between colorbars vertically
#' @slot y_length length of colorbars vertically
#' @slot x_start start position horizontally
#' @slot y_start start position vertically
#' @aliases IheatmapColorbarGrid
#' @seealso \code{\link{setup_colorbar_grid}}, \code{\link{Iheatmap-class}}
#' @keywords internal
#' @author Alicia Schep
setClass("IheatmapColorbarGrid",
         slots = list(nrows = "numeric",
                      x_spacing = "numeric",
                      y_spacing = "numeric",
                      y_length = "numeric",
                      x_start = "numeric",
                      y_start = "numeric"),
         prototype = list(nrows = 3,
                          y_length = 0.27,
                          x_spacing = 0.16,
                          y_spacing = 0.3,
                          x_start = 1.05,
                          y_start = 0.9))


## Axis classes ---------------------------------------------------------------

#' IheatmapAxis
#' 
#' Class for storing axis information
#' @slot id plotly id for axis
#' @slot domain_start start of domain (0 to 1)
#' @slot domain_end end of domain (0 to 1)
#' @slot anchor anchor for axis
#' @slot layout plotly layout parameters
#' @section SubClasses:
#' \itemize{
#' \item \code{\link{IheatmapX-class}}
#' \item \code{\link{IheatmapY-class}}
#' \item \code{\link{IheatmapMainX-class}}
#' \item \code{\link{IheatmapMainY-class}} 
#' }
#' @aliases IheatmapAxis
#' @export
#' @seealso \code{\link{Iheatmap-class}}
#' @keywords internal
#' @author Alicia Schep
setClass("IheatmapAxis",
         slots = list(id = "character",
                      domain_start = "numeric",
                      domain_end = "numeric",
                      anchor = "character",
                      layout = "list"))

setValidity("IheatmapAxis",
            function(object){
              if (domain_start(object) < -0.01)
                return("domain_start for axis can't be negative")
              if (domain_end(object) > 1.01)
                return("domain_end for axis can't be greater than one")
              if (domain_end(object) < domain_start(object))
                return("domain_end must be greater than domain_start")
              return(TRUE)
            })

#' IheatmapX
#' 
#' Class for storing X axis information
#' @slot id plotly id for axis
#' @slot domain_start start of domain (0 to 1)
#' @slot domain_end end of domain (0 to 1)
#' @slot anchor anchor for axis
#' @slot layout plotly layout parameters
#' @aliases IheatmapX
#' @seealso \code{\link{Iheatmap-class}}, \code{\link{IheatmapAxis}}
#' @keywords internal
#' @author Alicia Schep
setClass("IheatmapX",
         contains = "IheatmapAxis")

#' IheatmapY
#' 
#' Class for storing Y axis information
#' @slot id plotly id for axis
#' @slot domain_start start of domain (0 to 1)
#' @slot domain_end end of domain (0 to 1)
#' @slot anchor anchor for axis
#' @slot layout plotly layout parameters
#' @aliases IheatmapY
#' @seealso \code{\link{Iheatmap-class}}, \code{\link{IheatmapAxis}}
#' @keywords internal
#' @author Alicia Schep
setClass("IheatmapY",
         contains = "IheatmapAxis")

setClass("IheatmapMainAxis",
         slots = list(categorical = "logical",
                      order = "integer",
                      text = "vector"),
         contains = "IheatmapAxis")

#' IheatmapMainX
#' 
#' Class for storing X axis information for "main" x axis-- x axis for 
#' \code{\link{MainHeatmap-class}}
#' @slot id plotly id for axis
#' @slot domain_start start of domain (0 to 1)
#' @slot domain_end end of domain (0 to 1)
#' @slot anchor anchor for axis
#' @slot layout plotly layout parameters
#' @slot categorical is axis categorical?
#' @slot order ordering of columns
#' @slot text text labels for columns
#' @aliases IheatmapMainX
#' @seealso \code{\link{Iheatmap-class}}, \code{\link{IheatmapAxis}}
#' @keywords internal
#' @author Alicia Schep
setClass("IheatmapMainX",
         contains = c("IheatmapX","IheatmapMainAxis"))

#' IheatmapMainY
#' 
#' Class for storing Y axis information for "main" y axis-- y axis for 
#' \code{\link{MainHeatmap-class}}
#' @slot id plotly id for axis
#' @slot domain_start start of domain (0 to 1)
#' @slot domain_end end of domain (0 to 1)
#' @slot anchor anchor for axis
#' @slot layout plotly layout parameters
#' @slot categorical is axis categorical?
#' @slot order ordering of rows
#' @slot text text labels for rows
#' @aliases IheatmapMainY
#' @seealso \code{\link{Iheatmap-class}}, \code{\link{IheatmapAxis}}
#' @keywords internal
#' @author Alicia Schep
setClass("IheatmapMainY",
         contains = c("IheatmapY","IheatmapMainAxis"))

#' IheatmapAxes
#' 
#' Class for storing \code{\link{IheatmapAxis}} objects
#' @slot axis x or y?
#' @aliases IheatmapAxes
#' @seealso \code{\link{IheatmapAxis-class}}, \code{\link{Iheatmap-class}}
#' @keywords internal
#' @author Alicia Schep
setClass("IheatmapAxes",
         slots = c(axis = "character"),
         prototype = c(elementType = "IheatmapAxis"),
         contains = "SimpleList")


setValidity("IheatmapAxes",
            function(object){
              if (length(object) > 0)
                return(all(vapply(object, validObject, FALSE)))
              return(TRUE)
            })

## Iheatmap class  - main class! ----------------------------------------------

#' Iheatmap-class
#'
#' Class to store complex interactive heatmap objects from iheatmapr package
#' @slot plots list of plot element in \code{\link{IheatmapPlots}} format
#' @slot shapes list of shape element in \code{\link{IheatmapShapes}} format 
#' @slot annotations list of annotation elements in 
#' \code{\link{IheatmapAnnotations}} format
#' @slot xaxes list of x axes in \code{\link{IheatmapAxes}} format
#' @slot yaxes list of y axes in \code{\link{IheatmapAxes}} format
#' @slot colorbars list of colorbars in \code{\link{IheatmapColorbars}} format
#' @slot colorbar_grid colorbar grid parameters in
#'  \code{\link{IheatmapColorbarGrid}} format 
#' @slot current_xaxis name of current x axis 
#' @slot current_yaxis name of current y axis 
#' @slot layout list of plotly layout parameters 
#' @details This is a virtual class with two children classes, 
#'  IheatmapHorizontal and IheatmapVertical. For IheatmapHorizontal additional
#'  main heatmaps are added horizontally, and for IheatmapVertical additional
#'  main heatmaps are added vertically. For details on accessing certain slots 
#'  of this class, see \code{\link{access_component}} documentation.
#' @aliases IheatmapHorizontal IheatmapVertical
#' IheatmapHorizontal-class IheatmapVertical-class
#' @seealso \code{\link{iheatmap}}, \code{\link{main_heatmap}}, 
#' \code{\link{access_component}}
#' @export
#' @author Alicia Schep
setClass("Iheatmap",
         slots = list(plots = "IheatmapPlots",
                               shapes = "IheatmapShapes",
                               annotations = "IheatmapAnnotations",
                               xaxes = "IheatmapAxes",
                               yaxes = "IheatmapAxes",
                               colorbars = "IheatmapColorbars",
                               colorbar_grid = "IheatmapColorbarGrid",
                               current_xaxis = "character",
                               current_yaxis = "character",
                               layout = "list"),
         contains = "VIRTUAL")

setValidity("Iheatmap",
            function(object){
              if (!all(xaxis_name(object) %in% names(xaxes(object))))
                return("x axis names in panels don't match names of xaxes")
              if (!all(yaxis_name(object) %in% names(yaxes(object))))
                return("y axis names in panels don't match names of yaxes")
              if (!all(colorbars(plots(object)) %in% names(colorbars(object))))
                return("colorbar names in plots don't match names of colorbars")
              validObject(xaxes(object))
              validObject(yaxes(object))
              return(TRUE)
            })

setClass("IheatmapHorizontal",
         contains = "Iheatmap")

setClass("IheatmapVertical",
         contains = "Iheatmap")
