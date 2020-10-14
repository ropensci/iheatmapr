#' iheatmapr
#' 
#' Interactive complex heatmaps in R
#' 
#' iheatmapr is a package for building complex, interactive heatmaps in R that 
#' can be explored in interactive R sessions or incorporated into rmarkdown 
#' documents, shiny applications, or standalone html files. 
#' 
#' The package includes a modular system for building up complex heatmaps, where
#' subplots get iteratively added to the top/left/right/bottom of the main 
#' heatmap(s). The \code{\link{iheatmap}} function provides a wrapper around
#' many of the common modular subcomponents to build fairly standard, moderately
#' complex heatmap.  
#' 
#' See the vignette for detailed instructions for how to use the package.
#' 
#' iheatmapr uses the plotly javascript library (\url{https://plotly.com/}) for making the 
#' interactive figures and htmlwidgets (http://www.htmlwidgets.org/) for 
#' rendering them in R. 
#' 
#' @import methods
#' @importFrom utils modifyList
#' @importFrom magrittr %>%
#' @importFrom scales zero_range
#' @importFrom fastcluster hclust
#' @name iheatmapr
#' @rdname iheatmapr
#' @seealso \code{\link{main_heatmap}}, \code{\link{iheatmap}},
#' \code{\link{Iheatmap-class}}
#' @author Alicia Schep
#' @docType package
NULL

#' measles
#' 
#' Data on measles cases for different states from 1930 to 2001
#' @name measles
#' @docType data
#' @examples 
#' data(measles)
NULL