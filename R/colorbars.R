setMethod("get_colorbar_position", c(x = "Iheatmap"),
  function(x, new = TRUE){
    if (new){
      j <- 1
      existing <- unname(get_colorbar_position(x, new = FALSE))
      while (j %in% existing){
        j <- j + 1
      }
      return(j)
    } else{
      return(vapply(colorbars(x), get_colorbar_position, 1))
    }
  }
)

setMethod("get_colorbar_position", c(x = "IheatmapColorbar"),
          function(x){ x@position})

setMethod("get_colorbar_position", c(x = "IheatmapColorbars"),
          function(x){vapply(x, get_colorbar_position, 1)})

setMethod("get_legend_position", c(x = "Iheatmap"),
          function(x){
            max_pos <- max(get_colorbar_position(x, new = FALSE))
            x@colorbar_grid@x_start + 
              ((max_pos %/% x@colorbar_grid@nrows) + 1) *
              x@colorbar_grid@x_spacing
          })


setMethod("zmin", c(x = "ContinuousColorbar"),
          function(x){ x@zmin})

setMethod("zmax", c(x = "ContinuousColorbar"),
          function(x){ x@zmax})

setMethod("zmin", c(x = "DiscreteColorbar"),
          function(x){min(x@tickvals)})

setMethod("zmax", c(x = "DiscreteColorbar"),
          function(x){max(x@tickvals)})

#' setup_colorbar_grid
#'
#' function to set parameters controlling colorbar placement in iHeatmap object
#' @param nrows number of rows in colorbar grid
#' @param y_length length of colorbar
#' @param x_spacing spacing along horizonatal axis between colorbars
#' @param y_spacing spacing along vertical axis between colorbars
#' @param x_start left most position of colorbar grid
#' @param y_start top most position of colorbar grid
#' @return \code{\link{IheatmapColorbarGrid-class}} object
#' @export
setup_colorbar_grid <- function(nrows = 3,
                                y_length = y_spacing * 0.9,
                                x_spacing = 0.16,
                                y_spacing = y_start / nrows,
                                x_start = 1.05,
                                y_start = 0.9){

  out <- new("IheatmapColorbarGrid",
             nrows = nrows,
             x_spacing = x_spacing,
             y_spacing = y_spacing,
             y_length = y_length,
             x_start = x_start,
             y_start = y_start)

  return(out)
}

#' @importFrom scales zero_range
#' @importFrom plyr round_any
tickvals_helper <- function(zmin, zmid, zmax) {
  
  rng <- c(zmin, zmax)
  
  span <- if (zero_range(rng)) abs(rng[1]) else diff(rng)
  if (span == 0){
    precision <- 1
  } else{
    precision <- 10 ^ floor(log10(span)-1)
  }
  
  if (zmid > zmin && zmid < zmax){
    out <- c(round_any(zmin,precision,ceiling), 
             round_any(zmid,precision), 
             round_any(zmax,precision,floor))
  } else{
    out <- c(round_any(zmin,precision,ceiling), 
             round_any(zmax,precision,floor))
  }

  out
}

setMethod("make_colorbar",
          signature = c(cb = "ContinuousColorbar", 
                        grid = "IheatmapColorbarGrid"),
          function(cb, grid){
            cbx <- grid@x_start + ((cb@position - 1) %/% grid@nrows) * 
              grid@x_spacing
            cby <- grid@y_start - ((cb@position - 1) %% grid@nrows) * 
              grid@y_spacing
            out <- list(x = cbx,
                        y = cby,
                        len = grid@y_length,
                        title = cb@title,
                        ypad = 5,
                        thickness = 20,
                        tickvals = tickvals_helper(cb@zmin, cb@zmid, cb@zmax))
          })

setMethod("make_colorbar",
          signature = c(cb = "DiscreteColorbar", 
                        grid = "IheatmapColorbarGrid"),
          function(cb, grid){
            cbx <- grid@x_start + ((cb@position - 1) %/% grid@nrows) * 
              grid@x_spacing
            cby <- grid@y_start - ((cb@position - 1) %% grid@nrows) * 
              grid@y_spacing
            n <- length(cb@ticktext)
            w <- (n - 1) / n 
            out <- list(x = cbx,
                        y = cby,
                        len = grid@y_length,
                        title = cb@title,
                        ypad = 5,
                        thickness = 20,
                        ticktext = cb@ticktext,
                        tickvals = seq(1 + w * 0.5, 
                                       n - w * 0.5,
                                       length.out = n))
          })


setReplaceMethod("colorbars", signature = c(x = "Iheatmap"),
          function(x, value){
            x@colorbars <- value
            x
          })

setMethod("colorbar", signature = c(x = "IheatmapPlot"),
          function(x){
            if ("colorbar" %in% slotNames(x)){
              return(x@colorbar)
            } else{
              return(NULL)
            }
          })


setMethod("colorbars", signature = c(x = "IheatmapPlots"),
          function(x){
            do.call(c,lapply(x, colorbar))
          })

#' @rdname access_component
#' @export
setMethod("colorbars", signature = c(x = "Iheatmap"),
          function(x, what = c("all","continuous","discrete")){
            what <- match.arg(what)
            colorbars(x@colorbars, what)
          })

setMethod("colorbars", c( x= "IheatmapColorbars"),
          function(x, what = c("all","continuous","discrete")){
            what <- match.arg(what)
            if (what == "continuous"){
              ix <- vapply(x, inherits, FALSE, "ContinuousColorbar")
              return(x[ix])
            } else if (what == "discrete"){
              ix <- vapply(x, inherits, FALSE, "DiscreteColorbar")
              return(x[ix])
            } else{
              return(x)
            }
          })

setMethod("color_palette", c(x = "Iheatmap"),
          function(x, what = c("all","continuous","discrete")){
            what <- match.arg(what)
            color_palette(colorbars(x@colorbars), what)
          })

setMethod("color_palette", c(x = "IheatmapColorbars"),
          function(x, what = c("all","continuous","discrete")){
            what <- match.arg(what)
            lapply(colorbars(x,what), color_palette)
          })

setMethod("color_palette",c(x = "IheatmapColorbar"),
          function(x){
            x@colors
          })

discrete_colorbar <- function(name, position, colors, ticktext, tickvals){
  new("DiscreteColorbar",
      title = name,
      position = as.integer(position),
      colors = colors,
      ticktext = ticktext,
      tickvals = tickvals)
}

continuous_colorbar <- function(name, position, colors, zmid, zmin, zmax){
  new("ContinuousColorbar",
      title = name,
      position = as.integer(position),
      colors = colors,
      zmid = zmid,
      zmin = zmin,
      zmax = zmax)
}

setMethod(add_colorbar, c(p = "Iheatmap", new_colorbar = "ContinuousColorbar"),
          function(p, new_colorbar){
            if (new_colorbar@title %in% names(colorbars(p, what = "continuous"))){
              colorbars(p)[[new_colorbar@title]]@zmin <- 
                min(colorbars(p)[[new_colorbar@title]]@zmin,new_colorbar@zmin)
              colorbars(p)[[new_colorbar@title]]@zmax <- 
                max(colorbars(p)[[new_colorbar@title]]@zmax, new_colorbar@zmax)
            } else{
              colorbars(p)[[new_colorbar@title]] <- new_colorbar
            }
            return(p)})

setMethod(add_colorbar, c(p = "Iheatmap", new_colorbar = "DiscreteColorbar"),
          function(p, new_colorbar){
            if (new_colorbar@title %in% names(colorbars(p, what = "discrete"))){
              if (length(intersect(colorbars(p)[[new_colorbar@title]]@ticktext, 
                                   new_colorbar@ticktext)) == 0){
                stop(paste("No elements in common between groups with name:",
                           new_colorbar@title))
              } else if (length(setdiff(colorbars(p)[[new_colorbar@title]]@ticktext, 
                                        new_colorbar@ticktext))>0){
                warning(paste("Adding elements to group:", new_colorbar@title))
              }
              colorbars(p)[[new_colorbar@title]]@ticktext <- 
                union(colorbars(p)[[new_colorbar@title]]@ticktext, new_colorbar@ticktext)
            } else{
              colorbars(p)[[new_colorbar@title]] <- new_colorbar
            }
            return(p)})

