#' reorder_rows
#' 
#' Reorder the rows of an \code{\link{Iheatmap-class}} object
#' @param p \code{\link{Iheatmap-class}} object
#' @param row_order integer vector
#' @param yname name of yaxis to reorder, only applicable if object is oriented
#' vertically
#' @seealso \code{\link{add_row_clustering}}, \code{\link{reorder_cols}} 
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @author Alicia Schep
#' @rdname reorder_rows
#' @name reorder_rows
#' @aliases reorder_rows,IheatmapHorizontal,integer-method
#' reorder_rows,IheatmapVertical,integer-method
#' @examples 
#' 
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)  
#' dend <- hclust(dist(mat))
#' hm <- iheatmap(mat) %>% reorder_rows(dend$order)
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod(reorder_rows, c(p = "IheatmapHorizontal", row_order = "integer"),
          function(p, row_order){
            stopifnot(yaxes(p)[["y"]]@categorical)
            axis_order(yaxes(p)[["y"]]) <- row_order
            validObject(p)
            p
          })

#' @rdname reorder_rows
setMethod(reorder_rows, c(p = "IheatmapVertical", row_order = "integer"),
          function(p, row_order, yname = current_yaxis(p)){
            stopifnot(yaxes(p)[[yname]]@categorical)
            axis_order(yaxes(p)[[yname]]) <- row_order
            p
          })

#' reorder_cols
#' 
#' Reorder the columns of an \code{\link{Iheatmap-class}} object
#' @param p \code{\link{Iheatmap-class}} object
#' @param col_order integer vector
#' @param xname name of xaxis to reorder, only applicable if object is oriented
#' horizontally
#' @seealso \code{\link{add_row_clustering}}, \code{\link{reorder_cols}} 
#' @return \code{\link{Iheatmap-class}} object, which can be printed to generate 
#' an interactive graphic
#' @export
#' @author Alicia Schep
#' @rdname reorder_cols
#' @name reorder_cols
#' @aliases reorder_cols,IheatmapHorizontal,integer-method
#' reorder_cols,IheatmapVertical,integer-method
#' @examples 
#' 
#' mat <- matrix(rnorm(20), ncol = 5, nrow = 4)  
#' dend <- hclust(dist(t(mat)))
#' hm <- iheatmap(mat) %>% reorder_cols(dend$order)
#' 
#' # Print heatmap if interactive session 
#' if (interactive()) hm 
setMethod(reorder_cols, c(p = "IheatmapHorizontal", col_order = "integer"),
          function(p, col_order, xname = current_xaxis(p)){
            stopifnot(xaxes(p)[[xname]]@categorical)
            axis_order(xaxes(p)[[xname]]) <- col_order
            p
          })

#' @rdname reorder_cols
#' @export
setMethod(reorder_cols, c(p = "IheatmapVertical", col_order = "integer"),
          function(p, col_order){
            stopifnot(xaxes(p)[["x"]]@categorical)
            axis_order(xaxes(p)[["x"]]) <- col_order
            p
          })

new_xaxis <- function(p, yname, layout = list()){
  if (yname %ni% names(yaxes(p)))
    stop("Invalid yname argument")
  
  new("IheatmapX",
      id = paste0("x", length(xaxes(p)) + 1),
      domain_start = 0,
      domain_end = 1,
      anchor = id(yaxes(p)[[yname]]),
      layout = layout)
}

new_yaxis <- function(p, xname, layout = list()){
  if (xname %ni% names(xaxes(p)))
    stop("Invalid xname argument")
  
  new("IheatmapY",
      id = paste0("y", length(yaxes(p)) + 1),
      domain_start = 0,
      domain_end = 1,
      anchor = id(xaxes(p)[[xname]]),
      layout = layout)
}

zero_bounded <- function(x){
  if (abs(x) < 10**-10){
    out <- 0
  } else if (x >= 0){
    out <- x
  } else{
    stop("Value should be positive")
  }
  return(out)
}

one_bounded <- function(x){
  if (abs(x - 1) < 10**-10){
    out <- 1
  } else if (x <= 1){
    out <- x
  } else{
    stop("Value should be less than 1")
  }
  return(out)
}

no_axis <- list(title = "",
               zeroline = FALSE,
               showline = FALSE,
               showticklabels = FALSE,
               showgrid = FALSE,
               ticks = "")

setMethod(current_xaxis, c(x = "Iheatmap"),
          function(x){x@current_xaxis})


setReplaceMethod("current_xaxis", c(x = "IheatmapHorizontal"),
                 function(x, value){x@current_xaxis <- value; x})


setMethod(current_yaxis, c(x = "Iheatmap"),
          function(x){x@current_yaxis})

setReplaceMethod("current_yaxis", c(x = "IheatmapVertical"),
                 function(x, value){x@current_yaxis <- value; x})

setMethod(axis_text, c(x = "IheatmapMainAxis"),
          function(x, ordered = FALSE){
            if (x@categorical){
              if (ordered){
                return(as.character(x@text[x@order]))
              } else{
                return(as.character(x@text))
              }
            } else{
              return(as.character(x@text))
            }
          })

setMethod(axis_values, c(x = "IheatmapMainAxis"),
          function(x, ordered = FALSE){
            if (x@categorical){
              vals <- seq_along(x@text) 
              if (ordered){
                return(vals[x@order])
              } else{
                return(vals)
              }
            } else{
              return(as.numeric(x@text))
            }
          })

setMethod(axis_order, c(x = "IheatmapMainAxis"),
          function(x){x@order})

setMethod(domain_start, c(x = "IheatmapAxis"),
          function(x){x@domain_start})

setMethod(domain_start, c(x = "IheatmapAxes"),
          function(x){vapply(x, domain_start, 1)})


setReplaceMethod("domain_start", c(x = "IheatmapAxis"),
                 function(x, value){x@domain_start <- value; x})


setReplaceMethod("domain_start", c(x = "IheatmapAxes"),
                 function(x, value){
                   for (i in seq_along(x)){
                     x[[i]]@domain_start <- value[i]
                   }
                   x})


setMethod(domain_end, c(x = "IheatmapAxis"),
          function(x){x@domain_end})


setMethod(domain_end, c(x = "IheatmapAxes"),
          function(x){vapply(x, domain_end, 1)})


setReplaceMethod("domain_end", c(x = "IheatmapAxis"),
                 function(x, value){x@domain_end <- value; x})


setReplaceMethod("domain_end", c(x = "IheatmapAxes"),
                 function(x, value){
                   for (i in seq_along(x)){
                     x[[i]]@domain_end <- value[i]
                   }
                   x})


setMethod(id, c(x = "IheatmapAxis"),
          function(x){x@id})


setMethod(id, c(x = "IheatmapAxes"),
          function(x){vapply(x, id, "")})


setMethod(xaxis_name, c(x = "IheatmapPanel"),
          function(x){x@xaxis})


setMethod(xaxis_name, c(x = "IheatmapPanels"),
          function(x){vapply(x, xaxis_name,"")})


setMethod(xaxis_name, c(x = "Iheatmap"),
          function(x){
            unlist(c(xaxis_name(x@plots),
                     xaxis_name(x@shapes),
                     xaxis_name(x@annotations)))})



setMethod(yaxis_name, c(x = "IheatmapPanel"),
          function(x){x@yaxis})


setMethod(yaxis_name, c(x = "IheatmapPanels"),
          function(x){vapply(x, yaxis_name, "")})


setMethod(yaxis_name, c(x = "Iheatmap"),
          function(x){
            unlist(c(yaxis_name(x@plots),
                     yaxis_name(x@shapes),
                     yaxis_name(x@annotations)))})


setMethod('buffers', c(x = "IheatmapAxes"),
          function(x){
            stopifnot(isSorted(x))
            if (length(x) > 1) domain_start(x)[2:length(x)] -
              domain_end(x)[1:(length(x) - 1)] else c()
          })

setMethod('sort', c(x = "IheatmapAxes"),
          function(x, decreasing = FALSE){
            ix <- order(domain_start(x), decreasing = decreasing)
            x[ix]
          })

setMethod('isSorted', c(x = "IheatmapAxes"),
          function(x){
            identical(x, sort(x))
          })

setReplaceMethod("axis_order", c("IheatmapMainAxis"),
                 function(x, value){
                   if (x@categorical){
                     x@order <- value
                     return(x)
                   } else{
                     stop("Cannot reorder continuous axis!")
                   }
                   })


setMethod(add_axis, c(p = "IheatmapHorizontal", new_axis = "IheatmapX"),
          function(p,
                   new_axis,
                   xname,
                   size = 1,
                   buffer = 0.04,
                   side = c("left","right"),
                   ...){

            side <- match.arg(side)
            axes <- sort(xaxes(p))

            if (xname %in% names(axes)){
              return(p)
            }

            ids <- id(axes)
            main_ix <- which(ids == "x")

            starts <- unname(domain_start(axes))
            ends <- unname(domain_end(axes))

            sizes <- ends - starts
            main_size <- sizes[main_ix]

            new_size <- main_size * size

            buffer_sizes <- c(unname(buffers(axes)), buffer * main_size)

            norm_factor <- sum(sizes) + sum(buffer_sizes) + new_size
            new_size <- new_size / norm_factor
            buffer_sizes <- buffer_sizes / norm_factor
            sizes <- sizes / norm_factor

            if (side == "right"){

              domain_start(axes) <- starts / norm_factor
              domain_end(axes) <- ends / norm_factor

              domain_start(new_axis) <- 1 - new_size
              domain_end(new_axis) <- 1

              axes[[xname]] <- new_axis

            } else{

              domain_start(axes) <- 1  - ((1 - starts) / norm_factor)
              domain_end(axes) <- 1 - ((1 - ends) / norm_factor)

              domain_start(new_axis) <- 0
              domain_end(new_axis) <- new_size

              axes[[xname]] <- new_axis

            }
            p@xaxes <- axes
            return(p)
          })

setMethod(add_axis, c(p = "IheatmapHorizontal", new_axis = "IheatmapY"),
          function(p,
                   new_axis,
                   yname,
                   xname,
                   size = 1,
                   buffer = 0.04,
                   side = c("top","bottom")){
            side <- match.arg(side)

            if (yname %in% names(yaxes(p))){
              if (yname %in% names(yaxes(p, xname))){
                return(p)
              } else{
                existing <- TRUE
              }
            } else{
              existing <- FALSE
            }

            axes <- yaxes(p)

            current <- sort(yaxes(p, xname))

            ids <- id(current)
            main_ix <- which(ids == "y")

            starts <- unname(domain_start(current))
            ends <- unname(domain_end(current))

            sizes <- ends - starts
            main_size <- sizes[main_ix]

            if (existing){
              existing_start <- domain_start(yaxes(p)[[yname]])
              existing_end <- domain_end(yaxes(p)[[yname]])
              new_size <- existing_end - existing_start
              other_x <- names(xaxes(p, yname))
              y_for_other_x <- sort(yaxes(p, other_x[[1]]))
              if (side == "top" && existing_start < ends[main_ix])
                stop("Mismatch between side and existing yname")
              if (side == "bottom" && existing_start > starts[main_ix])
                stop("Mismatch between side and existing yname")
              y_ix <- which(names(y_for_other_x) == yname)
              new_buffer_size <- if (side == "top"){
                domain_start(y_for_other_x[y_ix]) - 
                  domain_end(y_for_other_x[y_ix - 1])
              }  else{
                domain_start(y_for_other_x[y_ix + 1]) - 
                  domain_end(y_for_other_x[y_ix])
              }
            } else{
              new_size <- main_size * size
              new_buffer_size <- buffer * main_size
            }

            buffer_sizes <- unname(buffers(current))

            if (side == "top"){
              if (existing){
                resize <-  existing_start < (max(ends) + new_buffer_size)
              } else{
                needed_room <- new_size + new_buffer_size
                available_room <- 1 - max(ends)
                resize <- needed_room > available_room
              }

              if (resize){
                #need to adjust sizes
                if (existing){
                  norm_factor <- sum(sizes) + sum(buffer_sizes) + new_size +
                    new_buffer_size + min(starts) + (1-existing_end)
                } else{
                  norm_factor <- sum(sizes) + sum(buffer_sizes) + new_size +
                    new_buffer_size + min(starts)
                }
                sizes <- sizes / norm_factor
                buffer_sizes <- c(buffer_sizes, new_buffer_size) / norm_factor
                new_size <- new_size / norm_factor
                j <- min(starts) / norm_factor
                for (i in seq_along(sizes)){
                  domain_start(axes[[names(current)[i]]]) <- j
                  domain_end(axes[[names(current)[i]]]) <- j + sizes[i]
                  j <- j + sizes[i] + buffer_sizes[i]
                }
                if (existing){
                  domain_start(axes[[yname]]) <- j
                  domain_end(axes[[yname]]) <- one_bounded(j + new_size)
                } else{
                  domain_start(new_axis) <- j
                  domain_end(new_axis) <- one_bounded(j + new_size)
                  axes[[yname]] <- new_axis
                }
              } else{
                if (!existing){
                  domain_start(new_axis) <- max(ends) + new_buffer_size
                  domain_end(new_axis) <- one_bounded(max(ends) + 
                                                        new_buffer_size +
                                                        new_size)
                  axes[[yname]] <- new_axis
                }
              }
            } else if (side == "bottom"){
              if (existing){
                resize <- existing_end > (min(starts) - new_buffer_size)
              } else{
                needed_room <- new_size + new_buffer_size
                available_room <- min(starts)
                resize <- needed_room > available_room
              }
              if (resize){
                #need to adjust sizes
                if (existing){
                  norm_factor <- sum(sizes) + sum(buffer_sizes) +
                    new_size + new_buffer_size + (1 - max(ends)) +
                    existing_start
                } else{
                  norm_factor <- sum(sizes) + sum(buffer_sizes) +
                    new_size + new_buffer_size + (1 - max(ends))
                }
                sizes <- sizes / norm_factor
                buffer_sizes <- c(new_buffer_size, buffer_sizes) / norm_factor
                new_buffer_size <- new_buffer_size / norm_factor
                new_size <- new_size / norm_factor
                j <- 1 - ((1 - (max(ends))) / norm_factor)
                for (i in rev(seq_along(sizes))){
                  domain_start(axes[[names(current)[i]]]) <- j - sizes[i]
                  domain_end(axes[[names(current)[i]]]) <- j
                  j <- j - sizes[i] - buffer_sizes[i]
                }
                if (existing){
                  domain_start(axes[[yname]]) <- zero_bounded(j - new_size)
                  domain_end(axes[[yname]]) <- j
                } else{
                  domain_start(new_axis) <- zero_bounded(j - new_size)
                  domain_end(new_axis) <- j
                  axes[[yname]] <- new_axis
                }
              } else{
                if (!existing){
                  domain_start(new_axis) <- zero_bounded(min(starts) - 
                                                           new_buffer_size -
                                                           new_size)
                  domain_end(new_axis) <- min(starts) - new_buffer_size
                  axes[[yname]] <- new_axis
                }
              }
            }
            if (resize){
              #Adjust additional y axes as well
              additional_x_axes <- xaxes(p)[names(xaxes(p)) != xname]

              for (k in names(additional_x_axes)){
                additional <- sort(yaxes(p, k))
                if (length(additional) >= 2){
                  additional_starts <- unname(domain_start(additional)) 
                  additional_ends <- unname(domain_end(additional))
                  additional_sizes <- (additional_ends - additional_starts) /
                    norm_factor
                  additional_buffer_sizes <- c(unname(buffers(additional)),0) /
                    norm_factor
                  new_ybottom <- domain_start(axes[[which(id(axes) == "y")]])
                  old_ybottom <- domain_start(yaxes(p)[[which(id(yaxes(p)) == 
                                                                "y")]])
                  old_bottom <- min(additional_starts)
                  j <- zero_bounded(new_ybottom - ((old_ybottom - old_bottom) / 
                                             norm_factor))
                  for (i in seq_along(additional_sizes)){
                    if (names(additional)[i] %ni% c(yname,names(current))){
                      domain_start(axes[[names(additional)[i]]]) <- j
                      domain_end(axes[[names(additional)[i]]]) <- 
                        one_bounded(j + additional_sizes[i])
                      j <- j + additional_sizes[i] + additional_buffer_sizes[i]
                    } else{
                      j <- domain_end(axes[[names(additional)[i]]]) +
                        additional_buffer_sizes[i]
                    }
                  }
                }
              }
            }
            p@yaxes <- axes
            return(p)

          })

setMethod(add_axis, c(p = "IheatmapVertical", new_axis = "IheatmapY"),
          function(p,
                   new_axis,
                   yname,
                   size = 1,
                   buffer = 0.04,
                   side = c("top","bottom"),
                   ...){

            side <- match.arg(side)
            axes <- sort(yaxes(p))

            if (yname %in% names(axes)){
              return(p)
            }

            ids <- id(axes)
            main_ix <- which(ids == "y")

            starts <- unname(domain_start(axes))
            ends <- unname(domain_end(axes))

            sizes <- ends - starts
            main_size <- sizes[main_ix]

            new_size <- main_size * size

            buffer_sizes <- c(unname(buffers(axes)), buffer * main_size)

            norm_factor <- sum(sizes) + sum(buffer_sizes) + new_size
            new_size <- new_size / norm_factor
            buffer_sizes <- buffer_sizes / norm_factor
            sizes <- sizes / norm_factor

            if (side == "top"){

              domain_start(axes) <- starts / norm_factor
              domain_end(axes) <- ends / norm_factor

              domain_start(new_axis) <- 1 - new_size
              domain_end(new_axis) <- 1

              axes[[yname]] <- new_axis

            } else{

              domain_start(axes) <- 1  - ((1 - starts) / norm_factor)
              domain_end(axes) <- 1 - ((1 - ends) / norm_factor)

              domain_start(new_axis) <- 0
              domain_end(new_axis) <- new_size

              axes[[yname]] <- new_axis

            }
            p@yaxes <- axes
            return(p)
          })

setMethod(add_axis, c(p = "IheatmapVertical", new_axis = "IheatmapX"),
          function(p,
                   new_axis,
                   xname,
                   yname,
                   size = 1,
                   buffer = 0.04,
                   side = c("right","left")){
            side <- match.arg(side)

            if (xname %in% names(xaxes(p))){
              if (xname %in% names(xaxes(p, yname))){
                return(p)
              } else{
                existing <- TRUE
              }
            } else{
              existing <- FALSE
            }

            axes <- xaxes(p)

            current <- sort(xaxes(p, yname))

            ids <- id(current)
            main_ix <- which(ids == "x")

            starts <- unname(domain_start(current))
            ends <- unname(domain_end(current))

            sizes <- ends - starts
            main_size <- sizes[main_ix]

            if (existing){
              existing_start <- domain_start(xaxes(p)[[xname]])
              existing_end <- domain_end(xaxes(p)[[xname]])
              new_size <- existing_end - existing_start
              other_y <- names(yaxes(p, xname))
              x_for_other_y <- sort(xaxes(p, other_y[[1]]))
              if (side == "right" && existing_start < ends[main_ix])
                stop("Mismatch between side and existing x axis with xname")
              if (side == "left" && existing_start > starts[main_ix])
                stop("Mismatch between side and existing x axis with xname")
              x_ix <- which(names(x_for_other_y) == xname)
              new_buffer_size <- if (side == "top"){
                 domain_start(x_for_other_y[x_ix]) - 
                  domain_end(x_for_other_y[x_ix - 1])
              } else{
                domain_start(x_for_other_y[x_ix + 1]) - 
                  domain_end(x_for_other_y[x_ix])
              }
            } else{
              new_size <- main_size * size
              new_buffer_size <- buffer * main_size
            }

            buffer_sizes <- unname(buffers(current))

            if (side == "right"){
              if (existing){
                resize <-  existing_start < (max(ends) + new_buffer_size)
              } else{
                needed_room <- new_size + new_buffer_size
                available_room <- 1 - max(ends)
                resize <- needed_room > available_room
              }

              if (resize){
                #need to adjust sizes
                if (existing){
                  norm_factor <- sum(sizes) + sum(buffer_sizes) + new_size +
                    new_buffer_size + min(starts) + (1-existing_end)
                } else{
                  norm_factor <- sum(sizes) + sum(buffer_sizes) + new_size +
                    new_buffer_size + min(starts)
                }
                sizes <- sizes / norm_factor
                buffer_sizes <- c(buffer_sizes, new_buffer_size) / norm_factor
                new_size <- new_size / norm_factor
                j <- min(starts) / norm_factor
                for (i in seq_along(sizes)){
                  domain_start(axes[[names(current)[i]]]) <- j
                  domain_end(axes[[names(current)[i]]]) <- j + sizes[i]
                  j <- j + sizes[i] + buffer_sizes[i]
                }
                if (existing){
                  domain_start(axes[[xname]]) <- j
                  domain_end(axes[[xname]]) <- one_bounded(j + new_size)
                } else{
                  domain_start(new_axis) <- j
                  domain_end(new_axis) <- one_bounded(j + new_size)
                  axes[[xname]] <- new_axis
                }
              } else{
                if (!existing){
                  domain_start(new_axis) <- max(ends) + new_buffer_size
                  domain_end(new_axis) <- one_bounded(max(ends) + 
                                                        new_buffer_size +
                                                        new_size)
                  axes[[xname]] <- new_axis
                }
              }
            } else if (side == "left"){
              if (existing){
                resize <- existing_end > (min(starts) - new_buffer_size)
              } else{
                needed_room <- new_size + new_buffer_size
                available_room <- min(starts)
                resize <- needed_room > available_room
              }
              if (resize){
                #need to adjust sizes
                if (existing){
                  norm_factor <- sum(sizes) + sum(buffer_sizes) +
                    new_size + new_buffer_size + (1 - max(ends)) +
                    existing_start
                } else{
                  norm_factor <- sum(sizes) + sum(buffer_sizes) +
                    new_size + new_buffer_size + (1 - max(ends))
                }
                sizes <- sizes / norm_factor
                buffer_sizes <- c(new_buffer_size, buffer_sizes) / norm_factor
                new_buffer_size <- new_buffer_size / norm_factor
                new_size <- new_size / norm_factor
                j <- 1 - ((1 - (max(ends))) / norm_factor)
                for (i in rev(seq_along(sizes))){
                  domain_start(axes[[names(current)[i]]]) <- j - sizes[i]
                  domain_end(axes[[names(current)[i]]]) <- j
                  j <- j - sizes[i] - buffer_sizes[i]
                }
                if (existing){
                  domain_start(axes[[xname]]) <- zero_bounded(j - new_size)
                  domain_end(axes[[xname]]) <- j
                } else{
                  domain_start(new_axis) <- zero_bounded(j - new_size)
                  domain_end(new_axis) <- j
                  axes[[xname]] <- new_axis
                }
              } else{
                if (!existing){
                  domain_start(new_axis) <- zero_bounded(min(starts) - 
                                                           new_buffer_size -
                                                           new_size)
                  domain_end(new_axis) <- min(starts) - new_buffer_size
                  axes[[xname]] <- new_axis
                }
              }
            }
            if (resize){
              #Adjust additional y axes as well
              additional_y_axes <- yaxes(p)[names(yaxes(p)) != yname]

              for (k in names(additional_y_axes)){
                additional <- sort(xaxes(p, k))
                if (length(additional) >= 2){
                  additional_starts <- unname(domain_start(additional))
                  additional_ends <- unname(domain_end(additional))
                  additional_sizes <- (additional_ends - additional_starts) /
                    norm_factor
                  additional_buffer_sizes <- c(unname(buffers(additional)),0) /
                    norm_factor
                  new_xbottom <- domain_start(axes[[which(id(axes) == "x")]])
                  old_xbottom <- domain_start(xaxes(p)[[which(id(xaxes(p)) == 
                                                                "x")]])
                  old_bottom <- min(additional_starts)
                  j <- zero_bounded(new_xbottom - ((old_xbottom - old_bottom) / 
                                             norm_factor))
                  for (i in seq_along(additional_sizes)){
                    if (names(additional)[i] %ni% c(xname,names(current))){
                      domain_start(axes[[names(additional)[i]]]) <- j
                      domain_end(axes[[names(additional)[i]]]) <- 
                        one_bounded(j + additional_sizes[i])
                      j <- j + additional_sizes[i] + additional_buffer_sizes[i]
                    } else{
                      j <- domain_end(axes[[names(additional)[i]]]) +
                        additional_buffer_sizes[i]
                    }
                  }
                }
              }
            }
            p@xaxes <- axes
            return(p)

          })
