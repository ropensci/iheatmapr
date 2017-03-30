# makes x based on colnames of mat if available
# if not available, just uses 1 to number of columns
default_x <- function(mat){
  if (is.null(colnames(mat))){
    return(seq_len(ncol(mat)))
  } else{
    colnames(mat)
  }
}
# makes y based on rownames of mat if available
# if not available, just uses 1 to number of rows
default_y <- function(mat){
  if (is.null(rownames(mat))){
    return(seq_len(nrow(mat)))
  } else{
    rownames(mat)
  }
}

is_categorical <- function(vals, var_order){
  if (!all(var_order == seq_along(vals))){
    return(TRUE)
  } else if (!is.numeric(vals)){
    return(TRUE)
  } else if (all(vals == seq_along(vals) )){
    return(TRUE)
  } else{
    return(FALSE)
  }
}



setMethod(get_heatmap, c(p = "IheatmapHorizontal"),
          function(p, xname, side = c("right","left","top","bottom"),...){
            side <- match.arg(side)
            candidates <- which(vapply(plots(p), is, FALSE, "MainHeatmap"))
            if (length(candidates) == 1) return(plots(p)[[candidates]])
            xcand <- xaxis_name(plots(p)[candidates])
            if (side == "left"){
              left <- xcand[which.min(domain_start(xaxes(p)[xcand]))]
              out <- plots(p)[[candidates[which(xcand == left)]]]
            } else if (side == "right"){
              right <- xcand[which.max(domain_start(xaxes(p)[xcand]))]
              out <- plots(p)[[candidates[which(xcand == right)]]]
            } else {
              out <- plots(p)[[candidates[which(xcand == xname)]]]
            }
            out
          })

setMethod(get_heatmap, c(p = "IheatmapVertical"),
          function(p, yname, side = c("right","left","top","bottom"),...){
            side <- match.arg(side)
            candidates <- which(vapply(plots(p), is, FALSE, "MainHeatmap"))
            if (length(candidates) == 1) return(plots(p)[[candidates]])
            ycand <- yaxis_name(plots(p)[candidates])
            if (side == "bottom"){
              bottom <- ycand[which.min(domain_start(yaxes(p)[ycand]))]
              out <- plots(p)[[candidates[which(ycand == bottom)]]]
            } else if (side == "top"){
              top <- ycand[which.max(domain_start(yaxes(p)[ycand]))]
              out <- plots(p)[[candidates[which(ycand == top)]]]
            } else {
              out <- plots(p)[[candidates[which(ycand == yname)]]]
            }
            out
          })


setMethod(get_row_groups, c(p = "IheatmapHorizontal"),
          function(p,...){
            candidates <- which(vapply(plots(p), is, FALSE, "RowAnnotation"))
            if (length(candidates) > 0){
              ix <- which(vapply(plots(p)[candidates],
                                 function(x){
                                   is(colorbars(p)[[x@colorbar]], 
                                      "DiscreteColorbar")
                                 }, FALSE))
              candidates <- candidates[ix]
            } 
            return(plots(p)[candidates])
          })

setMethod(get_row_groups, c(p = "IheatmapVertical"),
          function(p, yname){
            candidates <- which(vapply(plots(p), is, FALSE, "RowAnnotation"))
            if (length(candidates) == 0) return(plots(p)[c()])
            candidates <- candidates[which(vapply(plots(p)[candidates],
                                                  function(x){
                                                    x@yaxis == yname
                                                  }, FALSE))]
            if (length(candidates) == 0) return(plots(p)[c()])
            ix <- which(vapply(plots(p)[candidates],
                               function(x){
                                 is(colorbars(p)[[x@colorbar]], 
                                    "DiscreteColorbar")
                               }, FALSE))
            candidates <- candidates[ix]
            return(plots(p)[candidates])
          })

setMethod(get_col_groups, c(p = "IheatmapVertical"),
          function(p,...){
            candidates <- which(vapply(plots(p), is, FALSE, "ColumnAnnotation"))
            if (length(candidates) > 0){
              ix <- which(vapply(plots(p)[candidates],
                                 function(x){
                                   is(colorbars(p)[[x@colorbar]], 
                                      "DiscreteColorbar")
                                 }, FALSE))
              candidates <- candidates[ix]
            } 
            return(plots(p)[candidates])
          })

setMethod(get_col_groups, c(p = "IheatmapHorizontal"),
          function(p, xname){
            candidates <- which(vapply(plots(p), is, FALSE, "ColumnAnnotation"))
            if (length(candidates) == 0) return(plots(p)[c()])
            candidates <- candidates[which(vapply(plots(p)[candidates],
                                                  function(x){
                                                    x@xaxis == xname
                                                  }, FALSE))]
            if (length(candidates) == 0) return(plots(p)[c()])
            ix <- which(vapply(plots(p)[candidates],
                               function(x){
                                 is(colorbars(p)[[x@colorbar]], 
                                    "DiscreteColorbar")
                               }, FALSE))
            candidates <- candidates[ix]
            return(plots(p)[candidates])
          })

scale_mat <- function(mat, 
                      scale = c("rows","cols"), 
                      scale_method = c("standardize","center","normalize")){
  scale <- match.arg(scale)
  scale_method <- match.arg(scale_method)
  if (scale_method == "standardize"){
    scale_func <- function(x){
      centered <- x - mean(x, na.rm = TRUE)
      dev <- stats::sd(centered, na.rm = TRUE)
      if (dev == 0){
        return(centered)
      } else{
        return(centered / dev)
      }
    }
  } else if (scale_method == "center"){
    scale_func <- function(x){
      x - mean(x, na.rm = TRUE)
    }
  } else if (scale_method == "normalize"){
    if (min(mat) < 0) 
      stop("normalize method can only be used with positive values")
    scale_func <- function(x){
      m <- mean(x, na.rm = TRUE)
      if (m == 0){
        x
      } else{
        x / m
      }
    }
  }
  if (scale == "rows"){
    mat <- t(apply(mat, 1, scale_func))
  } else if (scale == "cols"){
    mat <- apply(mat, 2, scale_func)
  }
  return(mat)
}

pname_check <- function(pname, p){
  stopifnot(is.character(pname))
  if (pname %in% names(plots(p))){
    same_pre <- grep(paste0(pname,"[[:digit:]]*$"),names(plots(p)))
    numbers <- vapply(names(plots(p))[same_pre],
                      function(x){
                        m <- regexpr("[[:digit:]]+$", x)
                        if (m == -1){
                          1L
                        } else{
                          as.integer(substr(x,m,nchar(x)))
                        }
                      }, 1)
    out <- paste0(pname, max(numbers) + 1)
  } else{
    out <- pname
  }
  return(out)
}

sname_check <- function(sname, p){
  stopifnot(is.character(sname))
  if (sname %in% names(shapes(p))){
    same_pre <- grep(paste0(sname,"[[:digit:]]*$"),names(shapes(p)))
    numbers <- vapply(names(shapes(p))[same_pre],
                      function(x){
                        m <- regexpr("[[:digit:]]+$", x)
                        if (m == -1){
                          1L
                        } else{
                          as.integer(substr(x,m,nchar(x)))
                        }
                      }, 1)
    out <- paste0(sname, max(numbers) + 1)
  } else{
    out <- sname
  }
  return(out)
}

aname_check <- function(aname, p){
  stopifnot(is.character(aname))
  if (aname %in% names(annotations(p))){
    same_pre <- grep(paste0(aname,"[[:digit:]]*$"),names(annotations(p)))
    numbers <- vapply(names(annotations(p))[same_pre],
                      function(x){
                        m <- regexpr("[[:digit:]]+$", x)
                        if (m == -1){
                          1L
                        } else{
                          as.integer(substr(x,m,nchar(x)))
                        }
                      }, 1)
    out <- paste0(aname, max(numbers) + 1)
  } else{
    out <- aname
  }
  return(out)
}

