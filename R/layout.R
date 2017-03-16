

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


