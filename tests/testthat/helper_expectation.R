remove_text <- function(x) {
  # Colors are problematic because a slight change in color will lead to test 
  # failing -- this helper function removes them.
  x$colorscale <- x$colorscale[[1]]
  # Text is problematic because it is sensitive to precision because has numbers in it
  x$text <- length(x$text)

  x
}

replace_text <- function(x) {
  x$data <- lapply(x$data, remove_text)
  x
}


# modified from testhat expect_equal_to_reference
expect_ihm_equal_to_reference <- function(object, file, ..., info = NULL) {
  
  lab_exp <- paste0("reference from `", file, "`")
    
  if (!file.exists(file)) {
    # first time always succeeds
    saveRDS(object, file)
    succeed()
  } else {
    reference <- readRDS(file)
    
    objectsub <- replace_text(object$x[c("data","layout")])
    referencesub <- replace_text(reference$x[c("data","layout")])
    
    comp <- testthat::compare(objectsub, referencesub, tolerance = 0.1, ...)
    expect(
      comp$equal,
      sprintf("Not equal to %s.\n%s", lab_exp, comp$message),
      info = info
    )
  }
  
  invisible(object)
}



expect_iheatmap <- function(test_plot, ref_name, 
                            orientation = c("horizontal","vertical")){
  test_widget <- test_plot %>% to_widget()
  orientation <- match.arg(orientation)
  if (orientation == "horizontal"){
    expect_is(test_plot,"IheatmapHorizontal")
  } else{
    expect_is(test_plot,"IheatmapVertical")
  }
  expect_is(test_widget,"htmlwidget")
  expect_is(test_widget,"iheatmapr")
  expect_ihm_equal_to_reference(test_widget, paste0("reference/",
                                                    ref_name,".rds"))
}