library(purrr)

# modified from testhat expect_equal_to_reference
expect_ihm_equal_to_reference <- function(object, file, ..., info = NULL,
                                      label = NULL, expected.label = NULL) {
  
  lab_act <- make_label(object, label)
  lab_exp <- expected.label %||% paste0("reference from `", file, "`")
  
  if (!file.exists(file)) {
    # first time always succeeds
    saveRDS(object, file)
    succeed()
  } else {
    reference <- readRDS(file)
    
    objectsub <- object$x[c("data","layout")]
    referencesub <- reference$x[c("data","layout")]
    
    comp <- testthat::compare(objectsub, referencesub, ...)
    expect(
      comp$equal,
      sprintf("%s not equal to %s.\n%s", lab_act, lab_exp, comp$message),
      info = info
    )
  }
  
  invisible(object)
}



expect_iheatmap <- function(test_plot, ref_name, 
                            orientation = c("horizontal","vertical")){
  test_plotly <- test_plot %>% as_plotly()
  orientation <- match.arg(orientation)
  if (orientation == "horizontal"){
    expect_is(test_plot,"IheatmapHorizontal")
  } else{
    expect_is(test_plot,"IheatmapVertical")
  }
  expect_is(test_plotly,"plotly")
  expect_ihm_equal_to_reference(test_plotly, paste0("reference/",ref_name,".rds"))
}