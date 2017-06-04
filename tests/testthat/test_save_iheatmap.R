context("save_iheatmap")


test_that("error when saving with unsupported type",{
  expect_error(main_heatmap(a) %>% save_iheatmap("test.js"))
})