#library(iheatmapr)
context("adjust_y_domains")


a <- matrix(rnorm(200),ncol=10)

test_that("adding a col_plot on top gives correct sizing",{
  p <- main_heatmap(a) %>% 
    add_col_signal(1:10, "test", size = 0.2, buffer = 0.015)
  y1_start <- domain_start(yaxes(p)[["y"]])
  y1_end <- domain_end(yaxes(p)[["y"]])
  y2_start <- domain_start(yaxes(p)[["y2"]])
  y2_end <- domain_end(yaxes(p)[["y2"]])
  y1_size <- y1_end - y1_start
  y2_size <- y2_end - y2_start

  expect_equal(y1_size * 0.2, y2_size)
  expect_equal(y1_start, 0)
  expect_equal(y2_end, 1)
  expect_equal(y2_start - y1_end, 0.015 * y1_size)

})


test_that("adding a col_plot on bottom gives correct sizing",{
  p <- main_heatmap(a) %>% 
    add_col_signal(1:10, "test", size = 0.2, buffer = 0.015, side = "bottom")
  y1_start <- domain_start(yaxes(p)[["y"]])
  y1_end <- domain_end(yaxes(p)[["y"]])
  y2_start <- domain_start(yaxes(p)[["y2"]])
  y2_end <- domain_end(yaxes(p)[["y2"]])
  y1_size <- y1_end - y1_start
  y2_size <- y2_end - y2_start

  expect_equal(y1_size * 0.2, y2_size)
  expect_equal(y2_start, 0)
  expect_equal(y1_end, 1)
  expect_equal(y1_start - y2_end, 0.015 * y1_size)
})
