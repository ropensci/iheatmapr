#library(iheatmapr)
context("adjust_x_domains")

a <- matrix(rnorm(200),ncol=10)

test_that("adding a main heatmap of same size to right gives correct sizing",{
  p <- main_heatmap(a) %>% add_main_heatmap(a, size = 1, buffer = 0.015)
  x1_start <- domain_start(xaxes(p)[["x"]])
  x1_end <- domain_end(xaxes(p)[["x"]])
  x2_start <- domain_start(xaxes(p)[["x2"]])
  x2_end <- domain_end(xaxes(p)[["x2"]])
  x1_size <- x1_end - x1_start
  x2_size <- x2_end - x2_start

  expect_equal(x1_size, x2_size)
  expect_equal(x1_start, 0)
  expect_equal(x2_end, 1)
  expect_equal(x2_start - x1_end, 0.015 * x1_size)

})

test_that("adding a main heatmap of different size to right gives right size",{
  p <- main_heatmap(a) %>% add_main_heatmap(a, size = 0.5, buffer = 0.015)
  x1_start <- domain_start(xaxes(p)[["x"]])
  x1_end <- domain_end(xaxes(p)[["x"]])
  x2_start <- domain_start(xaxes(p)[["x2"]])
  x2_end <- domain_end(xaxes(p)[["x2"]])
  x1_size <- x1_end - x1_start
  x2_size <- x2_end - x2_start

  expect_equal(x1_size * 0.5, x2_size)
  expect_equal(x1_start, 0)
  expect_equal(x2_end, 1)
  expect_equal(x2_start - x1_end, 0.015 * x1_size)
})


test_that("adding a main heatmap of same size to left gives correct sizing",{
  p <- main_heatmap(a)  %>% add_main_heatmap(a, size = 1, buffer = 0.015,
                                             side = "left")
  x1_start <- domain_start(xaxes(p)[["x"]])
  x1_end <- domain_end(xaxes(p)[["x"]])
  x2_start <- domain_start(xaxes(p)[["x2"]])
  x2_end <- domain_end(xaxes(p)[["x2"]])
  x1_size <- x1_end - x1_start
  x2_size <- x2_end - x2_start

  expect_equal(x1_size, x2_size)
  expect_equal(x1_end, 1)
  expect_equal(x2_start, 0)
  expect_equal(x1_start - x2_end, 0.015 * x1_size)
})

test_that("adding a main heatmap of different size to left gives right size",{
  p <- main_heatmap(a) %>% add_main_heatmap(a, size = 0.2, buffer = 0.035, 
                                            side = "left")
  x1_start <- domain_start(xaxes(p)[["x"]])
  x1_end <- domain_end(xaxes(p)[["x"]])
  x2_start <- domain_start(xaxes(p)[["x2"]])
  x2_end <- domain_end(xaxes(p)[["x2"]])
  x1_size <- x1_end - x1_start
  x2_size <- x2_end - x2_start

  expect_equal(x1_size * 0.2, x2_size)
  expect_equal(x1_end, 1)
  expect_equal(x2_start, 0)
  expect_equal(x1_start - x2_end, 0.035 * x1_size)
})

test_that("adding a third plot to right gives correct sizing",{
  p <- main_heatmap(a) %>%
    add_main_heatmap(a, size = 1, buffer = 0.015) %>%
    add_row_signal(1:20, "test", side = "right", size = 0.2, buffer = 0.2)
  x1_start <- domain_start(xaxes(p)[["x"]])
  x1_end <- domain_end(xaxes(p)[["x"]])
  x2_start <- domain_start(xaxes(p)[["x2"]])
  x2_end <- domain_end(xaxes(p)[["x2"]])
  x3_start <- domain_start(xaxes(p)[["x3"]])
  x3_end <- domain_end(xaxes(p)[["x3"]])
  x1_size <- x1_end - x1_start
  x2_size <- x2_end - x2_start
  x3_size <- x3_end - x3_start

  expect_equal(x1_size, x2_size)
  expect_equal(x1_size * 0.2, x3_size)
  expect_equal(x1_start, 0)
  expect_equal(x3_end, 1)
  expect_equal(x2_start - x1_end, 0.015 * x1_size)
  expect_equal(x3_start - x2_end, 0.2 * x1_size)
})
