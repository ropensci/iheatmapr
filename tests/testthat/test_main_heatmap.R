context("main_heatmap")



test_that("can add a main heatmap horizontally",{
  test_plot <- main_heatmap(a) %>% add_main_heatmap(b)
  expect_iheatmap(test_plot, "two_main_heatmap_horizontal")
})

test_that("can add a main heatmap vertically",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
              add_main_heatmap(b)
  expect_iheatmap(test_plot, "two_main_heatmap_vertical","vertical")
})

test_that("main_heatmap returns IHeatmap object",{
  test_plot <- main_heatmap(a)
  expect_iheatmap(test_plot, "main_heatmap")
})

test_that("orientation = 'vertical' returns IHeatmapVertical",{
  test_plot <- main_heatmap(a, orientation = "vertical")
  expect_iheatmap(test_plot, "main_heatmap_vertical","vertical")
})


test_that("main_heatmap gives error if row_order contains invalid indices",{
  expect_error(main_heatmap(a, row_order = 1:30),
               "Row order contains invalid indices")
})

test_that("main_heatmap gives error if col_order contains invalid indices",{
  expect_error(main_heatmap(a, col_order = 1:40),
               "Col order contains invalid indices")
})

test_that("main_heatmap gives error if y not equal to number of rows",{
  expect_error(main_heatmap(a, y = 1:10),
               "y does not match number of rows of matrix")
  expect_error(main_heatmap(a, y = 1:30),
               "y does not match number of rows of matrix")
})

test_that("main_heatmap gives error if x not equal to number of columns",{
  expect_error(main_heatmap(a, x = 1:20),
               "x does not match number of columns of matrix")
  expect_error(main_heatmap(a, x = 1:40),
               "x does not match number of columns of matrix")
})

test_that("main_heatmap gives error if mat is not matrix",{
  expect_error(main_heatmap(1:20))
})
