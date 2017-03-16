context("title")

test_that("can add a row title to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_row_title("Test")
  expect_iheatmap(test_plot, "row_title_horizontal")
})

test_that("can add a row title to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_row_title("Test")
  expect_iheatmap(test_plot, "row_title_vertical", "vertical")
})

test_that("can add a column title to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_col_title("Test")
  expect_iheatmap(test_plot, "col_title_horizontal")
})

test_that("can add a column title to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_col_title("Test")
  expect_iheatmap(test_plot, "col_title_vertical","vertical")
})

