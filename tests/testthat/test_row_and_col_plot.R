context("row and col plot")

test_that("can add a row plot to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_row_plot(row_sig,name = "Test")
  expect_iheatmap(test_plot, "row_plot_horizontal")
})

test_that("can add a row plot to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_row_plot(row_sig,name = "Test")
  expect_iheatmap(test_plot, "row_plot_vertical", "vertical")
})

test_that("can add a column plot to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_col_plot(col_sig,name = "Test")
  expect_iheatmap(test_plot, "col_plot_horizontal")
})

test_that("can add a column plot to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_col_plot(col_sig,name = "Test")
  expect_iheatmap(test_plot, "col_plot_vertical","vertical")
})

test_that("can add a row scatter plot to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_row_plot(row_sig,name = "Test",mode="markers")
  expect_iheatmap(test_plot, "row_scatter_plot_horizontal")
})

test_that("can add a row scatter plot to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_row_plot(row_sig,name = "Test",mode="markers")
  expect_iheatmap(test_plot, "row_scatter_plot_vertical", "vertical")
})

test_that("can add a column scatter plot to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_col_plot(col_sig,name = "Test",mode="markers")
  expect_iheatmap(test_plot, "col_scatter_plot_horizontal")
})

test_that("can add a column scatter plot to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_col_plot(col_sig,name = "Test", mode="markers")
  expect_iheatmap(test_plot, "col_scatter_plot_vertical","vertical")
})
