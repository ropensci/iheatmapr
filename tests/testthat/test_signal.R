context("signal")

test_that("can add a row signal to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_row_signal(row_sig,"Test")
  expect_iheatmap(test_plot, "row_signal_horizontal")
})

test_that("can add a row signal to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_row_signal(row_sig,"Test")
  expect_iheatmap(test_plot, "row_signal_vertical", "vertical")
})

test_that("can add a column signal to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_col_signal(col_sig,"Test")
  expect_iheatmap(test_plot, "col_signal_horizontal")
})

test_that("can add a column signal to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_col_signal(col_sig,"Test")
  expect_iheatmap(test_plot, "col_signal_vertical","vertical")
})


