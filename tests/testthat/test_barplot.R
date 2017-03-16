context("barplot")

test_that("can add a row barplot to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_row_barplot(row_sig,name = "Test")
  expect_iheatmap(test_plot, "row_barplot_horizontal")
})

test_that("can add a row barplot to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_row_barplot(row_sig,name = "Test")
  expect_iheatmap(test_plot, "row_barplot_vertical", "vertical")
})

test_that("can add a column barplot to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_col_barplot(col_sig,name = "Test")
  expect_iheatmap(test_plot, "col_barplot_horizontal")
})

test_that("can add a column barplot to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_col_barplot(col_sig,name = "Test")
  expect_iheatmap(test_plot, "col_barplot_vertical","vertical")
})


