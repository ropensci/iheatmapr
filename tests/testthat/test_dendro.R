context("dendrogram")

test_that("can add a row signal to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_row_dendro(row_dendro)
  expect_iheatmap(test_plot, "row_dendro_horizontal")
})

test_that("can add a row signal to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_row_dendro(row_dendro)
  expect_iheatmap(test_plot, "row_dendro_vertical", "vertical")
})

test_that("can add a column signal to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_col_dendro(col_dendro)
  expect_iheatmap(test_plot, "col_dendro_horizontal")
})

test_that("can add a column signal to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_col_dendro(col_dendro)
  expect_iheatmap(test_plot, "col_dendro_vertical", "vertical")
})


