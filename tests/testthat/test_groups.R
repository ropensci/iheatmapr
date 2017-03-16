context("groups")

test_that("can add a row groups to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_row_groups(row_grp,"Test")
  expect_iheatmap(test_plot, "row_groups_horizontal")
})

test_that("can add a row groups to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% add_row_groups(row_grp,"Test")
  expect_iheatmap(test_plot, "row_groups_vertical", "vertical")
})

test_that("can add a column groups to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_col_groups(col_grp,"Test")
  expect_iheatmap(test_plot, "col_groups_horizontal")
})

test_that("can add a column groups to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% add_col_groups(col_grp,"Test")
  expect_iheatmap(test_plot, "col_groups_vertical", "vertical")
})


