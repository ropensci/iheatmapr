context("summary")

test_that("can add a row summary to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_row_summary()
  expect_iheatmap(test_plot, "row_summary_horizontal")
})

test_that("can add a row summary to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_row_summary()
  expect_iheatmap(test_plot, "row_summary_vertical", "vertical")
})

test_that("can add a column summary to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_col_summary()
  expect_iheatmap(test_plot, "col_summary_horizontal")
})

test_that("can add a column summary to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_col_summary()
  expect_iheatmap(test_plot, "col_summary_vertical","vertical")
})


test_that("can add a row summary with groups to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_row_summary(groups = col_grp)
  expect_iheatmap(test_plot, "row_summary_groups_horizontal")
})

test_that("can add a row summary with groups to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_row_summary(groups = col_grp)
  expect_iheatmap(test_plot, "row_summary_groups_vertical", "vertical")
})

test_that("can add a column summary with groups to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_col_summary(groups = row_grp)
  expect_iheatmap(test_plot, "col_summary_groups_horizontal")
})

test_that("can add a column summary with groups to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_col_summary(groups = row_grp)
  expect_iheatmap(test_plot, "col_summary_groups_vertical","vertical")
})


