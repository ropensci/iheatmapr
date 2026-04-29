context("groups")

test_that("can add a row groups to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_row_groups(row_grp,"Test")
  expect_iheatmap(test_plot, "row_groups_horizontal")
})

test_that("can add a row groups to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_row_groups(row_grp,"Test")
  expect_iheatmap(test_plot, "row_groups_vertical", "vertical")
})

test_that("can add a column groups to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_col_groups(col_grp,"Test")
  expect_iheatmap(test_plot, "col_groups_horizontal")
})

test_that("can add a column groups to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_col_groups(col_grp,"Test")
  expect_iheatmap(test_plot, "col_groups_vertical", "vertical")
})


test_that("can add groups with different elements",{
  test_plot <- main_heatmap(a) %>% 
    add_col_groups(col_grp,"Test") %>%
    add_main_heatmap(a) %>%
    add_col_groups(rep("C",10),"Test")
  expect_iheatmap(test_plot, "col_groups_distinct")
})


test_that("can add groups and reuse colors",{
  expect_warning(
    test_plot <- main_heatmap(a) %>% 
      add_col_groups(col_grp,"Test1") %>%
      add_col_groups(col_grp,"Test2") %>%
      add_col_groups(col_grp,"Test3") %>%
      add_col_groups(col_grp,"Test4") %>%
      add_col_groups(col_grp,"Test5") %>%
      add_col_groups(col_grp,"Test6") %>%
      add_col_groups(col_grp,"Test7") %>%
      add_col_groups(col_grp,"Test8") %>%
      add_col_groups(col_grp,"Test9") %>%
      add_col_groups(col_grp,"Test10") %>%
      add_col_groups(col_grp,"Test11") %>%
      add_main_heatmap(a),
      "Reusing")
  expect_iheatmap(test_plot, "col_groups_many")
})


