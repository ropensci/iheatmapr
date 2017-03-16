context("complex_examples")

test_that("Can make shared axes on bottom with gap in one column",{
  test_plot <- iheatmap(a) %>% 
    add_subplot(x = 1:10, y=1:10, side = "top") %>% 
    add_iheatmap(b, side = "left") %>% 
    add_subplot(x = 1:10, y=1:10, side = "bottom", yname = "bah") %>% 
    add_subplot(x = 1:10, y=1:10, side = "top") %>% 
    add_subplot(x = 1:10, y=1:10, side = "bottom", xname = "x") %>% 
    add_subplot(x = 1:10, y=1:10, side = "bottom", xname = "x", yname = "bah") %>% 
    add_subplot(x = 1:10, y=1:10, side = "bottom")
  expect_iheatmap(test_plot, "shared_axes_gap_bottom_horizontal")
})

test_that("Can make shared axes on top with gap in one column",{
  test_plot <- iheatmap(a) %>% 
    add_subplot(x = 1:10, y=1:10, side = "top") %>% 
    add_iheatmap(b, side = "left") %>% 
    add_subplot(x = 1:10, y=1:10, side = "top", yname = "bah") %>% 
    add_subplot(x = 1:10, y=1:10, side = "top") %>% 
    add_subplot(x = 1:10, y=1:10, side = "bottom", xname = "x") %>% 
    add_subplot(x = 1:10, y=1:10, side = "top", xname = "x", yname = "bah") %>% 
    add_subplot(x = 1:10, y=1:10, side = "bottom")
  expect_iheatmap(test_plot, "shared_axes_gap_top_horizontal")
})

test_that("Can make shared axes on left with gap in one row",{
  test_plot <- iheatmap(a, orientation = "vertical") %>% 
    add_subplot(x = 1:10, y=1:10, side = "right") %>% 
    add_iheatmap(b, side = "bottom") %>% 
    add_subplot(x = 1:10, y=1:10, side = "left", xname = "bah") %>% 
    add_subplot(x = 1:10, y=1:10, side = "right") %>% 
    add_subplot(x = 1:10, y=1:10, side = "left", yname = "y") %>% 
    add_subplot(x = 1:10, y=1:10, side = "left", yname = "y", xname = "bah") %>% 
    add_subplot(x = 1:10, y=1:10, side = "left")
  expect_iheatmap(test_plot, "shared_axes_gap_left_vertical", "vertical")
})

test_that("Can make shared axes on right with gap in one row",{
  test_plot <- iheatmap(a, orientation = "vertical") %>% 
    add_subplot(x = 1:10, y=1:10, side = "right") %>% 
    add_iheatmap(b, side = "bottom") %>% 
    add_subplot(x = 1:10, y=1:10, side = "right", xname = "bah") %>% 
    add_subplot(x = 1:10, y=1:10, side = "right") %>% 
    add_subplot(x = 1:10, y=1:10, side = "left", yname = "y") %>% 
    add_subplot(x = 1:10, y=1:10, side = "right", yname = "y", xname = "bah") %>% 
    add_subplot(x = 1:10, y=1:10, side = "left")
  expect_iheatmap(test_plot, "shared_axes_gap_right_vertical", "vertical")
})

test_that("Can make plot with many components",{
  test_plot <- main_heatmap(a) %>% 
    add_row_groups(row_grp) %>%
    add_row_clustering() %>% 
    add_col_groups(col_grp) %>%
    add_col_clustering() %>%
    add_main_heatmap(b) %>%
    add_col_summary(groups = TRUE) %>%
    add_row_labels(side = "right") %>%
    add_col_signal(col_sig, "Test",xname = "x", side = "bottom") %>%
    add_col_labels()
  expect_iheatmap(test_plot, "complex_example_horizontal")
})

test_that("Can make plot with many components vertically",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_row_clustering() %>% 
    add_row_groups(row_grp, side = "right") %>%
    add_col_clustering(k = 2) %>%
    add_main_heatmap(b) %>%
    add_col_groups(col_grp, side = "bottom", name = "Groups") %>%
    add_row_summary(groups = "Groups") %>%
    add_row_labels(side = "right", y = "y", tickvals = c(1,20)) %>%
    add_col_signal(col_sig, "Test", side = "bottom") %>%
    add_col_labels() %>%
    add_col_title("Test!")
  expect_iheatmap(test_plot, "complex_example_vertical", "vertical")
})



