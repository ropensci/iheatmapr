context("labels")

# Categorical default

test_that("can add a row labels to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_row_labels()
  expect_iheatmap(test_plot, "row_labels_horizontal")
})

test_that("can add a row labels to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_row_labels()
  expect_iheatmap(test_plot, "row_labels_vertical", "vertical")
})

test_that("can add a column labels to single horizontal heatmap",{
  test_plot <- main_heatmap(a) %>% add_col_labels()
  expect_iheatmap(test_plot, "col_labels_horizontal")
})

test_that("can add a column labels to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_col_labels()
  expect_iheatmap(test_plot, "col_labels_vertical","vertical")
})

# Categorical non-default

test_that("can add a row labels with custom ticktext of same length",{
  test_plot <- main_heatmap(a) %>% 
    add_row_labels(ticktext = as.character(1:20 - 5))
  expect_iheatmap(test_plot, "row_labels_custom_ticktext1_horizontal")
})

test_that("can add a row labels with custom ticktext selection",{
  test_plot <- main_heatmap(a) %>% 
    add_row_labels(ticktext = c("1","5"))
  expect_iheatmap(test_plot, "row_labels_custom_ticktext2_horizontal")
})

test_that("get errors on invalid ticktext for row labels",{
  expect_error(main_heatmap(a) %>% 
                 add_row_labels(ticktext = c("-1","5")))
  expect_error(main_heatmap(a) %>% 
                 add_row_labels(ticktext = 1:21))
})


test_that("can add a row labels with custom tickvals selection",{
  test_plot <- main_heatmap(a) %>% 
    add_row_labels(ticktext = c(1,5))
  expect_iheatmap(test_plot, "row_labels_custom_tickvals_horizontal")
})

test_that("get errors on invalid tickvals for row labels",{
  expect_error(main_heatmap(a) %>% 
                 add_row_labels(tickvals = c(-1,5)))
  expect_error(main_heatmap(a) %>% 
                 add_row_labels(tickvals = 1:21))
})

test_that("can add a row labels with custom tickvals and ticktext selection",{
  test_plot <- main_heatmap(a) %>% 
    add_row_labels(tickvals = c(1,5), ticktext = c("A","B"))
  expect_iheatmap(test_plot, "row_labels_custom_ticktext_tickvals_horizontal")
})

test_that("get errors on invalid tickvals and ticktext for row labels",{
  expect_error(main_heatmap(a) %>% 
                 add_row_labels(tickvals = c(1,5), ticktext = letters[1:3]))
})

# continuous labels defaults

test_that("can add a row labels to single horizontal heatmap",{
  test_plot <- main_heatmap(a, y_categorical = TRUE) %>% add_row_labels()
  expect_iheatmap(test_plot, "row_labels_continuous_horizontal")
})

test_that("can add a row labels to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical", 
                        y_categorical = TRUE) %>% 
    add_row_labels()
  expect_iheatmap(test_plot, "row_labels_continuous_vertical", "vertical")
})

test_that("can add a column labels to single horizontal heatmap",{
  test_plot <- main_heatmap(a, x_categorical = TRUE) %>% add_col_labels()
  expect_iheatmap(test_plot, "col_labels_continuous_horizontal")
})

test_that("can add a column labels to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical", 
                        x_categorical = TRUE) %>% 
    add_col_labels()
  expect_iheatmap(test_plot, "col_labels_continuous_vertical","vertical")
})

# continuous non-default

test_that("can add continuous row labels with custom ticktext of same length",{
  test_plot <- main_heatmap(a, y_categorical = TRUE) %>% 
    add_row_labels(ticktext = as.character(1:20 - 5))
  expect_iheatmap(test_plot, "row_labels_custom_ticktext1_horizontal")
})

test_that("can add continuous row labels with custom ticktext selection",{
  test_plot <- main_heatmap(a, y_categorical = TRUE) %>% 
    add_row_labels(ticktext = c("1","5"))
  expect_iheatmap(test_plot, "row_labels_custom_ticktext2_horizontal")
})

test_that("get errors on invalid ticktext for continuous row labels",{
  expect_error(main_heatmap(a, y_categorical = TRUE) %>% 
                 add_row_labels(ticktext = c("-1","5")))
  expect_error(main_heatmap(a) %>% 
                 add_row_labels(ticktext = 1:21))
})


test_that("can add continuous row labels with custom tickvals selection",{
  test_plot <- main_heatmap(a, y_categorical = TRUE) %>% 
    add_row_labels(ticktext = c(1,5))
  expect_iheatmap(test_plot, "row_labels_custom_tickvals_horizontal")
})

test_that("get errors on invalid tickvals for continuous row labels",{
  expect_error(main_heatmap(a, y_categorical = TRUE) %>% 
                 add_row_labels(tickvals = c(-1,5)))
  expect_error(main_heatmap(a, y_categorical = TRUE) %>% 
                 add_row_labels(tickvals = 1:21))
})

test_that("can add continuous row labels with custom tickvals and ticktext",{
  test_plot <- main_heatmap(a, y_categorical = TRUE) %>% 
    add_row_labels(tickvals = c(1,5), ticktext = c("A","B"))
  expect_iheatmap(test_plot, "row_labels_custom_ticktext_tickvals_horizontal")
})

test_that("get errors on bad tickvals and ticktext for continuous row labels",{
  expect_error(main_heatmap(a, y_categorical = TRUE) %>% 
                 add_row_labels(tickvals = c(1,5), ticktext = letters[1:3]))
})
