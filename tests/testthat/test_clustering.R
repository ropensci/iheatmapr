context("clustering")

a <- matrix(rnorm(200),ncol=10)
col_grp <- c(rep("A",5),rep("B",5))
row_grp <- c(rep("A",15),rep("B",5))

# Col clustering with default option (hclust) ---------------------------------

test_that("can add row clustering with dendrogram to single horizontal heatmap",
          {
  test_plot <- main_heatmap(a) %>% add_row_clustering()
  expect_iheatmap(test_plot, "row_clustering_hclust_horizontal")
})

test_that("can add row clustering with dendrogram single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% add_row_clustering()
  expect_iheatmap(test_plot, "row_clustering_vertical", "vertical")
})

test_that("can add col clustering with dendrogram to single horizontal heatmap",
          {
  test_plot <- main_heatmap(a) %>% add_col_clustering()
  expect_iheatmap(test_plot, "col_clustering_hclust_horizontal")
})

test_that("can add col clustering with dendrogram  to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% add_col_clustering()
  expect_iheatmap(test_plot, "col_clustering_hclust_vertical", "vertical")
})


# Col clustering with default option (hclust) plus k ---------------------------

test_that("can add row clustering with dendrogram and k to single horizontal 
          heatmap",{
            test_plot <- main_heatmap(a) %>% add_row_clustering(k = 3)
            expect_iheatmap(test_plot, "row_clustering_hclust_k_horizontal")
          })

test_that("can add row clustering with dendrogram and k to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_row_clustering(k = 3)
  expect_iheatmap(test_plot, "row_clustering_k_vertical", "vertical")
})

test_that("can add col clustering with dendrogram and k to single horizontal heatmap",
          { test_plot <- main_heatmap(a) %>% add_col_clustering(k = 3)
          expect_iheatmap(test_plot, "col_clustering_hclust_k_horizontal")
          })

test_that("can add col clustering with dendrogram and k to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_col_clustering(k = 3)
  expect_iheatmap(test_plot, "col_clustering_hclust_k_vertical", "vertical")
})

# Col clustering with kmeans  --------------------------------------------------

test_that("can add row clustering with kmeans to single horizontal 
          heatmap",
          { test_plot <- main_heatmap(a) %>% 
            add_row_clustering(method = "kmeans",
                               k = 3)
          expect_iheatmap(test_plot, "row_clustering_kmeans_horizontal")
          })

test_that("can add row clustering with kmeans to single vertical 
          heatmap",
          {
            test_plot <- main_heatmap(a, orientation = "vertical") %>% 
              add_row_clustering(method = "kmeans", k = 3)
            expect_iheatmap(test_plot, "row_clustering_kmeans_vertical",
                            "vertical")
          })

test_that("can add col clustering with kmeans to single horizontal heatmap",
          { test_plot <- main_heatmap(a) %>% 
            add_col_clustering(method = "kmeans",k = 3)
          expect_iheatmap(test_plot, "col_clustering_kmeans_horizontal")
          })

test_that("can add col clustering with kmeans to single vertical heatmap",{
  test_plot <- main_heatmap(a, orientation = "vertical") %>% 
    add_col_clustering(method = "kmeans", k = 3)
  expect_iheatmap(test_plot, "col_clustering_kmeans_vertical", "vertical")
})


