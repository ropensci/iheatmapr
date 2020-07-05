context("list_utils")

test_that("can subset a list using [ and integer index",{
  a <- new("IheatmapAxis",
      id = "axis-1",
      domain_start = 0,
      domain_end = 0.45,
      anchor = "y",
      layout = list())
  b <- new("IheatmapAxis",
          id = "axis-3",
          domain_start = 0.5,
          domain_end = 1,
          anchor = "y",
          layout = list())
  l <- new("IheatmapAxes", listData = c(a,b), axis = "x")
  expect_equal(l[1], new("IheatmapAxes", listData = c(a), axis = 'x'))
})

test_that("can subset a list using [ and name",{
  a <- new("IheatmapAxis",
           id = "axis-1",
           domain_start = 0,
           domain_end = 0.45,
           anchor = "y",
           layout = list())
  b <- new("IheatmapAxis",
           id = "axis-3",
           domain_start = 0.5,
           domain_end = 1,
           anchor = "y",
           layout = list())
  l <- new("IheatmapAxes", listData = c('a' = a,'b' = b), axis = "x")
  expect_equal(l['a'], new("IheatmapAxes", listData = list('a' = a), axis = 'x'))
})

test_that("can subset a list using [[] and integer index",{
  a <- new("IheatmapAxis",
           id = "axis-1",
           domain_start = 0,
           domain_end = 0.45,
           anchor = "y",
           layout = list())
  b <- new("IheatmapAxis",
           id = "axis-3",
           domain_start = 0.5,
           domain_end = 1,
           anchor = "y",
           layout = list())
  l <- new("IheatmapAxes", listData = c(a,b), axis = "x")
  expect_equal(l[[1]], a)
})

test_that("can subset a list using [[ and name",{
  a <- new("IheatmapAxis",
           id = "axis-1",
           domain_start = 0,
           domain_end = 0.45,
           anchor = "y",
           layout = list())
  b <- new("IheatmapAxis",
           id = "axis-3",
           domain_start = 0.5,
           domain_end = 1,
           anchor = "y",
           layout = list())
  l <- new("IheatmapAxes", listData = c('a' = a,'b' = b), axis = "x")
  expect_equal(l[['a']], a)
})

test_that("can get names of list",{
  a <- new("IheatmapAxis",
           id = "axis-1",
           domain_start = 0,
           domain_end = 0.45,
           anchor = "y",
           layout = list())
  b <- new("IheatmapAxis",
           id = "axis-3",
           domain_start = 0.5,
           domain_end = 1,
           anchor = "y",
           layout = list())
  l <- new("IheatmapAxes", listData = c('a' = a,'b' = b), axis = "x")
  expect_equal(names(l), c('a','b'))
})

test_that("can get length of list",{
  a <- new("IheatmapAxis",
           id = "axis-1",
           domain_start = 0,
           domain_end = 0.45,
           anchor = "y",
           layout = list())
  b <- new("IheatmapAxis",
           id = "axis-3",
           domain_start = 0.5,
           domain_end = 1,
           anchor = "y",
           layout = list())
  l <- new("IheatmapAxes", listData = c('a' = a,'b' = b), axis = "x")
  expect_equal(length(l), 2)
})



