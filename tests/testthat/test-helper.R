context("helper")

test_that("convert matrix to data.frame", {
  mat <- matrix(1:10, ncol = 5)
  df <- matrix_to_df(mat)
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 10L)
  expect_equal(c("row", "column", "coefficient"), colnames(df))
  expect_true(is.integer(df$column))
  expect_true(is.integer(df$row))
})
