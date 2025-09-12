#  tests/testthat/test-sum_across.R
test_that("rb_sum_across sums columns correctly and removes originals", {
  df <- data.frame(
    id = 1:3,
    catch_cod = c(1, 2, 3),
    catch_had = c(0, 1, 0),
    catch_her = c(2, 1, 2)
  )
  summed <- rb_sum_across(df, start_name = "catch_")
  expect_true("catch_" %in% names(summed))
  expect_false(any(grepl("^catch_", names(summed)[names(summed) != "catch_"])))
  expect_equal(summed$catch_, c(3, 4, 5))
})

test_that("rb_sum_across works with remove = FALSE", {
  df <- data.frame(
    id = 1:2,
    catch_cod = c(1, 2),
    catch_had = c(0, 1)
  )
  summed <- rb_sum_across(df, start_name = "catch_", remove = FALSE)
  expect_true("catch_" %in% names(summed))
  expect_true(all(c("catch_cod", "catch_had") %in% names(summed)))
  expect_equal(summed$catch_, c(1, 3))
})

test_that("rb_sum_across errors if no matching columns", {
  df <- data.frame(id = 1:2, foo = c(1, 2))
  expect_error(rb_sum_across(df, start_name = "bar_"), "No columns found")
})

test_that("rb_sum_across errors if non-numeric columns are present", {
  df <- data.frame(
    id = 1:2,
    catch_cod = c(1, 2),
    catch_had = c("a", "b")
  )
  expect_error(rb_sum_across(df, start_name = "catch_"), "must be numeric")
})

test_that("rb_sum_across errors if start_name is not a single string", {
  df <- data.frame(id = 1:2, catch_cod = c(1, 2))
  expect_error(rb_sum_across(df, start_name = NULL), "must be a single character string")
  expect_error(rb_sum_across(df, start_name = c("catch_", "foo_")), "must be a single character string")
})