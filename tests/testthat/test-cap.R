test_that("rb_cap_outliers(method = \"miller\") caps above the log-step limit", {
  expect_equal(
    rb_cap_outliers(c(1, 2, 3, 1000), method = "miller"),
    c(1, 2, 3, 3)
  )
})

test_that("rb_cap_outliers(method = \"miller\") leaves an all-zero vector unchanged", {
  # the lmit typo returned all -Inf here
  expect_equal(rb_cap_outliers(c(0, 0, 0), method = "miller"), c(0, 0, 0))
})

test_that("rb_cap_outliers caps by iqr and winsorize", {
  x <- c(rnorm(100, 10, 1), 1e6)
  expect_lt(max(rb_cap_outliers(x, method = "iqr")), 1e6)
  expect_equal(
    max(rb_cap_outliers(x, method = "winsorize", probs = 0.99)),
    as.numeric(quantile(x, 0.99))
  )
})
