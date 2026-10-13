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

test_that("rb_cap_outliers(method = \"miller\") ignores a `limit` object on the search path", {
  # exists("limit") found a global `limit` and returned it as the cap: 0.5 0.5 0.5
  had <- exists("limit", envir = globalenv(), inherits = FALSE)
  if (had) {
    old <- get("limit", envir = globalenv())
  }
  assign("limit", 0.5, envir = globalenv())
  on.exit(
    if (had) {
      assign("limit", old, envir = globalenv())
    } else {
      rm("limit", envir = globalenv())
    }
  )
  expect_equal(rb_cap_outliers(c(1, 2, 3), method = "miller"), c(1, 2, 3))
  expect_equal(
    rb_cap_outliers(c(1, 2, 3, 1000), method = "miller"),
    c(1, 2, 3, 3)
  )
})

test_that("rb_cap_outliers caps by iqr and winsorize", {
  x <- c(rnorm(100, 10, 1), 1e6)
  expect_lt(max(rb_cap_outliers(x, method = "iqr")), 1e6)
  expect_equal(
    max(rb_cap_outliers(x, method = "winsorize", probs = 0.99)),
    as.numeric(quantile(x, 0.99))
  )
})

test_that("rb_cap_outliers(method = \"winsorize\") ignores NA when setting the limit", {
  # with na.rm = FALSE one NA made the limit NA and capped everything to NA
  x <- c(1:10, NA)
  out <- rb_cap_outliers(x, method = "winsorize", probs = 0.9)
  expect_equal(
    out[1:10],
    rb_cap_outliers(1:10, method = "winsorize", probs = 0.9)
  )
  expect_true(is.na(out[11]))
})
