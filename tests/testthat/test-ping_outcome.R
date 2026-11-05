test_that("every ping in one outcome: caught, of another kind, missed, kept, flagged", {
  d <- data.frame(
    expect = c("keep", "keep", "flag position", "flag time", "flag time", "copy", "copy", "keep"),
    ping_flag = c(NA, "forward", "local_spike", "time", "local_forward", "duplicate", NA, "time")
  )
  o <- rb_add_ping_outcome(d, ping_flag, expect)
  expect_equal(
    o$ping_flag_outcome,
    c("kept", "flagged", "caught", "caught", "caught, as position", "caught", "missed", "flagged")
  )
  expect_equal(names(o), c(names(d), "ping_flag_outcome"))
})

test_that("a run without the wrong pings tells the rule's own flags from those beside a fault", {
  d <- data.frame(
    expect = c("keep", "keep", "flag position", "keep"),
    rule = c("forward", "forward", "forward", NA),
    rule_clean = c(NA, "forward", NA, NA)
  )
  o <- rb_add_ping_outcome(d, rule, expect, clean = rule_clean, name = "outcome")
  expect_equal(o$outcome, c("flagged, beside a fault", "flagged", "caught", "kept"))
})

test_that("a logical truth has no kinds", {
  d <- data.frame(wrong = c(TRUE, TRUE, FALSE, NA), ping_flag = c("time", NA, NA, "spike"))
  o <- rb_add_ping_outcome(d, ping_flag, wrong)
  expect_equal(o$ping_flag_outcome, c("caught", "missed", "kept", NA))
})

test_that("a lazy table gives the same outcomes, still lazy", {
  d <- data.frame(expect = c("keep", "flag time", "keep"), ping_flag = c(NA, "local_forward", "spike"))
  lz <- rb_add_ping_outcome(as_lazy(d), ping_flag, expect)
  expect_s3_class(lz, "tbl_lazy")
  expect_equal(dplyr::collect(lz)$ping_flag_outcome, rb_add_ping_outcome(d, ping_flag, expect)$ping_flag_outcome)
})

test_that("the columns are checked", {
  d <- data.frame(expect = "keep", ping_flag = NA_character_)
  expect_error(rb_add_ping_outcome(d, flag, expect), "flag")
  expect_error(rb_add_ping_outcome(d, ping_flag, expect, name = c("a", "b")), "name")
})
