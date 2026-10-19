# a missing column names the step that adds it; data frame and lazy table alike

    Code
      .rb_check_input(p[, "vid"], "rb_add_effort")
    Condition
      Error:
      ! `pings` must have a column `time`.
      x `time` is missing.
      i It is an input column: supply it, or rename yours to it.

---

    Code
      rb_add_effort(p[, "vid"])
    Condition
      Error in `rb_add_effort()`:
      ! `pings` must have a column `time`.
      x `time` is missing.
      i Name the column that holds it with the `time` argument.

---

    Code
      .rb_check_input(p, "rb_add_effort", also = c(fishing = "fishing"))
    Condition
      Error:
      ! `pings` must have a column `fishing`.
      x `fishing` is missing.
      i It is added by `rb_add_fishing()`.

# types are checked on a lazy table's schema; a step's own columns mean it has run

    Code
      .rb_check_input(p, "rb_add_effort", types = c(time = "time"))
    Condition
      Error:
      ! `time` must be a date-time (POSIXct).
      x It is character.

