# a missing column names the step that adds it; data frame and lazy table alike

    Code
      .rb_check_input(p[, "vid"], "rb_assign_effort")
    Condition
      Error:
      ! `pings` must have a column `time`.
      x `time` is missing.
      i It is an input column: supply it, or rename yours to it.

---

    Code
      rb_assign_effort(p[, "vid"])
    Condition
      Error in `rb_assign_effort()`:
      ! `pings` must have a column `time`.
      x `time` is missing.
      i Name the column that holds it with the `time` argument.

---

    Code
      .rb_check_input(p, "rb_assign_effort", also = c(fishing = "fishing"))
    Condition
      Error:
      ! `pings` must have a column `fishing`.
      x `fishing` is missing.
      i It is added by `rb_assign_fishing()`.

# types are checked on a lazy table's schema; a step's own columns mean it has run

    Code
      .rb_check_input(p, "rb_assign_effort", types = c(time = "time"))
    Condition
      Error:
      ! `time` must be a date-time (POSIXct).
      x It is character.

