#' Each ping's outcome under a rule, against the truth
#'
#' For scoring a flagger on a planted set such as `dansk_truth`: every ping in one outcome (fishycode plan 062 step 3,
#' plan 065), so the tests, the scores and the viewer ([rb_map_time()]'s lanes) read the same thing:
#'
#' * a wrong ping: `"caught"` where the rule flagged it as the truth expects; `"caught, as position"`, `"caught, as
#'   time"` or `"caught, as copy"` where it flagged it as another kind; `"missed"` where it kept it;
#' * a good ping: `"kept"`; where flagged, `"flagged"`, the rule's own false flag, or `"flagged, beside a fault"`, a
#'   flag the wrong pings caused. The two are told apart by `clean`, the same rule's labels on the tracks without their
#'   wrong pings: a good ping flagged there too is the rule's own. Without `clean`, every good ping flagged is
#'   `"flagged"`.
#'
#' A label's kind: `"time"` a time ([rb_flag_ping_time()]); `"duplicate"` and `"shifted"` a copy
#' ([rb_flag_ping_duplicate()]); any other a position. The truth's: `"flag position"`, `"flag time"` and `"copy"` (as
#' `dansk_truth`'s `expect`); a logical truth has none, and any flag of a wrong ping is `"caught"`.
#'
#' @param x A data frame or a lazy DuckDB table.
#' @param label The rule's label column (`NA`: kept), as `ping_flag`.
#' @param truth The truth column: `"keep"` for a good ping and what an ideal flagger does with a wrong one (`"flag
#'   position"`, `"flag time"`, `"copy"`), or logical, `TRUE` for a wrong ping.
#' @param clean Optional: the column of the rule's labels on a run without the wrong pings (`NA` on the wrong pings).
#' @param name The outcome column. Default: the label's name with `_outcome`.
#'
#' @return `x` with the outcome column added: the same type, the same rows.
#' @family flag pings
#' @export
#' @examples
#' d <- data.frame(
#'   expect = c("keep", "keep", "flag position", "flag time", "keep"),
#'   ping_flag = c(NA, "forward", "local_spike", "local_forward", NA)
#' )
#' rb_add_ping_outcome(d, ping_flag, expect)
rb_add_ping_outcome <- function(x, label, truth, clean = NULL, name = NULL) {
  label_nm <- rlang::as_name(rlang::enquo(label))
  truth_nm <- rlang::as_name(rlang::enquo(truth))
  clean_quo <- rlang::enquo(clean)
  has_clean <- !rlang::quo_is_null(clean_quo)
  clean_nm <- if (has_clean) rlang::as_name(clean_quo)
  if (is.null(name)) {
    name <- paste0(label_nm, "_outcome")
  }
  if (!is.character(name) || length(name) != 1 || is.na(name) || !nzchar(name)) {
    stop("`name` must be one column name.", call. = FALSE)
  }
  miss <- setdiff(c(label_nm, truth_nm, clean_nm), colnames(x))
  if (length(miss)) {
    stop(sprintf("`x` lacks %s.", paste0("`", miss, "`", collapse = ", ")), call. = FALSE)
  }
  lab <- rlang::sym(label_nm)
  tru <- rlang::sym(truth_nm)
  is_lgl <- if (inherits(x, "tbl_lazy")) {
    is.logical(dplyr::pull(utils::head(dplyr::select(x, !!tru), 1)))
  } else {
    is.logical(x[[truth_nm]])
  }
  kind <- rlang::expr(dplyr::case_when(
    is.na(!!lab) ~ NA_character_,
    !!lab == "time" ~ "time",
    !!lab %in% c("duplicate", "shifted") ~ "copy",
    TRUE ~ "position"
  ))
  wrong <- if (is_lgl) rlang::expr(!!tru) else rlang::expr(!!tru != "keep")
  want <- if (is_lgl) {
    rlang::expr(NA_character_)
  } else {
    rlang::expr(dplyr::case_when(
      !!tru == "flag position" ~ "position",
      !!tru == "flag time" ~ "time",
      !!tru == "copy" ~ "copy",
      TRUE ~ NA_character_
    ))
  }
  own <- if (has_clean) rlang::expr(!is.na(!!rlang::sym(clean_nm))) else TRUE
  x |>
    dplyr::mutate(
      !!name := dplyr::case_when(
        is.na(!!tru) ~ NA_character_,
        !!wrong & !is.na(!!lab) & (is.na(!!want) | !!want == !!kind) ~ "caught",
        !!wrong & !is.na(!!lab) ~ paste0("caught, as ", !!kind),
        !!wrong ~ "missed",
        is.na(!!lab) ~ "kept",
        !!own ~ "flagged",
        TRUE ~ "flagged, beside a fault"
      )
    )
}
