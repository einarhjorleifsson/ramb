# The rename plan behind the naming rule below is fishycode's curate/plans/013-ramb-one-flow.md;
# the pre-2026-10-08 names were removed on 2026-10-11.

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib ramb, .registration = TRUE
## usethis namespace: end
#' @section Naming:
#' Every function is named verb first: \code{rb_<verb>_<stage>[_<detail>]} for a function that
#' belongs to a step of the fishing-activity flow (the WGSFD 2026 one-flow document), and
#' \code{rb_<verb>_<object>} for a tool that serves every step. The stage word says where on the
#' flow a function sits: vessel, ping, trip, record, gear, fishing, effort, catch. For example
#' \code{rb_assign_trip()} is step 1, \code{rb_find_trip_stays()} a step-1 builder.
#'
#' Each verb has one meaning: flag (label rows, drop none), assign (a step: a value from the best
#' source, with a basis), share (distribute a quantity, conserving the total), find (intervals from
#' a track), cut (segments of a track), build (a rule or window table), link (a crosswalk, ambiguity
#' kept), limit (judge a value against a rule, raw kept), number, detect (a logical vector), calc (a
#' numeric vector), encode, extract, convert, bin, and the plain English verbs (interpolate, fill,
#' predict, classify, lookup, cap, standardise, sum, check, get, read, plot, register). Two of these
#' pair up: \code{detect} returns a logical vector, \code{check} inspects a table and warns.
#'
#' A method is never part of a name; it is the \code{method} argument. A grouping is never part of
#' a method; it is the \code{by} argument.
#' @keywords internal
"_PACKAGE"
