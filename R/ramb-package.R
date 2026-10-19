# The rename plan behind the naming rule below is fishycode's curate/plans/013-ramb-one-flow.md, and the verbs by
# output shape are its plan 032 (2026-10-19); the pre-2026-10-08 names were removed on 2026-10-11.

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib ramb, .registration = TRUE
## usethis namespace: end
#' @section Naming:
#' Every function is named verb first, and the verb says what it returns, after tidyverse's \code{count()} and
#' \code{add_count()}: \code{rb_create_*()} a table of its own, \code{rb_add_*()} the input with columns added,
#' \code{rb_flag_*()} the input with a flag column, \code{rb_link_*()} a table of pairs.
#'
#' A function of the fishing-activity flow (the WGSFD 2026 one-flow document) is
#' \code{rb_<verb>_<stage>[_<detail>]}, the stage one of vessel, ping, trip, record, gear, fishing, effort, catch;
#' step 1 names the objects it makes or adds instead, voyages and port calls. For example
#' \code{rb_create_port_calls()} and \code{rb_create_voyages()} are step-1 builders, \code{rb_add_voyages()} is
#' step 1, \code{rb_add_fishing()} step 4. A tool that serves every step is \code{rb_<verb>_<object>}.
#'
#' Each verb has one meaning: create (a table of its own), add (the input with columns added; in a step, a value
#' from the best source, with a basis), flag (label rows, drop none), link (a crosswalk, ambiguity kept), share
#' (distribute a quantity, conserving the total), limit (judge a value against a rule, raw kept), number, detect
#' (a logical vector), calc (a numeric vector), encode, extract, convert, bin, and the plain English verbs
#' (interpolate, fill, predict, classify, lookup, cap, standardise, sum, check, get, read, plot, register). Two of
#' these pair up: \code{detect} returns a logical vector, \code{check} inspects a table and warns.
#'
#' A method is never part of a name; it is the \code{method} argument. A grouping is never part of
#' a method; it is the \code{by} argument.
#' @keywords internal
"_PACKAGE"
