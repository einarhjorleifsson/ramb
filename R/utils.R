# Check that it doesn't match any non-letter
letters_only <- function(x) !grepl("[^A-Za-z]", x)
# Check that it doesn't match any non-number
numbers_only <- function(x) !grepl("\\D", x)

str_extract_between_parenthesis <- function(x) {
  stringr::str_match(x, "(?<=\\().+?(?=\\))")
}

str_extract_dmy_period <-
  function(x) {
    stringr::str_extract(x, "\\d+\\D+\\d+\\D+\\d+")
  }