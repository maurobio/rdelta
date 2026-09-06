#' Get implicit value for a character
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @param iv_type Implicit value type (1 or 2, default: 1)
#' @return Integer value or NA if not defined
#' @examples
#' \dontrun{
#' delta <- load_delta("chars", "items", "specs")
#' get_implicit_value(delta, 1)
#' }
#' @export
get_implicit_value <- function(delta, charnum, iv_type = 1) {
  delta$get_implicit_value(charnum, iv_type)
}

#' Get all implicit values
#'
#' @param delta A DeltaParser object
#' @param iv_type Implicit value type (1 or 2, default: 1)
#' @return Integer vector of implicit values
#' @examples
#' \dontrun{
#' delta <- load_delta("chars", "items", "specs")
#' get_all_implicit_values(delta)
#' }
#' @export
get_all_implicit_values <- function(delta, iv_type = 1) {
  delta$get_all_implicit_values(iv_type)
}

#' Get number of dependent characters
#'
#' @param delta A DeltaParser object
#' @param ccnum Control character number
#' @param ccstate Control character state
#' @return Number of dependent characters
#' @examples
#' \dontrun{
#' delta <- load_delta("chars", "items", "specs")
#' get_depchar_nb(delta, 13, 2)
#' }
#' @export
get_depchar_nb <- function(delta, ccnum, ccstate) {
  delta$get_depchar_nb(ccnum, ccstate)
}

#' Get dependent character at specific rank
#'
#' @param delta A DeltaParser object
#' @param ccnum Control character number
#' @param ccstate Control character state
#' @param rank Rank position (default: 1)
#' @return Dependent character number or 0 if none
#' @examples
#' \dontrun{
#' delta <- load_delta("chars", "items", "specs")
#' get_depchar(delta, 13, 2, 1)
#' }
#' @export
get_depchar <- function(delta, ccnum, ccstate, rank = 1) {
  delta$get_depchar(ccnum, ccstate, rank)
}

#' Get all dependent characters
#'
#' @param delta A DeltaParser object
#' @param ccnum Control character number
#' @param ccstate Control character state
#' @return Integer vector of dependent character numbers
#' @examples
#' \dontrun{
#' delta <- load_delta("chars", "items", "specs")
#' get_all_depchar(delta, 13, 2)
#' }
#' @export
get_all_depchar <- function(delta, ccnum, ccstate) {
  delta$get_all_depchar(ccnum, ccstate)
}

#' Check if a character is dependent
#'
#' @param delta A DeltaParser object
#' @param dcnum Dependent character number
#' @param ccnum Control character number
#' @param ccstate Control character state
#' @return Logical indicating if dcnum is dependent
#' @examples
#' \dontrun{
#' delta <- load_delta("chars", "items", "specs")
#' is_dependent(delta, 14, 13, 2)
#' }
#' @export
is_dependent <- function(delta, dcnum, ccnum, ccstate) {
  delta$is_dependent(dcnum, ccnum, ccstate)
}

#' Get all dependency relationships
#'
#' @param delta A DeltaParser object
#' @return List of all dependency relationships
#' @examples
#' \dontrun{
#' delta <- load_delta("chars", "items", "specs")
#' get_all_dependencies(delta)
#' }
#' @export
get_all_dependencies <- function(delta) {
  delta$get_all_dependencies()
}