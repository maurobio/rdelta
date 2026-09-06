# R/globals.R
#' Parse DELTA Attribute
#'
#' Internal function to parse DELTA attributes from the C++ module.
#' @param x Character string to parse
#' @keywords internal
parse_delta_attribute <- function(x) {
  delta_module <- get("delta_module", envir = .GlobalEnv)
  delta_module$parse_delta_attribute(x)
}

#' Strip DELTA Comments
#'
#' Internal function to strip comments from DELTA format.
#' @param x Character string to strip comments from
#' @keywords internal
strip_delta_comments <- function(x) {
  delta_module <- get("delta_module", envir = .GlobalEnv)
  delta_module$strip_delta_comments(x)
}