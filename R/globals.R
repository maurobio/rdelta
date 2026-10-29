# R/globals.R - Internal module wrappers

#' Internal wrapper for module parse_delta_attribute
#' @keywords internal
#' @noRd
.parse_delta_attribute <- function(x) {
  delta_module <- get("delta_module", envir = .GlobalEnv)
  delta_module$parse_delta_attribute(x)
}

#' Internal wrapper for module strip_delta_comments
#' @keywords internal
#' @noRd
.strip_delta_comments <- function(x) {
  delta_module <- get("delta_module", envir = .GlobalEnv)
  delta_module$strip_delta_comments(x)
}