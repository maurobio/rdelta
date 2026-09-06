#' DeltaParser: Create a DELTA Parser Object
#'
#' Creates a DeltaParser object for parsing DELTA files.
#'
#' @param file Path to the DELTA file to parse.
#'
#' @return An object of class \code{Rcpp_DeltaParser}.
#'
#' @keywords internal
DeltaParser <- function(file) {
  delta_module <- get("delta_module", envir = .GlobalEnv)
  delta_module$DeltaParser(file)
}