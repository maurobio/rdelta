#' RDelta: Parse and Query DELTA-Format Taxonomic Descriptions
#'
#' Provides an R interface to the tDelta C++ library for parsing
#' DELTA (DEscription Language for TAxonomy) format files.
#'
#' @docType package
#' @name RDelta-package
#' @aliases RDelta
#' @return No return value; this is a package-level documentation object.
#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  tryCatch({
    Rcpp::loadModule("delta_module", TRUE)
    invisible()
  }, error = function(e) {
    warning("Could not load DeltaParser module: ", e$message)
  })
}