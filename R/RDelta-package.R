#' RDelta: Parse and Query DELTA-Format Taxonomic Descriptions
#'
#' Provides an R interface to the tDelta C++ library for parsing
#' DELTA (DEscription Language for TAxonomy) format files.
#'
#' @docType package
#' @name RDelta-package
#' @aliases RDelta
#' @keywords internal
"_PACKAGE"

# REMOVE this section or comment it out:
# @section Important Classes:
# \describe{
#   \item{\code{\linkS4class{Rcpp_DeltaParser}}}{Main parser class for DELTA files}
# }

.onLoad <- function(libname, pkgname) {
  tryCatch({
    Rcpp::loadModule("delta_module", TRUE)
    invisible()
  }, error = function(e) {
    warning("Could not load DeltaParser module: ", e$message)
  })
}