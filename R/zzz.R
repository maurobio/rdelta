
.onLoad <- function(libname, pkgname) {
  # Load the Rcpp module using Rcpp::loadModule
  tryCatch({
    Rcpp::loadModule("delta_module", TRUE)
    invisible()
  }, error = function(e) {
    warning("Could not load DeltaParser module: ", e$message)
  })
}

