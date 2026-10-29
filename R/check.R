#' Check the characters and items
#'
#' This function creates a directives file for the CONFOR program to check
#' the characters and items in a DELTA dataset.
#'
#' @param delta A Delta dataset object (created by \code{\link{load_delta}})
#' @param remove_temp Logical; if TRUE, remove temporary files after conversion (default = TRUE)
#'
#' @return Integer: 0 if check is successful, 1 if check fails
#' @export
#'
#' @details The function assumes that the CONFOR program executable is available
#'          in the system PATH or in the current working directory.
#'
#' @examples
#' # Requires the CONFOR executable on the PATH.
#' \donttest{
#' try({
#'   delta <- load_delta(
#'     system.file("extdata", "chars_viola", package = "RDelta"),
#'     system.file("extdata", "items_viola", package = "RDelta"),
#'     system.file("extdata", "specs_viola", package = "RDelta")
#'   )
#'   check(delta)
#' })
#' }
check <- function(delta, remove_temp = TRUE) {

  if (!inherits(delta, "Rcpp_DeltaParser") && !inherits(delta, "DeltaParser")) {
    stop("'delta' must be a DeltaParser object created by load_delta()")
  }

  if (!(delta$is_parsed() && delta$is_items_parsed())) {
    stop("Error: Delta dataset not properly parsed")
  }

  chars_file <- delta$get_filename()
  items_file <- delta$get_items_filename()
  has_specs <- delta$has_specifications()
  specs_file <- if (has_specs) delta$get_specs_filename() else NULL

  tmp_dir <- file.path(tempdir(), paste0("RDelta_check_", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  old_wd <- setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  directives_file <- "check.drt"
  con_in <- file(directives_file, "w")

  cat("*SHOW Check the characters and items.\n", file = con_in)
  cat("\n", file = con_in)
  cat("*LISTING FILE check.lst\n", file = con_in)
  cat("\n", file = con_in)

  if (!is.null(specs_file) && file.exists(specs_file)) {
    cat('*INPUT FILE "', specs_file, '"\n', sep = "", file = con_in)
    cat("\n", file = con_in)
  }

  cat('*INPUT FILE "', chars_file, '"\n', sep = "", file = con_in)
  cat("\n", file = con_in)
  cat('*INPUT FILE "', items_file, '"\n', sep = "", file = con_in)

  close(con_in)

  if (Sys.info()["sysname"] == "Windows") {
    result <- system(paste("confor", directives_file))
  } else {
    confor_cmd <- if (file.exists("./confor")) "./confor" else "confor"
    result <- system(paste(confor_cmd, directives_file))
  }

  if (remove_temp) {
    if (file.exists(directives_file)) unlink(directives_file)
    if (file.exists("check.lst")) unlink("check.lst")
  }

  if (result != 0) {
    warning("CONFOR reported an error during the check; see the listing file.")
    return(1)
  } else {
    return(0)
  }
}