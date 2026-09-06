#' Check the items
#'
#' This function creates a directives file for the CONFOR program to check
#' the items in a DELTA dataset.
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
#' \dontrun{
#' # Load a DELTA dataset
#' delta <- load_delta("chars_viola", "items_viola", "specs_viola")
#' 
#' # Check items only
#' checki(delta)
#' }
checki <- function(delta, remove_temp = TRUE) {
  
  # Validate input - consistent with other RDelta functions
  if (!inherits(delta, "Rcpp_DeltaParser") && !inherits(delta, "DeltaParser")) {
    stop("'delta' must be a DeltaParser object created by load_delta()")
  }
  
  if (!(delta$is_parsed() && delta$is_items_parsed())) {
    stop("Error: Delta dataset not properly parsed")
  }
  
  # Get file information
  items_file <- delta$get_items_filename()
  has_specs <- delta$has_specifications()
  specs_file <- if (has_specs) delta$get_specs_filename() else NULL
  
  # --- Create CONFOR directives file ---
  directives_file <- "checki.drt"
  con_in <- file(directives_file, "w")
  
  # 1. SHOW directive
  cat("*SHOW Check the items.\n", file = con_in)
  cat("\n", file = con_in)
  
  # 2. LISTING FILE directive
  cat("*LISTING FILE checki.lst\n", file = con_in)
  cat("\n", file = con_in)
  
  # 3. INPUT FILE specs (if available)
  if (!is.null(specs_file) && file.exists(specs_file)) {
    cat("*INPUT FILE ", specs_file, "\n", sep = "", file = con_in)
    cat("\n", file = con_in)
  }
  
  # 4. INPUT FILE items
  cat("*INPUT FILE ", items_file, "\n", sep = "", file = con_in)
  
  close(con_in)
  
  # Run CONFOR
  if (Sys.info()["sysname"] == "Windows") {
    result <- system(paste("confor", directives_file))
  } else {
    confor_cmd <- if (file.exists("./confor")) "./confor" else "confor"
    result <- system(paste(confor_cmd, directives_file))
  }
  
  # Clean up temporary files
  if (remove_temp) {
    if (file.exists(directives_file)) unlink(directives_file)
    if (file.exists("checki.lst")) unlink("checki.lst")
  }
  
  # Return 0 if successful, 1 if failed
  if (result != 0) {
    message("Check items failed!")
    return(1)
  } else {
    message("Check items completed successfully")
    return(0)
  }
}
