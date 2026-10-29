#' Generate a statistical summary of taxonomic characters using CONFOR
#'
#' This function creates a directives file for the CONFOR program to generate
#' a statistical summary of characters for a DELTA dataset. The summary includes
#' character types, states, and frequency distributions.
#'
#' @param delta A Delta dataset object (created by \code{\link{load_delta}})
#' @param heading Heading for the summary (default = NULL, uses heading from specs file)
#' @param remove_temp Logical; if TRUE, remove temporary files after conversion (default = TRUE)
#'
#' @return Invisibly returns the path to the generated summary file
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
#'   summary(delta)
#'   summary(delta, heading = "Genre Viola")
#' })
#' }
summary <- function(delta, heading = NULL, remove_temp = TRUE) {

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

  heading_text <- "Summary of DELTA dataset"
  if (!is.null(heading)) {
    heading_text <- heading
  } else if (!is.null(specs_file) && file.exists(specs_file)) {
    specs_lines <- readLines(specs_file, n = 20)
    for (line in specs_lines) {
      if (grepl("^\\*HEADING", line)) {
        heading_text <- sub("^\\*HEADING\\s+", "", line)
        break
      }
    }
  }

  timestamp <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")

  tmp_dir <- file.path(tempdir(), paste0("RDelta_summary_", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  old_wd <- setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  directives_file <- "summary.drt"
  con_in <- file(directives_file, "w")

  cat("*SHOW Print summary\n", file = con_in)
  cat("*SHOW Generated on ", timestamp, "\n", sep = "", file = con_in)
  cat("*HEADING ", heading_text, "\n", sep = "", file = con_in)
  cat("*PRINT FILE summary.txt\n", file = con_in)
  cat("\n", file = con_in)

  if (!is.null(specs_file) && file.exists(specs_file)) {
    cat('*INPUT FILE "', specs_file, '"\n', sep = "", file = con_in)
    cat("\n", file = con_in)
  }

  cat("*PRINT SUMMARY\n", file = con_in)
  cat("\n", file = con_in)

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

  if (result != 0) {
    stop("Error: CONFOR execution failed!")
  }

  output_file <- "summary.txt"
  if (!file.exists(output_file)) {
    stop("Error: Summary file (summary.txt) not created!")
  }

  out_path <- file.path(old_wd, "summary.txt")
  file.copy(output_file, out_path, overwrite = TRUE)

  if (remove_temp) {
    if (file.exists(directives_file)) unlink(directives_file)
    if (file.exists(paste0(directives_file, ".lst"))) unlink(paste0(directives_file, ".lst"))
  }

  invisible(out_path)
}