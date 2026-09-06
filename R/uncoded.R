#' Generate a list of uncoded characters using CONFOR
#'
#' This function creates a directives file for the CONFOR program to generate
#' a list of characters that are not coded (i.e., have no data) in a DELTA dataset.
#' This is useful for identifying missing data and gaps in taxonomic descriptions.
#'
#' @param delta A Delta dataset object (created by \code{\link{load_delta}})
#' @param heading Heading for the output (default = NULL, uses heading from specs file)
#' @param remove_temp Logical; if TRUE, remove temporary files after conversion (default = TRUE)
#'
#' @return Invisibly returns the path to the generated uncoded characters file
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
#' # Generate list of uncoded characters
#' uncoded(delta)
#' 
#' # With custom heading
#' uncoded(delta, heading = "Genre Viola - Missing data report")
#' }
uncoded <- function(delta, heading = NULL, remove_temp = TRUE) {
  
  # Validate input - consistent with other RDelta functions
  if (!inherits(delta, "Rcpp_DeltaParser") && !inherits(delta, "DeltaParser")) {
    stop("'delta' must be a DeltaParser object created by load_delta()")
  }
  
  if (!(delta$is_parsed() && delta$is_items_parsed())) {
    stop("Error: Delta dataset not properly parsed")
  }
  
  # Get file information
  chars_file <- delta$get_filename()
  items_file <- delta$get_items_filename()
  has_specs <- delta$has_specifications()
  specs_file <- if (has_specs) delta$get_specs_filename() else NULL
  
  # Get counts
  num_chars <- delta$get_chars_nb()
  num_items <- delta$get_items_nb()
  
  # Get heading
  heading_text <- "Uncoded characters report"
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
  
  # Create timestamp for the output
  timestamp <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
  
  # --- Create CONFOR directives file ---
  directives_file <- "uncoded.drt"
  con_in <- file(directives_file, "w")
  
  # 1. SHOW and HEADING directives
  cat("*SHOW Print uncoded characters\n", file = con_in)
  cat("*SHOW Generated on ", timestamp, "\n", sep = "", file = con_in)
  cat("*HEADING ", heading_text, "\n", sep = "", file = con_in)
  cat("*PRINT FILE uncoded.txt\n", file = con_in)
  cat("\n", file = con_in)
  
  # 2. INPUT FILE specs (if available)
  if (!is.null(specs_file) && file.exists(specs_file)) {
    cat("*INPUT FILE ", specs_file, "\n", sep = "", file = con_in)
    cat("\n", file = con_in)
  }
  
  # 3. PRINT UNCODED CHARACTERS directive
  cat("*PRINT UNCODED CHARACTERS\n", file = con_in)
  cat("\n", file = con_in)
  
  # 4. INPUT FILE chars
  cat("*INPUT FILE ", chars_file, "\n", sep = "", file = con_in)
  cat("\n", file = con_in)
  
  # 5. INPUT FILE items
  cat("*INPUT FILE ", items_file, "\n", sep = "", file = con_in)
  
  close(con_in)
  
  # Run CONFOR
  if (Sys.info()["sysname"] == "Windows") {
    result <- system(paste("confor", directives_file))
  } else {
    confor_cmd <- if (file.exists("./confor")) "./confor" else "confor"
    result <- system(paste(confor_cmd, directives_file))
  }
  
  if (result != 0) {
    stop("Error: CONFOR execution failed!")
  }
  
  # Check if output file was created
  output_file <- "uncoded.txt"
  if (!file.exists(output_file)) {
    stop("Error: Uncoded characters file (uncoded.txt) not created!")
  }
  
  # Read the generated output
  uncoded_text <- readLines(output_file)
  
  # Clean up temporary files
  if (remove_temp) {
    if (file.exists(directives_file)) unlink(directives_file)
    if (file.exists(paste0(directives_file, ".lst"))) unlink(paste0(directives_file, ".lst"))
    # Keep uncoded.txt as the output file
  }
  
  message("Uncoded characters report generated successfully")
  message("  - Number of items: ", num_items)
  message("  - Number of characters: ", num_chars)
  message("  - Output file: uncoded.txt")
  
  invisible(output_file)
}