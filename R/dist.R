#' Generate Distance Matrix using Gower's Coefficient
#' 
#' This function replicates the DIST command in the DELTA system for generating
#' a distance matrix using Gower's coefficient. It creates the necessary
#' directives files and runs the CONFOR and DIST programs.
#'
#' @param delta A Delta dataset object (created by \code{\link{load_delta}})
#' @param heading Heading for the output (default = NULL, uses heading from specs file)
#' @param match_overlap For unordered multistate characters, the contribution 
#'   of a character to the distance between two items is 0 if the items have 
#'   any of the values of the character in common (default = FALSE)
#' @param minimum_comparisons Minimum number of character comparisons required 
#'   to calculate the distance between two items (default = ceiling(sqrt(number 
#'   of included characters)))
#' @param phylip_format Causes the taxon names to be interleaved with the rows 
#'   of the distance matrix, as required by the program Phylip (default = FALSE)
#' @param exclude_items A list of items to be excluded (default = NULL)
#' @param exclude_characters A list of characters to be excluded (default = NULL)
#' @param remove_temp Logical; if TRUE, remove temporary files after conversion
#'
#' @return Invisibly returns 0 on success
#' @export
#'
#' @details The function assumes that the CONFOR and DIST program executables are available 
#'          in the system PATH or in the current working directory.
#'
#' @examples
#' \dontrun{
#' # Load a DELTA dataset
#' delta <- load_delta("chars", "items", "specs")
#' 
#' # Generate distance matrix
#' dist(delta)
#' 
#' # With custom options
#' dist(delta, match_overlap = TRUE, phylip_format = TRUE, 
#'      minimum_comparisons = 10)
#' }
dist <- function(delta, 
                 heading = NULL,
                 match_overlap = FALSE, 
                 minimum_comparisons, 
                 phylip_format = FALSE, 
                 exclude_items = NULL, 
                 exclude_characters = NULL,
                 remove_temp = TRUE) {
  
  # Validate input
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
  
  # Set default minimum_comparisons if not provided
  if (missing(minimum_comparisons)) {
    n_chars <- num_chars
    if (!is.null(exclude_characters)) {
      n_chars <- n_chars - length(exclude_characters)
    }
    minimum_comparisons <- ceiling(sqrt(n_chars))
  } else {
    minimum_comparisons <- ceiling(minimum_comparisons)
  }
  
  # Get heading
  heading_text <- "DELTA Dataset"
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
  
  # --- FIRST STAGE: Create CONFOR directives to generate DIST binary file ---
  con_file <- "todis"
  con_in <- file(con_file, "w")
  
  cat("*SHOW Translate into DIST format\n", file = con_in)
  cat("*SHOW Generated on ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "\n\n", sep = "", file = con_in)
  cat("*HEADING ", heading_text, "\n\n", sep = "", file = con_in)
  
  # Include specifications file if it exists
  if (!is.null(specs_file) && file.exists(specs_file)) {
    cat("*INPUT FILE ", specs_file, "\n\n", sep = "", file = con_in)
  }
  
  cat("*OMIT TYPESETTING MARKS\n", file = con_in)
  
  # Add MATCH OVERLAP if TRUE
  if (match_overlap) {
    cat("*MATCH OVERLAP\n", file = con_in)
  }
  
  # Add EXCLUDE ITEMS if specified
  if (!is.null(exclude_items) && length(exclude_items) > 0) {
    item_exclusions <- character()
    for (item in exclude_items) {
      if (is.character(item)) {
        for (i in 1:num_items) {
          if (delta$get_item_name(i) == item) {
            item_exclusions <- c(item_exclusions, as.character(i))
            break
          }
        }
      } else if (is.numeric(item)) {
        item_exclusions <- c(item_exclusions, as.character(item))
      }
    }
    if (length(item_exclusions) > 0) {
      cat("*EXCLUDE ITEMS ", paste(item_exclusions, collapse = " "), "\n", 
          sep = "", file = con_in)
    }
  }
  
  # Add EXCLUDE CHARACTERS if specified
  if (!is.null(exclude_characters) && length(exclude_characters) > 0) {
    char_exclusions <- character()
    for (char in exclude_characters) {
      if (is.character(char)) {
        for (i in 1:num_chars) {
          if (delta$get_char_feature(i) == char) {
            char_exclusions <- c(char_exclusions, as.character(i))
            break
          }
        }
      } else if (is.numeric(char)) {
        char_exclusions <- c(char_exclusions, as.character(char))
      }
    }
    if (length(char_exclusions) > 0) {
      cat("*EXCLUDE CHARACTERS ", paste(char_exclusions, collapse = " "), "\n", 
          sep = "", file = con_in)
    }
  }
  
  cat("*TRANSLATE INTO DISTANCE FORMAT\n", file = con_in)
  cat("*DIST OUTPUT FILE ditems\n", file = con_in)
  cat("*INPUT FILE ", items_file, "\n", sep = "", file = con_in)
  
  close(con_in)
  
  # Run CONFOR
  if (Sys.info()["sysname"] == "Windows") {
    result <- system(paste("confor", con_file))
  } else {
    confor_cmd <- if (file.exists("./confor")) "./confor" else "confor"
    result <- system(paste(confor_cmd, con_file))
  }
  
  if (result != 0) {
    stop("Error: CONFOR execution failed!")
  }
  
  # Check if binary file was created
  if (!file.exists("ditems")) {
    stop("Error: DIST binary file (ditems) not created!")
  }
  
  # --- SECOND STAGE: Create DIST directives ---
  dirfile <- "dist"
  dist_in <- file(dirfile, "w")
  
  cat("*COMMENT Generate distance matrix.\n", file = dist_in)
  cat("*ITEMS FILE ditems\n", file = dist_in)
  
  if (phylip_format) {
    cat("*PHYLIP FORMAT\n", file = dist_in)
  }
  
  cat("*OUTPUT FILE ", dirfile, ".dis\n", sep = "", file = dist_in)
  cat("*MINIMUM NUMBER OF COMPARISONS ", minimum_comparisons, "\n", sep = "", file = dist_in)
  
  close(dist_in)
  
  # Run DIST
  if (Sys.info()["sysname"] == "Windows") {
    result <- system(paste("dist", dirfile))
  } else {
    dist_cmd <- if (file.exists("./dist")) "./dist" else "dist"
    result <- system(paste(dist_cmd, dirfile))
  }
  
  if (result != 0) {
    stop("Error: DIST execution failed!")
  }
  
  # Check if distance matrix was created
  output_file <- paste0(dirfile, ".dis")
  if (!file.exists(output_file)) {
    stop("Error: DIST output file not created!")
  }
  
  # Clean up item names in dist.nam (replace spaces with underscores)
  nam_file <- paste0(dirfile, ".nam")
  if (file.exists(nam_file)) {
    nam_lines <- readLines(nam_file)
    nam_lines <- gsub(" ", "_", nam_lines)
    writeLines(nam_lines, nam_file)
  }
  
  # Clean up temporary files
  if (remove_temp) {
    # Remove directives files
    if (file.exists(con_file)) unlink(con_file)
    if (file.exists(dirfile)) unlink(dirfile)
    if (file.exists(paste0(dirfile, ".lst"))) unlink(paste0(dirfile, ".lst"))
    
    # Remove DIST binary file
    if (file.exists("ditems")) unlink("ditems")
  }
  
  message("Distance matrix generated successfully")
  message("  - Distance matrix: ", dirfile, ".dis")
  message("  - Item names: ", dirfile, ".nam")
  if (file.exists(paste0(dirfile, ".dst"))) {
    message("  - DIST binary file: ", dirfile)
  }
  
  invisible(0)
}