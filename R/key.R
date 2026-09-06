#' Generate Dichotomous Key using CONFOR/KEY
#' 
#' This function replicates the KEY command in the DELTA system for generating
#' dichotomous keys. It creates the necessary directives files and runs the 
#' CONFOR and KEY programs.
#'
#' @param delta A Delta dataset object (created by \code{\link{load_delta}})
#' @param heading Heading for the output (default = NULL, uses heading from specs file)
#' @param add_character_numbers Character numbers are to be inserted in bracketed keys (default = FALSE)
#' @param no_bracketted_key Suppresses the production of a key in the conventional, 'bracketed' format (default = FALSE)
#' @param no_tabular_key Suppresses the production of a key in tabular format (default = TRUE)
#' @param number_of_confirmatory_characters Number of confirmatory characters that will be sought for each main character in the key (default = 0, maximum = 4)
#' @param treat_characters_as_variable A list of characters for which particular attributes are to be treated as 'variable' (default = NULL)
#' @param use_normal_values A list of numeric characters for which extreme values are to be ignored and normal values used when translating into other formats (default = NULL)
#' @param character_reliabilities A list of the 'reliabilities' of the characters (default = NULL)
#' @param key_states A list of numeric character states to be divided into ranges for use as states in identification keys (default = NULL)
#' @param abase Sets the base of the logarithmic item abundance scale (default = 2)
#' @param rbase Sets the base of the logarithmic character-reliability scale (default = 1.4)
#' @param reuse Attempt to minimize the number of different characters used in the key (default = 1.01)
#' @param varywt Sets the value of a parameter that determines the treatment of intra-taxon variability in the process of character selection (default = 0.8)
#' @param print_width Maximum line length for output of the keys (default = 80)
#' @param exclude_items A list of items to be excluded (default = NULL)
#' @param exclude_characters A list of characters to be excluded (default = NULL)
#' @param remove_temp Logical; if TRUE, remove temporary files after conversion (default = TRUE)
#'
#' @return Invisibly returns the key as a character vector on success
#' @export
#'
#' @details The function assumes that the CONFOR and KEY program executables are available 
#'          in the system PATH or in the current working directory.
#'
#' @examples
#' \dontrun{
#' # Load a DELTA dataset
#' delta <- load_delta("chars", "items", "specs")
#' 
#' # Generate dichotomous key
#' key(delta)
#' 
#' # With custom options
#' key(delta, abase = 2, rbase = 1.4, 
#'     no_tabular_key = TRUE, number_of_confirmatory_characters = 2)
#' }
key <- function(delta, 
                heading = NULL,
                add_character_numbers = FALSE, 
                no_bracketted_key = FALSE, 
                no_tabular_key = TRUE, 
                number_of_confirmatory_characters = 0, 
                treat_characters_as_variable = NULL,
                use_normal_values = NULL,
                character_reliabilities = NULL,
                key_states = NULL,
                abase = 2, 
                rbase = 1.4, 
                reuse = 1.01, 
                varywt = 0.8, 
                print_width = 80, 
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
  
  # Validate parameters
  if (number_of_confirmatory_characters < 0 || number_of_confirmatory_characters > 4) {
    stop("number_of_confirmatory_characters must be between 0 and 4")
  }
  
  if (abase <= 0) stop("abase must be positive")
  if (rbase <= 0) stop("rbase must be positive")
  if (reuse <= 0) stop("reuse must be positive")
  if (varywt < 0 || varywt > 1) stop("varywt must be between 0 and 1")
  if (print_width <= 0) stop("print_width must be positive")
  
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
  
  # --- FIRST STAGE: Create CONFOR directives to generate KEY binary files ---
  con_file <- "tokey"
  con_in <- file(con_file, "w")
  
  cat("*SHOW Translate into KEY format\n", file = con_in)
  cat("*SHOW Generated on ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "\n\n", sep = "", file = con_in)
  cat("*HEADING ", heading_text, "\n\n", sep = "", file = con_in)
  
  # Include specifications file if it exists
  if (!is.null(specs_file) && file.exists(specs_file)) {
    cat("*INPUT FILE ", specs_file, "\n\n", sep = "", file = con_in)
  }
  
  # Add USE NORMAL VALUES if specified
  if (!is.null(use_normal_values) && length(use_normal_values) > 0) {
    cat("*USE NORMAL VALUES ", paste(use_normal_values, collapse = " "), "\n", sep = "", file = con_in)
  }
  
  # Add CHARACTER RELIABILITIES if specified
  if (!is.null(character_reliabilities) && length(character_reliabilities) > 0) {
    cat("*CHARACTER RELIABILITIES ", paste(character_reliabilities, collapse = " "), "\n", sep = "", file = con_in)
  }
  
  # Add KEY STATES if specified
  if (!is.null(key_states) && length(key_states) > 0) {
    cat("*KEY STATES ", paste(key_states, collapse = " "), "\n", sep = "", file = con_in)
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
      cat("*EXCLUDE ITEMS ", paste(item_exclusions, collapse = " "), "\n\n", 
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
      cat("*EXCLUDE CHARACTERS ", paste(char_exclusions, collapse = " "), "\n\n", 
          sep = "", file = con_in)
    }
  }
  
  cat("*TRANSLATE INTO KEY FORMAT\n\n", file = con_in)
  cat("*KEY OUTPUT FILE kchars\n", file = con_in)
  cat("*INPUT FILE ", chars_file, "\n\n", sep = "", file = con_in)
  cat("*KEY OUTPUT FILE kitems\n", file = con_in)
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
  
  # Check if binary files were created
  if (!file.exists("kchars") || !file.exists("kitems")) {
    stop("Error: KEY binary files not created!")
  }
  
  # --- SECOND STAGE: Create KEY directives ---
  dirfile <- "key"
  key_in <- file(dirfile, "w")
  
  cat("*COMMENT Generate dichotomous key.\n", file = key_in)
  cat("*HEADING ", heading_text, "\n", sep = "", file = key_in)
  cat("*KEY OUTPUT FILE key.txt\n", file = key_in)
  cat("\n", file = key_in)
  cat("*ABASE ", abase, "\n", sep = "", file = key_in)
  cat("*RBASE ", rbase, "\n", sep = "", file = key_in)
  cat("*REUSE ", reuse, "\n", sep = "", file = key_in)
  cat("*VARYWT ", varywt, "\n", sep = "", file = key_in)
  cat("*PRINT WIDTH ", print_width, "\n", sep = "", file = key_in)
  
  # Add NO options if specified
  if (no_bracketted_key) {
    cat("*NO BRACKETTED KEY\n", file = key_in)
  }
  
  if (no_tabular_key) {
    cat("*NO TABULAR KEY\n", file = key_in)
  }
  
  cat("\n", file = key_in)
  cat("*NUMBER OF CONFIRMATORY CHARACTERS ", number_of_confirmatory_characters, "\n", sep = "", file = key_in)
  
  # Add TREAT CHARACTERS AS VARIABLE if specified
  if (!is.null(treat_characters_as_variable) && length(treat_characters_as_variable) > 0) {
    cat("*TREAT CHARACTERS AS VARIABLE ", paste(treat_characters_as_variable, collapse = " "), "\n", sep = "", file = key_in)
  }
  
  # Add add_character_numbers option
  if (add_character_numbers) {
    cat("*ADD CHARACTER NUMBERS\n", file = key_in)
  }
  
  close(key_in)
  
  # Run KEY - KEY automatically finds kchars and kitems in the current directory
  if (Sys.info()["sysname"] == "Windows") {
    result <- system(paste("key", dirfile))
  } else {
    key_cmd <- if (file.exists("./key")) "./key" else "key"
    result <- system(paste(key_cmd, dirfile))
  }
  
  if (result != 0) {
    stop("Error: KEY execution failed!")
  }
  
  # Check if key file was created
  output_file <- "key.txt"
  if (!file.exists(output_file)) {
    stop("Error: KEY output file (key.txt) not created!")
  }
  
  # Read the generated key
  key_output <- readLines(output_file)
  
  # Clean up temporary files (but keep the output file)
  if (remove_temp) {
    # Remove directives files
    if (file.exists(con_file)) unlink(con_file)
    if (file.exists(dirfile)) unlink(dirfile)
    if (file.exists(paste0(dirfile, ".lst"))) unlink(paste0(dirfile, ".lst"))
    
    # Remove CONFOR-generated binary files
    if (file.exists("kchars")) unlink("kchars")
    if (file.exists("kitems")) unlink("kitems")
    
    # DO NOT delete key.txt - this is the output file!
  }
  
  message("Dichotomous key generated successfully")
  message("  - Key file: key.txt")
  
  invisible(key_output)
}