#' Convert DELTA dataset to SLIKS format
#'
#' Converts a DELTA dataset to SLIKS (Simple List of Interactive Key States)
#' format, which is used for interactive identification keys.
#'
#' @param delta A DeltaParser object (created by \code{\link{load_delta}})
#' @param output_file Output file name (default: "data.js")
#' @param exclude_numeric Logical; if TRUE, exclude numeric and text characters
#' @param remove_temp Logical; if TRUE, remove temporary files after conversion
#' @return Invisibly returns the path to the output file
#' @export
#'
#' @details The function assumes that the CONFOR program executable is available 
#'          in the system PATH or in the current working directory.
#'
#' @examples
#' \dontrun{
#' # Load a DELTA dataset
#' delta <- load_delta("chars", "items", "specs")
#' 
#' # Convert to SLIKS format
#' delta2sliks(delta)
#' 
#' # Convert with custom output file
#' delta2sliks(delta, output_file = "mykey.js")
#' }
delta2sliks <- function(delta, output_file = "data.js", 
                        exclude_numeric = TRUE, remove_temp = TRUE) {
  
  # Validate input
  if (!inherits(delta, "DeltaParser")) {
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
  
  # Extract title from characters file
  title <- ""
  if (file.exists(chars_file)) {
    infile <- file(chars_file, "r")
    line <- readLines(infile, n = 1)
    command <- "*SHOW"
    
    while (length(line) > 0) {
      if (startsWith(line, command)) {
        title <- substr(line, nchar(command) + 1, nchar(line))
        title <- if (nchar(title) > 0) substr(title, 2, nchar(title)) else title
        break
      }
      line <- readLines(infile, n = 1)
    }
    close(infile)
  }
  
  # If title is empty, use a default
  if (title == "") {
    title <- "DELTA Dataset"
  }
  
  # Prepare dataset for SLIKS conversion
  if (exclude_numeric) {
    # Get numeric and text characters to exclude
    excluded <- c()
    for (i in 1:delta$get_chars_nb()) {
      char_type <- delta$get_char_type(i)
      # CT_IN = 4 (integer numeric), CT_RN = 5 (real numeric), CT_TE = 8 (text)
      if (char_type %in% c(4, 5, 8)) {
        excluded <- c(excluded, i)
      }
    }
    
    if (length(excluded) > 0) {
      # Create CONFOR directives file
      con_file <- "delchars"
      con_in <- file(con_file, "w")
      cat("*SHOW ~ Translate into DELTA format, omitting numeric and text characters\n\n", 
          file = con_in)
      cat("*LISTING FILE delchars.lst\n\n", file = con_in)
      
      if (!is.null(specs_file) && file.exists(specs_file)) {
        cat("*INPUT FILE ", specs_file, "\n\n", sep = "", file = con_in)
      }
      
      cat("*EXCLUDE CHARACTERS ", paste(excluded, collapse = " "), "\n\n", 
          sep = "", file = con_in)
      cat("*TRANSLATE INTO DELTA FORMAT\n\n", file = con_in)
      cat("*OUTPUT FILE chars.new\n", file = con_in)
      cat("*OUTPUT PARAMETERS\n", file = con_in)
      cat("#SHOW ", title, "\n\n", sep = "", file = con_in)
      cat("#CHARACTER LIST\n", file = con_in)
      cat("*INPUT FILE ", chars_file, "\n\n", sep = "", file = con_in)
      cat("*OUTPUT FILE items.new\n", file = con_in)
      cat("*OUTPUT PARAMETERS\n\n", file = con_in)
      cat("#ITEM DESCRIPTIONS\n", file = con_in)
      cat("*INPUT FILE ", items_file, "\n", sep = "", file = con_in)
      close(con_in)
      
      # Run CONFOR
      if (Sys.info()["sysname"] == "Windows") {
        result <- system(paste("confor", con_file))
      } else {
        # Try to find confor in PATH or current directory
        confor_cmd <- if (file.exists("./confor")) "./confor" else "confor"
        result <- system(paste(confor_cmd, con_file))
      }
      
      if (result != 0) {
        warning("CONFOR execution failed. Using original dataset.")
        # Use original dataset if CONFOR fails
        conv_delta <- delta
      } else {
        # Load the converted dataset
        if (file.exists("chars.new") && file.exists("items.new")) {
          conv_delta <- load_delta("chars.new", "items.new")
          if (!(conv_delta$is_parsed() && conv_delta$is_items_parsed())) {
            warning("Failed to parse converted dataset. Using original dataset.")
            conv_delta <- delta
          }
        } else {
          warning("Converted files not found. Using original dataset.")
          conv_delta <- delta
        }
      }
    } else {
      # No numeric/text characters to exclude
      conv_delta <- delta
    }
  } else {
    # Don't exclude any characters
    conv_delta <- delta
  }
  
  # Get counts
  num_chars <- conv_delta$get_chars_nb()
  num_items <- conv_delta$get_items_nb()
  
  # Parse attribute helper function
  parse_attribute <- function(input) {
    commaPos <- regexpr(",", input, fixed = TRUE)[1]
    if (commaPos != -1) {
      attributeValue <- substr(input, commaPos + 1, nchar(input))
      if (grepl("^[0-9]+$", attributeValue)) {
        return(attributeValue)
      }
    }
    return("?")
  }
  
  # Write SLIKS output
  outfile <- file(output_file, "w")
  on.exit(close(outfile), add = TRUE)
  
  # Write header
  cat("var dataset = \"<h2>", title, "</h2>\"\n\n", sep = "", file = outfile)
  
  # Output characters list
  cat("var chars = [ [ \"Latin Name\"],\n", file = outfile)
  for (i in 1:num_chars) {
    cat("\t[ \"", conv_delta$get_char_feature(i), "\", ", sep = "", file = outfile)
    num_states <- conv_delta$get_states_nb(i)
    if (num_states > 0) {
      for (j in 1:num_states) {
        state <- conv_delta$get_state(i, j)
        # Escape quotes and backslashes in state names
        state <- gsub("\\", "\\\\", state, fixed = TRUE)
        state <- gsub("\"", "\\\"", state, fixed = TRUE)
        if (j < num_states) {
          cat("\"", state, "\", ", sep = "", file = outfile)
        } else {
          cat("\"", state, "\"", sep = "", file = outfile)
        }
      }
    } else {
      cat("\"\"", file = outfile)
    }
    if (i < num_chars) {
      cat("],\n", file = outfile)
    } else {
      cat("] ]\n", file = outfile)
    }
  }
  
  # Output data matrix
  cat("\n\nvar items = [ [\"\"],\n", file = outfile)
  for (i in 1:num_items) {
    # Get item name
    item_name <- conv_delta$get_item_name(i)
    item_name <- trimws(item_name)
    # Escape quotes in item name
    item_name <- gsub("\"", "\\\"", item_name, fixed = TRUE)
    
    cat("\t[\"", item_name, "\", ", sep = "", file = outfile)
    
    # Get attributes for this item
    num_attrs <- conv_delta$get_attributes_nb(i)
    
    if (num_attrs > 0) {
      for (j in 1:num_attrs) {
        attr_val <- conv_delta$get_attribute(i, j)
        parsed_val <- parse_attribute(attr_val)
        
        if (j < num_attrs) {
          cat("\"", parsed_val, "\",", sep = "", file = outfile)
        } else {
          cat("\"", parsed_val, "\"", sep = "", file = outfile)
        }
      }
    } else {
      cat("\"?\"", file = outfile)
    }
    
    if (i < num_items) {
      cat("],\n", file = outfile)
    } else {
      cat("]\n", file = outfile)
    }
  }
  # Close the items array
  cat("]\n", file = outfile)
  
  # Clean up temporary files
  if (remove_temp && exclude_numeric && length(excluded) > 0) {
    temp_files <- c("delchars", "delchars.lst", "chars.new", "items.new")
    for (f in temp_files) {
      if (file.exists(f)) {
        unlink(f)
      }
    }
  }
  
  message("SLIKS output written to: ", output_file)
  message("Generated ", num_chars, " characters and ", num_items, " items")
  
  invisible(output_file)
}