# File: R/save_delta.R
# Save DELTA dataset to disk - Part of RDelta package

#==============================================================================
# Internal helper functions for word wrapping (not exported)
#==============================================================================

#' Word wrap text to a specified width
#'
#' This function wraps text to a specified maximum width, following DELTA
#' conventions for line wrapping.
#'
#' @param text Character string to wrap
#' @param width Maximum line width (default: 79 for DELTA files)
#' @param indent Indentation for continuation lines (default: 8 spaces)
#' @return Character vector of wrapped lines
#' @noRd
.wrap_delta_text <- function(text, width = 79, indent = 8) {
  if (is.null(text) || length(text) == 0 || nchar(text) == 0) {
    return(text)
  }
  
  words <- strsplit(text, " ")[[1]]
  if (length(words) == 0) return(text)
  
  lines <- character()
  current_line <- words[1]
  
  for (i in 2:length(words)) {
    candidate <- paste(current_line, words[i])
    if (nchar(candidate) <= width) {
      current_line <- candidate
    } else {
      lines <- c(lines, current_line)
      # Start new line with indentation
      current_line <- paste0(strrep(" ", indent), words[i])
    }
  }
  
  if (nchar(current_line) > 0) {
    lines <- c(lines, current_line)
  }
  
  return(lines)
}

#' Wrap a vector of text lines with proper DELTA formatting
#'
#' This function handles different types of DELTA lines:
#' - Directive lines (*): no wrapping
#' - Character state lines: preserve indentation
#' - Item attribute lines: no indentation on continuation lines
#' - Other lines: standard wrapping with 8-space indent
#'
#' @param lines Character vector of lines to wrap
#' @param width Maximum line width (default: 79)
#' @param indent Indentation for continuation lines (default: 8)
#' @return Character vector with wrapped lines
#' @noRd
.wrap_delta_lines <- function(lines, width = 79, indent = 8) {
  result <- character()
  
  for (line in lines) {
    if (is.null(line) || nchar(line) == 0) {
      result <- c(result, line)
      next
    }
    
    # Don't wrap lines that are directives (start with *)
    if (substr(line, 1, 1) == "*") {
      result <- c(result, line)
      next
    }
    
    # Check if it's a character state line (starts with tab/spaces + number + dot)
    if (grepl("^[ \\t]+[0-9]+\\.", line)) {
      # Extract the indentation and the rest
      indent_match <- regexpr("^[ \\t]+", line)
      if (indent_match > 0) {
        indent_str <- substr(line, 1, attr(indent_match, "match.length"))
        content <- substr(line, attr(indent_match, "match.length") + 1, nchar(line))
        
        # Wrap the content with the same indentation for continuation lines
        wrapped <- .wrap_delta_text(content, width - nchar(indent_str), nchar(indent_str))
        if (length(wrapped) > 0) {
          result <- c(result, paste0(indent_str, wrapped[1]))
          if (length(wrapped) > 1) {
            for (i in 2:length(wrapped)) {
              result <- c(result, wrapped[i])
            }
          }
        }
        next
      }
    }
    
    # Check if it's an item attribute line (starts with numbers, commas, slashes)
    # Item attribute lines typically start with character numbers like "1,1 2,1/2"
    if (grepl("^[0-9, /-]+", line)) {
      # Item attributes should NOT have indentation on continuation lines
      wrapped <- .wrap_delta_text(line, width, 0)  # indent = 0 for item attributes
      result <- c(result, wrapped)
      next
    }
    
    # Check if it's a character line (starts with #number)
    if (grepl("^#[0-9]+\\.", line)) {
      # Character lines should wrap with the same indentation as the start
      # Extract the indent (which is 0 for character lines)
      wrapped <- .wrap_delta_text(line, width, 0)
      result <- c(result, wrapped)
      next
    }
    
    # Default: wrap with 8-space indentation
    wrapped <- .wrap_delta_text(line, width, indent)
    result <- c(result, wrapped)
  }
  
  return(result)
}

#==============================================================================
# save_delta - Export DELTA dataset to disk
#==============================================================================

#' Save a Delta dataset to disk
#'
#' This function saves a Delta dataset object to disk as three plain text files:
#' "chars", "items", and "specs" in the DELTA format.
#'
#' @param delta A Delta dataset object (Rcpp_DeltaParser from load_delta())
#' @param dir Path to the directory where files should be saved (default: current directory)
#' @param prefix Optional prefix for filenames (default: "")
#' @param overwrite Whether to overwrite existing files (default: FALSE)
#' @param width Maximum line width for output files (default: 79, range: 40-132)
#'
#' @return Invisibly returns the input delta object
#' @export
#'
#' @examples
#' \dontrun{
#' delta <- load_delta("chars", "items", "specs")
#' save_delta(delta)
#' save_delta(delta, dir = "output", prefix = "my_", overwrite = TRUE)
#' save_delta(delta, width = 120)  # Use wider output
#' }
save_delta <- function(delta, dir = ".", prefix = "", overwrite = FALSE, width = 79) {
  # Check if it's a DeltaParser object
  if (!inherits(delta, "Rcpp_DeltaParser")) {
    stop("delta must be a Delta dataset object created by load_delta()")
  }
  
  # Validate width
  if (width < 40 || width > 132) {
    stop("width must be between 40 and 132")
  }
  
  # Create directory if it doesn't exist
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  
  # Define file paths
  char_file <- file.path(dir, paste0(prefix, "chars"))
  item_file <- file.path(dir, paste0(prefix, "items"))
  spec_file <- file.path(dir, paste0(prefix, "specs"))
  
  # Check if files exist and handle overwrite
  existing_files <- c(char_file, item_file, spec_file)[file.exists(c(char_file, item_file, spec_file))]
  if (length(existing_files) > 0 && !overwrite) {
    stop("Files already exist: ", paste(existing_files, collapse = ", "), 
         ". Use overwrite = TRUE to replace them.")
  }
  
  # --- Write CHARS file ---
  chars_content <- c(
    "*SHOW Characters",
    "*SHOW Generated by RDelta save_delta()",
    "",
    "*CHARACTER LIST",
    ""
  )
  
  num_chars <- delta$get_chars_nb()
  for (i in 1:num_chars) {
    feature <- delta$get_char_feature(i)
    char_type <- delta$get_char_type(i)
    
    char_line <- paste0("#", i, ". ", feature, "/")
    chars_content <- c(chars_content, char_line)
    
    if (char_type == 2 || char_type == 3) {
      n_states <- delta$get_states_nb(i)
      for (j in 1:n_states) {
        state <- delta$get_state(i, j)
        state_line <- paste0("\t", j, ". ", state, "/")
        chars_content <- c(chars_content, state_line)
      }
    } else if (char_type == 4 || char_type == 5) {
      unit <- delta$get_char_unit(i)
      if (!is.null(unit) && unit != "") {
        unit_line <- paste0("\t", unit, "/")
        chars_content <- c(chars_content, unit_line)
      }
    }
    chars_content <- c(chars_content, "")
  }
  
  chars_content <- .wrap_delta_lines(chars_content, width)
  
  # --- Write ITEMS file ---
  items_content <- c(
    "*SHOW Items",
    "*SHOW Generated by RDelta save_delta()",
    "",
    "*ITEM DESCRIPTIONS",
    ""
  )
  
  num_items <- delta$get_items_nb()
  for (i in 1:num_items) {
    name <- delta$get_item_name(i)
    items_content <- c(items_content, paste0("# ", name, "/"))
    
    n_attrs <- delta$get_attributes_nb(i)
    attrs <- character()
    for (j in 1:n_attrs) {
      attrs <- c(attrs, delta$get_attribute(i, j))
    }
    if (length(attrs) > 0) {
      attr_line <- paste(attrs, collapse = " ")
      items_content <- c(items_content, attr_line)
    }
    items_content <- c(items_content, "")
  }
  
  items_content <- .wrap_delta_lines(items_content, width)
  
  # --- Write SPECS file ---
  # Calculate maximum number of states
  maxStates <- 0
  for (i in 1:num_chars) {
    char_type <- delta$get_char_type(i)
    if (char_type == 2 || char_type == 3) {
      n_states <- delta$get_states_nb(i)
      if (n_states > maxStates) {
        maxStates <- n_states
      }
    }
  }
  if (maxStates == 0) maxStates <- 2
  
  # Calculate data buffer size
  size <- num_chars * 20
  dataBufferSize <- if (size > 2000) size else 2000
  if (dataBufferSize == 0) dataBufferSize <- 2000
  
  # Start building specs content
  specs_content <- c(
    "*SHOW Specifications",
    "*SHOW Generated by RDelta save_delta()",
    "",
    paste0("*NUMBER OF CHARACTERS ", num_chars),
    paste0("*MAXIMUM NUMBER OF STATES ", maxStates),
    paste0("*MAXIMUM NUMBER OF ITEMS ", num_items),
    paste0("*DATA BUFFER SIZE ", dataBufferSize),
    ""
  )
  
  # Character types
  type_parts <- character()
  for (i in 1:num_chars) {
    char_type <- delta$get_char_type(i)
    if (char_type == 2) type_code <- "OM"
    else if (char_type == 3) type_code <- "OM"
    else if (char_type == 4) type_code <- "IN"
    else if (char_type == 5) type_code <- "RN"
    else if (char_type == 8) type_code <- "TE"
    else type_code <- "OM"
    
    if (type_code != "OM") {
      type_parts <- c(type_parts, paste0(i, ",", type_code))
    }
  }
  
  if (length(type_parts) == 0) {
    type_str <- paste0("1-", num_chars, ",OM")
  } else {
    type_str <- paste(type_parts, collapse = " ")
  }
  specs_content <- c(specs_content, paste0("*CHARACTER TYPES ", type_str))
  
  # Numbers of states
  if (maxStates > 2) {
    state_parts <- character()
    for (i in 1:num_chars) {
      char_type <- delta$get_char_type(i)
      if (char_type == 2 || char_type == 3) {
        n_states <- delta$get_states_nb(i)
        if (n_states > 2) {
          state_parts <- c(state_parts, paste0(i, ",", n_states))
        }
      }
    }
    if (length(state_parts) > 0) {
      specs_content <- c(specs_content, paste0("*NUMBERS OF STATES ", paste(state_parts, collapse = " ")))
    }
  }
  
  # Implicit values
  specs_content <- c(specs_content, "")
  implicit_parts <- character()
  tryCatch({
    implicit_vals <- delta$get_all_implicit_values(1)
    if (!is.null(implicit_vals) && length(implicit_vals) > 0) {
      for (i in 1:length(implicit_vals)) {
        if (!is.na(implicit_vals[i]) && implicit_vals[i] > 0) {
          implicit_parts <- c(implicit_parts, paste0(i, ",", implicit_vals[i]))
        }
      }
    }
  }, error = function(e) {})
  
  if (length(implicit_parts) > 0) {
    specs_content <- c(specs_content, paste0("*IMPLICIT VALUES ", paste(implicit_parts, collapse = " ")))
  } else {
    specs_content <- c(specs_content, "*IMPLICIT VALUES")
  }
  
  # Dependent characters
  specs_content <- c(specs_content, "")
  dep_parts <- character()
  
  for (ccnum in 1:num_chars) {
    n_states <- delta$get_states_nb(ccnum)
    for (ccstate in 1:n_states) {
      dep_count <- tryCatch({
        delta$get_depchar_nb(ccnum, ccstate)
      }, error = function(e) { 0 })
      
      if (!is.null(dep_count) && dep_count > 0) {
        dep_chars <- tryCatch({
          delta$get_all_depchar(ccnum, ccstate)
        }, error = function(e) { integer(0) })
        
        if (length(dep_chars) > 0) {
          dep_str <- paste0(ccnum, ",", ccstate, ":", paste(dep_chars, collapse = "-"))
          dep_parts <- c(dep_parts, dep_str)
        }
      }
    }
  }
  
  if (length(dep_parts) > 0) {
    specs_content <- c(specs_content, paste0("*DEPENDENT CHARACTERS ", paste(dep_parts, collapse = " ")))
  } else {
    specs_content <- c(specs_content, "*DEPENDENT CHARACTERS")
  }
  
  # Mandatory characters
  specs_content <- c(specs_content, "", "*MANDATORY CHARACTERS")
  
  specs_content <- .wrap_delta_lines(specs_content, width)
  
  # --- Write all files ---
  writeLines(chars_content, char_file)
  writeLines(items_content, item_file)
  writeLines(specs_content, spec_file)
  
  invisible(delta)
}