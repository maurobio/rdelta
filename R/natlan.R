#' Translate DELTA-format data into natural language descriptions via CONFOR
#'
#' This function replicates the TRANSLATE INTO NATURAL LANGUAGE command in the 
#' DELTA system. It creates the necessary directives file and runs the CONFOR 
#' program to generate natural language descriptions.
#'
#' @param delta A Delta dataset object (created by \code{\link{load_delta}})
#' @param heading Heading for the descriptions (default = NULL, uses heading from specs file)
#' @param replace_angle_brackets Replace angle brackets with parentheses (default = TRUE)
#' @param omit_character_numbers Omit character numbers from descriptions (default = TRUE)
#' @param omit_inapplicables Omit 'or not applicable' from descriptions (default = TRUE)
#' @param omit_comments Omit comments in attributes (default = TRUE)
#' @param omit_inner_comments Omit inner comments in character list and attributes (default = TRUE)
#' @param omit_final_comma Omit final comma before 'and' (default = TRUE)
#' @param translate_implicit_values Output implicit values in descriptions (default = FALSE)
#' @param omit_lower_for_characters List of numeric characters to omit lower values
#' @param omit_or_for_characters List of characters to omit 'or' between alternatives
#' @param omit_period_for_characters List of characters where period would normally terminate a sentence
#' @param new_paragraphs_at_characters List of characters where new paragraphs should start
#' @param item_subheadings List of subheadings for items (delimiter/character/value)
#' @param link_characters List of characters specifying how attributes are combined
#' @param replace_semicolon_by_comma List of characters to use comma instead of semicolon
#' @param print_width Maximum line length for output (default = 80)
#' @param exclude_items List of items to exclude
#' @param exclude_characters List of characters to exclude
#' @param vocabulary List for custom vocabulary (default = NULL)
#' @param remove_temp Logical; if TRUE, remove temporary files after conversion (default = TRUE)
#'
#' @return Invisibly returns the descriptions as a character vector
#' @export
#'
#' @details The function assumes that the CONFOR program executable is available 
#'          in the system PATH or in the current working directory.
#' @examples
#' \dontrun{
#' # Load a DELTA dataset
#' delta <- load_delta("chars_viola", "items_viola", "specs_viola")
#' 
#' # Generate natural language descriptions
#' natlan(delta)
#' 
#' # With custom heading and subheadings
#' natlan(delta, 
#'        heading = "Flora of Region X - species descriptions",
#'        item_subheadings = list(
#'          "87" = "Transverse section of lamina",
#'          "96" = "Leaf epidermis"
#'        ))
#' }
natlan <- function(delta,
                   heading = NULL,
                   replace_angle_brackets = TRUE,
                   omit_character_numbers = TRUE,
                   omit_inapplicables = TRUE,
                   omit_comments = TRUE,
                   omit_inner_comments = TRUE,
                   omit_final_comma = TRUE,
                   translate_implicit_values = FALSE,
                   omit_lower_for_characters = NULL,
                   omit_or_for_characters = NULL,
                   omit_period_for_characters = NULL,
                   new_paragraphs_at_characters = NULL,
                   item_subheadings = NULL,
                   link_characters = NULL,
                   replace_semicolon_by_comma = NULL,
                   print_width = 80,
                   exclude_items = NULL,
                   exclude_characters = NULL,
                   vocabulary = NULL,
                   remove_temp = TRUE) {
  
  # Validate input - consistent with dist() and key()
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
  heading_text <- "Natural language descriptions"
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
  
  # --- Create CONFOR directives ---
  con_file <- "tonat"
  con_in <- file(con_file, "w")
  
  # 1. SHOW and HEADING directives
  cat("*SHOW Translate into natural language\n", file = con_in)
  cat("*SHOW Generated on ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "\n", sep = "", file = con_in)
  cat("*HEADING ", heading_text, "\n", sep = "", file = con_in)
  cat("*PRINT FILE description.txt\n", file = con_in)
  cat("\n", file = con_in)
  
  # 2. INPUT FILE specs (must come before TRANSLATE)
  if (!is.null(specs_file) && file.exists(specs_file)) {
    cat("*INPUT FILE ", specs_file, "\n", sep = "", file = con_in)
    cat("\n", file = con_in)
  }
  
  # 3. TRANSLATE directive
  cat("*TRANSLATE INTO NATURAL LANGUAGE\n", file = con_in)
  cat("\n", file = con_in)
  
  # 4. Translation options
  if (replace_angle_brackets) {
    cat("*REPLACE ANGLE BRACKETS\n", file = con_in)
  }
  
  if (omit_character_numbers) {
    cat("*OMIT CHARACTER NUMBERS\n", file = con_in)
  }
  
  if (omit_inapplicables) {
    cat("*OMIT INAPPLICABLES\n", file = con_in)
  }
  
  if (omit_comments) {
    cat("*OMIT COMMENTS\n", file = con_in)
  }
  
  if (omit_inner_comments) {
    cat("*OMIT INNER COMMENTS\n", file = con_in)
  }
  
  if (omit_final_comma) {
    cat("*OMIT FINAL COMMA\n", file = con_in)
  }
  
  cat("*OMIT TYPESETTING MARKS\n", file = con_in)
  cat("*PRINT WIDTH ", print_width, "\n", sep = "", file = con_in)
  cat("\n", file = con_in)
  
  # 5. Vocabulary
  cat("*COMMENT English vocabulary. This English vocabulary is built into\n", file = con_in)
  cat("the program, but is supplied separately here so that you can alter it\n", file = con_in)
  cat("to your requirements, for example, by deleting the word '(variant)'.\n", file = con_in)
  cat("\n", file = con_in)
  cat("*VOCABULARY\n", file = con_in)
  
  # Use custom vocabulary or default
  if (!is.null(vocabulary)) {
    # Validate custom vocabulary
    if (!is.list(vocabulary) || is.null(names(vocabulary))) {
      stop("vocabulary must be a named list")
    }
    for (i in seq_along(vocabulary)) {
      cat("#", names(vocabulary)[i], ". ", vocabulary[[i]], "\n", sep = "", file = con_in)
    }
  } else {
    # Default vocabulary
    default_vocab <- c(
      "or",
      "to",
      "and",
      "variable",
      "unknown",
      "not applicable",
      "(variant)",
      "not coded",
      "never",
      "minimum",
      "maximum",
      "up to",
      "or more",
      ".",
      ",",
      ", <alternate comma>",
      ";",
      "."
    )
    for (i in seq_along(default_vocab)) {
      cat("#", i, ". ", default_vocab[i], "\n", sep = "", file = con_in)
    }
  }
  
  cat("\n", file = con_in)
  
  # 6. Additional directives (ITEM SUBHEADINGS, LINK CHARACTERS, etc.)
  # These can come after VOCABULARY and before INPUT FILE chars
  
  # Add item subheadings if provided
  if (!is.null(item_subheadings)) {
    if (!is.list(item_subheadings)) {
      stop("item_subheadings must be a list")
    }
    cat("*ITEM SUBHEADINGS\n", file = con_in)
    for (char_num in names(item_subheadings)) {
      cat("#", char_num, ". ", item_subheadings[[char_num]], "\n", sep = "", file = con_in)
    }
    cat("\n", file = con_in)
  }
  
  # Add link characters if provided
  if (!is.null(link_characters)) {
    if (!is.list(link_characters)) {
      stop("link_characters must be a list")
    }
    cat("*LINK CHARACTERS\n", file = con_in)
    for (char_num in names(link_characters)) {
      cat("#", char_num, ". ", link_characters[[char_num]], "\n", sep = "", file = con_in)
    }
    cat("\n", file = con_in)
  }
  
  # Add omit lower for characters
  if (!is.null(omit_lower_for_characters)) {
    if (!is.character(omit_lower_for_characters)) {
      stop("omit_lower_for_characters must be a character vector")
    }
    cat("*OMIT LOWER FOR CHARACTERS ", paste(omit_lower_for_characters, collapse = " "), 
        "\n\n", sep = "", file = con_in)
  }
  
  # Add omit or for characters
  if (!is.null(omit_or_for_characters)) {
    if (!is.character(omit_or_for_characters)) {
      stop("omit_or_for_characters must be a character vector")
    }
    cat("*OMIT OR FOR CHARACTERS ", paste(omit_or_for_characters, collapse = " "), 
        "\n\n", sep = "", file = con_in)
  }
  
  # Add omit period for characters
  if (!is.null(omit_period_for_characters)) {
    if (!is.character(omit_period_for_characters)) {
      stop("omit_period_for_characters must be a character vector")
    }
    cat("*OMIT PERIOD FOR CHARACTERS ", paste(omit_period_for_characters, collapse = " "), 
        "\n\n", sep = "", file = con_in)
  }
  
  # Add new paragraphs at characters
  if (!is.null(new_paragraphs_at_characters)) {
    if (!is.character(new_paragraphs_at_characters)) {
      stop("new_paragraphs_at_characters must be a character vector")
    }
    cat("*NEW PARAGRAPHS AT CHARACTERS ", paste(new_paragraphs_at_characters, collapse = " "), 
        "\n\n", sep = "", file = con_in)
  }
  
  # Add replace semicolon by comma
  if (!is.null(replace_semicolon_by_comma)) {
    if (!is.character(replace_semicolon_by_comma)) {
      stop("replace_semicolon_by_comma must be a character vector")
    }
    cat("*REPLACE SEMICOLON BY COMMA ", paste(replace_semicolon_by_comma, collapse = " "), 
        "\n\n", sep = "", file = con_in)
  }
  
  # Add translate implicit values
  if (translate_implicit_values) {
    cat("*TRANSLATE IMPLICIT VALUES\n\n", file = con_in)
  }
  
  # Add exclude items
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
  
  # Add exclude characters
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
  
  # 7. INPUT FILE chars and items (must come after all translation directives)
  cat("*INPUT FILE ", chars_file, "\n", sep = "", file = con_in)
  cat("\n", file = con_in)
  cat("*PRINT HEADING\n", file = con_in)
  cat("\n", file = con_in)
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
  
  # Check if description file was created
  output_file <- "description.txt"
  if (!file.exists(output_file)) {
    stop("Error: Description file (description.txt) not created!")
  }
  
  # Read the generated descriptions
  descriptions <- readLines(output_file)
  
  # Clean up temporary files (but keep the output file)
  if (remove_temp) {
    # Remove directives file
    if (file.exists(con_file)) unlink(con_file)
    if (file.exists(paste0(con_file, ".lst"))) unlink(paste0(con_file, ".lst"))
    
    # DO NOT delete description.txt - this is the output file!
  }
  
  message("Natural language descriptions generated successfully")
  message("  - Number of items: ", num_items)
  message("  - Number of characters: ", num_chars)
  message("  - Output file: description.txt")
  
  invisible(descriptions)
}