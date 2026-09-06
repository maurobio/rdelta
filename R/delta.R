#==============================================================================
# RDelta - R Interface (Complete)
# ALL Functions Exported for PDF Manual and Tests
#==============================================================================

#' @importFrom Rcpp loadModule

#------------------------------------------------------------------------------
# Package Loader
#------------------------------------------------------------------------------

.onLoad <- function(libname, pkgname) {
  tryCatch({
    Rcpp::loadModule("delta_module", TRUE)
    invisible()
  }, error = function(e) {
    warning("Could not load DeltaParser module: ", e$message)
  })
}

#------------------------------------------------------------------------------
# Data Loading
#------------------------------------------------------------------------------

#' Load a DELTA dataset
#'
#' Create a DeltaParser object from DELTA format files.
#'
#' @param chars_file Path to the DELTA characters file
#' @param items_file Path to the DELTA items file
#' @param specs_file Optional path to the DELTA specifications file
#' @return A DeltaParser Rcpp module object
#' @export
load_delta <- function(chars_file, items_file, specs_file = NULL) {
  if (!exists("DeltaParser")) {
    tryCatch({
      loadModule("delta_module", TRUE)
    }, error = function(e) {
      stop("DeltaParser module not found. Please reinstall the package.")
    })
  }
  
  if (!file.exists(chars_file)) {
    stop("Characters file not found: ", chars_file)
  }
  if (!file.exists(items_file)) {
    stop("Items file not found: ", items_file)
  }
  if (!is.null(specs_file) && !file.exists(specs_file)) {
    stop("Specifications file not found: ", specs_file)
  }
  
  if (is.null(specs_file)) {
    new(DeltaParser, chars_file, items_file)
  } else {
    new(DeltaParser, chars_file, items_file, specs_file)
  }
}

#------------------------------------------------------------------------------
# R-friendly DataFrame Functions (High-level)
#------------------------------------------------------------------------------

#' Get character information as a data frame
#'
#' Extracts all character definitions from a DELTA dataset and returns them as a
#' data frame.
#'
#' @param delta A DeltaParser object
#' @return A data.frame with character information
#' @export
characters_df <- function(delta) {
  n <- delta$get_chars_nb()
  data.frame(
    number = seq_len(n),
    feature = sapply(seq_len(n), function(i) delta$get_char_feature(i)),
    type = sapply(seq_len(n), function(i) {
      ct <- delta$get_char_type(i)
      switch(as.character(ct),
        "2" = "unordered_multistate",
        "3" = "ordered_multistate",
        "4" = "integer_numeric",
        "5" = "real_numeric",
        "8" = "text",
        "unknown"
      )
    }),
    unit = sapply(seq_len(n), function(i) delta$get_char_unit(i)),
    states = I(lapply(seq_len(n), function(i) {
      if (delta$get_char_type(i) %in% c(2, 3)) {
        sapply(seq_len(delta$get_states_nb(i)), 
               function(j) delta$get_state(i, j))
      } else {
        character(0)
      }
    })),
    stringsAsFactors = FALSE
  )
}

#' Get item information as a data frame
#'
#' Extracts all item information from a DELTA dataset and returns it as a
#' data frame.
#'
#' @param delta A DeltaParser object
#' @param include_comments Logical; if TRUE, include comments in item names
#' @return A data.frame with item information
#' @export
items_df <- function(delta, include_comments = FALSE) {
  n <- delta$get_items_nb()
  data.frame(
    number = seq_len(n),
    name = sapply(seq_len(n), function(i) delta$get_item_name(i, include_comments)),
    attribute_count = sapply(seq_len(n), function(i) delta$get_attributes_nb(i)),
    stringsAsFactors = FALSE
  )
}

#' Get complete item data as a list
#'
#' Retrieves all data for a specific item.
#'
#' @param delta A DeltaParser object
#' @param itemnum Integer item number
#' @param include_comments Logical; if TRUE, include comments in item names
#' @return A list with item name and attributes
#' @export
get_item <- function(delta, itemnum, include_comments = FALSE) {
  delta$get_item_data(itemnum, include_comments)
}

#------------------------------------------------------------------------------
# Character Methods - R-friendly Names (EXPORTED)
#------------------------------------------------------------------------------

#' Get number of characters
#'
#' @param delta A DeltaParser object
#' @return Integer number of characters
#' @export
char_count <- function(delta) {
  delta$get_chars_nb()
}

#' Get character feature
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @return Character string with the feature description
#' @export
char_feature <- function(delta, charnum) {
  delta$get_char_feature(charnum)
}

#' Get character type
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @return Integer character type code
#' @export
char_type <- function(delta, charnum) {
  delta$get_char_type(charnum)
}

#' Get character type name
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @return Character string with type name
#' @export
char_type_name <- function(delta, charnum) {
  delta$get_char_type_name(charnum)
}

#' Get character unit
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @return Character string with the unit
#' @export
char_unit <- function(delta, charnum) {
  delta$get_char_unit(charnum)
}

#' Get number of states for a character
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @return Integer number of states
#' @export
state_count <- function(delta, charnum) {
  delta$get_states_nb(charnum)
}

#' Get state name
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @param statenum State number
#' @return Character string with state name
#' @export
state_name <- function(delta, charnum, statenum) {
  delta$get_state(charnum, statenum)
}

#' Get all states for a character
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @return Character vector of state names
#' @export
states <- function(delta, charnum) {
  delta$get_states(charnum)
}

#' Set character type
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @param chartype Integer character type code
#' @export
set_char_type <- function(delta, charnum, chartype) {
  delta$set_char_type(charnum, chartype)
}

#' Set character type by name
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @param type_name Character type name
#' @export
set_char_type_by_name <- function(delta, charnum, type_name) {
  delta$set_char_type_by_name(charnum, type_name)
}

#' Get characters filename
#'
#' @param delta A DeltaParser object
#' @return Character string with the filename
#' @export
chars_filename <- function(delta) {
  delta$get_filename()
}

#' Check if characters are parsed
#'
#' @param delta A DeltaParser object
#' @return Logical
#' @export
is_chars_parsed <- function(delta) {
  delta$is_parsed()
}

#' Set characters filename
#'
#' @param delta A DeltaParser object
#' @param fname New filename
#' @param parse Logical, parse the file immediately
#' @export
set_chars_filename <- function(delta, fname, parse = TRUE) {
  delta$set_filename(fname, parse)
}

#' Parse characters file
#'
#' @param delta A DeltaParser object
#' @return Logical indicating success
#' @export
parse_characters <- function(delta) {
  delta$parse_characters()
}

#' Get character debug output
#'
#' @param delta A DeltaParser object
#' @return Character string with debug output
#' @export
get_chars_debug <- function(delta) {
  delta$get_chars_debug()
}

#------------------------------------------------------------------------------
# Character Methods - C++ Style (EXPORTED for compatibility)
#------------------------------------------------------------------------------

#' Get number of characters (C++ style)
#'
#' @param delta A DeltaParser object
#' @return Integer number of characters
#' @export
get_chars_nb <- function(delta) {
  delta$get_chars_nb()
}

#' Get character type (C++ style)
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @return Integer character type code
#' @export
get_char_type <- function(delta, charnum) {
  delta$get_char_type(charnum)
}

#' Get character feature (C++ style)
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @return Character string with the feature description
#' @export
get_char_feature <- function(delta, charnum) {
  delta$get_char_feature(charnum)
}

#' Get character unit (C++ style)
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @return Character string with the unit
#' @export
get_char_unit <- function(delta, charnum) {
  delta$get_char_unit(charnum)
}

#' Get number of states (C++ style)
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @return Integer number of states
#' @export
get_states_nb <- function(delta, charnum) {
  delta$get_states_nb(charnum)
}

#' Get state name (C++ style)
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @param statenum State number
#' @return Character string with state name
#' @export
get_state <- function(delta, charnum, statenum) {
  delta$get_state(charnum, statenum)
}

#' Get filename (C++ style)
#'
#' @param delta A DeltaParser object
#' @return Character string with the filename
#' @export
get_filename <- function(delta) {
  delta$get_filename()
}

#' Check if parsed (C++ style)
#'
#' @param delta A DeltaParser object
#' @return Logical
#' @export
is_parsed <- function(delta) {
  delta$is_parsed()
}

#' Set filename (C++ style)
#'
#' @param delta A DeltaParser object
#' @param fname New filename
#' @param parse Logical, parse the file immediately
#' @export
set_filename <- function(delta, fname, parse = TRUE) {
  delta$set_filename(fname, parse)
}

#------------------------------------------------------------------------------
# Item Methods - R-friendly Names (EXPORTED)
#------------------------------------------------------------------------------

#' Get number of items
#'
#' @param delta A DeltaParser object
#' @return Integer number of items
#' @export
item_count <- function(delta) {
  delta$get_items_nb()
}

#' Get item name
#'
#' @param delta A DeltaParser object
#' @param itemnum Item number
#' @param include_comments Logical; if TRUE, include comments in item names
#' @return Character string with item name
#' @export
item_name <- function(delta, itemnum, include_comments = TRUE) {
  delta$get_item_name(itemnum, include_comments)
}

#' Get all item names
#'
#' @param delta A DeltaParser object
#' @param include_comments Logical; if TRUE, include comments in item names
#' @return Character vector of item names
#' @export
item_names <- function(delta, include_comments = FALSE) {
  delta$get_item_names(include_comments)
}

#' Get number of attributes for an item
#'
#' @param delta A DeltaParser object
#' @param itemnum Item number
#' @return Integer number of attributes
#' @export
attribute_count <- function(delta, itemnum) {
  delta$get_attributes_nb(itemnum)
}

#' Get attribute string
#'
#' @param delta A DeltaParser object
#' @param itemnum Item number
#' @param attrnum Attribute number
#' @return Character string with the attribute
#' @export
attribute_string <- function(delta, itemnum, attrnum) {
  delta$get_attribute(itemnum, attrnum)
}

#' Get item attributes as data frame
#'
#' @param delta A DeltaParser object
#' @param itemnum Item number
#' @return Data frame with attributes
#' @export
item_attributes <- function(delta, itemnum) {
  delta$get_item_attributes(itemnum)
}

#' Get item data as list
#'
#' @param delta A DeltaParser object
#' @param itemnum Item number
#' @param include_comments Logical; if TRUE, include comments in item names
#' @return List with item data
#' @export
item_data <- function(delta, itemnum, include_comments = FALSE) {
  delta$get_item_data(itemnum, include_comments)
}

#' Get items filename
#'
#' @param delta A DeltaParser object
#' @return Character string with the filename
#' @export
items_filename <- function(delta) {
  delta$get_items_filename()
}

#' Check if items are parsed
#'
#' @param delta A DeltaParser object
#' @return Logical
#' @export
is_items_parsed <- function(delta) {
  delta$is_items_parsed()
}

#' Set items filename
#'
#' @param delta A DeltaParser object
#' @param fname New filename
#' @param parse Logical, parse the file immediately
#' @export
set_items_file <- function(delta, fname, parse = TRUE) {
  delta$set_items_filename(fname, parse)
}

#' Parse items file
#'
#' @param delta A DeltaParser object
#' @return Logical indicating success
#' @export
parse_items <- function(delta) {
  delta$parse_items()
}

#' Get items debug output
#'
#' @param delta A DeltaParser object
#' @return Character string with debug output
#' @export
get_items_debug <- function(delta) {
  delta$get_items_debug()
}

#------------------------------------------------------------------------------
# Item Methods - C++ Style (EXPORTED for compatibility)
#------------------------------------------------------------------------------

#' Get number of items (C++ style)
#'
#' @param delta A DeltaParser object
#' @return Integer number of items
#' @export
get_items_nb <- function(delta) {
  delta$get_items_nb()
}

#' Get item name (C++ style)
#'
#' @param delta A DeltaParser object
#' @param itemnum Item number
#' @param include_comments Logical; if TRUE, include comments in item names
#' @return Character string with item name
#' @export
get_item_name <- function(delta, itemnum, include_comments = TRUE) {
  delta$get_item_name(itemnum, include_comments)
}

#' Get number of attributes (C++ style)
#'
#' @param delta A DeltaParser object
#' @param itemnum Item number
#' @return Integer number of attributes
#' @export
get_attributes_nb <- function(delta, itemnum) {
  delta$get_attributes_nb(itemnum)
}

#' Get attribute string (C++ style)
#'
#' @param delta A DeltaParser object
#' @param itemnum Item number
#' @param attrnum Attribute number
#' @return Character string with the attribute
#' @export
get_attribute <- function(delta, itemnum, attrnum) {
  delta$get_attribute(itemnum, attrnum)
}

#' Get items filename (C++ style)
#'
#' @param delta A DeltaParser object
#' @return Character string with the filename
#' @export
get_items_filename <- function(delta) {
  delta$get_items_filename()
}

#' Set items filename (C++ style)
#'
#' @param delta A DeltaParser object
#' @param fname New filename
#' @param parse Logical, parse the file immediately
#' @export
set_items_filename <- function(delta, fname, parse = TRUE) {
  delta$set_items_filename(fname, parse)
}

#------------------------------------------------------------------------------
# Search Methods (EXPORTED)
#------------------------------------------------------------------------------

#' Find first matching item
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @param values Numeric vector of values
#' @param strict Logical; strict comparison
#' @param with_extrval Logical; include extreme values
#' @return Integer item number or 0 if none
#' @export
first_matching <- function(delta, charnum, values, strict = TRUE, with_extrval = TRUE) {
  delta$first_matching(charnum, as.numeric(values), strict, with_extrval)
}

#' Find next matching item
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @param values Numeric vector of values
#' @param strict Logical; strict comparison
#' @param with_extrval Logical; include extreme values
#' @return Integer item number or 0 if none
#' @export
next_matching <- function(delta, charnum, values, strict = TRUE, with_extrval = TRUE) {
  delta$next_matching(charnum, as.numeric(values), strict, with_extrval)
}

#' Check if item matches values
#'
#' @param delta A DeltaParser object
#' @param itemnum Item number
#' @param charnum Character number
#' @param values Numeric vector of values
#' @param strict Logical; strict comparison
#' @param with_extrval Logical; include extreme values
#' @return Logical indicating if item matches
#' @export
matches <- function(delta, itemnum, charnum, values, strict = TRUE, with_extrval = TRUE) {
  delta$matches(itemnum, charnum, as.numeric(values), strict, with_extrval)
}

#' Find all matching items
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @param values Numeric vector of values
#' @param strict Logical; strict comparison
#' @param with_extrval Logical; include extreme values
#' @return Integer vector of matching item numbers
#' @export
find_matching <- function(delta, charnum, values, strict = TRUE, with_extrval = TRUE) {
  delta$find_all_matching(charnum, as.numeric(values), strict, with_extrval)
}

#------------------------------------------------------------------------------
# Specifications Methods (EXPORTED)
#------------------------------------------------------------------------------

#' Check if specifications are available
#'
#' @param delta A DeltaParser object
#' @return Logical
#' @export
has_specifications <- function(delta) {
  delta$has_specifications()
}

#' Get specifications filename
#'
#' @param delta A DeltaParser object
#' @return Character string with the filename
#' @export
specs_filename <- function(delta) {
  delta$get_specs_filename()
}

#' Check if specifications are parsed
#'
#' @param delta A DeltaParser object
#' @return Logical
#' @export
is_specs_parsed <- function(delta) {
  delta$is_specs_parsed()
}

#' Set specifications filename
#'
#' @param delta A DeltaParser object
#' @param fname New filename
#' @param parse Logical, parse the file immediately
#' @export
set_specs_file <- function(delta, fname, parse = TRUE) {
  delta$set_specs_filename(fname, parse)
}

#' Parse specifications file
#'
#' @param delta A DeltaParser object
#' @return Logical indicating success
#' @export
parse_specs <- function(delta) {
  delta$parse_specs()
}

#' Get implicit value
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @param iv_type Implicit value type (1 or 2)
#' @return Integer implicit value
#' @export
implicit_value <- function(delta, charnum, iv_type = 1) {
  delta$get_implicit_value(charnum, iv_type)
}

#' Get all implicit values
#'
#' @param delta A DeltaParser object
#' @param iv_type Implicit value type (1 or 2)
#' @return Integer vector of implicit values
#' @export
implicit_values <- function(delta, iv_type = 1) {
  delta$get_all_implicit_values(iv_type)
}

#' Get number of dependent characters
#'
#' @param delta A DeltaParser object
#' @param ccnum Control character number
#' @param ccstate Control character state
#' @return Integer count of dependent characters
#' @export
dependent_count <- function(delta, ccnum, ccstate) {
  delta$get_depchar_nb(ccnum, ccstate)
}

#' Get all dependent characters
#'
#' @param delta A DeltaParser object
#' @param ccnum Control character number
#' @param ccstate Control character state
#' @return Integer vector of dependent character numbers
#' @export
dependent_characters <- function(delta, ccnum, ccstate) {
  delta$get_all_depchar(ccnum, ccstate)
}

#' Check if character is dependent
#'
#' @param delta A DeltaParser object
#' @param dcnum Dependent character number
#' @param ccnum Control character number
#' @param ccstate Control character state
#' @return Logical indicating if dependent
#' @export
is_dependent <- function(delta, dcnum, ccnum, ccstate) {
  delta$is_dependent(dcnum, ccnum, ccstate)
}

#' Get specifications debug output
#'
#' @param delta A DeltaParser object
#' @return Character string with debug output
#' @export
get_specs_debug <- function(delta) {
  delta$get_specs_debug()
}

#------------------------------------------------------------------------------
# Specifications Methods - C++ Style (EXPORTED for compatibility)
#------------------------------------------------------------------------------

#' Get specifications filename (C++ style)
#'
#' @param delta A DeltaParser object
#' @return Character string with the filename
#' @export
get_specs_filename <- function(delta) {
  delta$get_specs_filename()
}

#' Set specifications filename (C++ style)
#'
#' @param delta A DeltaParser object
#' @param fname New filename
#' @param parse Logical, parse the file immediately
#' @export
set_specs_filename <- function(delta, fname, parse = TRUE) {
  delta$set_specs_filename(fname, parse)
}

#' Get implicit value (C++ style)
#'
#' @param delta A DeltaParser object
#' @param charnum Character number
#' @param iv_type Implicit value type (1 or 2)
#' @return Integer implicit value
#' @export
get_implicit_value <- function(delta, charnum, iv_type = 1) {
  delta$get_implicit_value(charnum, iv_type)
}

#' Get number of dependent characters (C++ style)
#'
#' @param delta A DeltaParser object
#' @param ccnum Control character number
#' @param ccstate Control character state
#' @return Integer count of dependent characters
#' @export
get_depchar_nb <- function(delta, ccnum, ccstate) {
  delta$get_depchar_nb(ccnum, ccstate)
}

#' Get dependent character by rank (C++ style)
#'
#' @param delta A DeltaParser object
#' @param ccnum Control character number
#' @param ccstate Control character state
#' @param rank Rank of dependent character
#' @return Integer dependent character number
#' @export
get_depchar <- function(delta, ccnum, ccstate, rank = 1) {
  delta$get_depchar(ccnum, ccstate, rank)
}

#------------------------------------------------------------------------------
# Debug Methods - retrieve_all (matching C++ original)
#------------------------------------------------------------------------------

#' Retrieve all character information for debugging
#'
#' This matches the C++ retrieve_all() method from the original tDelta library.
#'
#' @param delta A DeltaParser object
#' @return Character string with debug information
#' @export
retrieve_all_chars <- function(delta) {
  delta$retrieve_all_chars()
}

#' Retrieve all item information for debugging
#'
#' This matches the C++ retrieve_all() method from the original tDelta library.
#'
#' @param delta A DeltaParser object
#' @return Character string with debug information
#' @export
retrieve_all_items <- function(delta) {
  delta$retrieve_all_items()
}

#' Retrieve all specifications information for debugging
#'
#' This matches the C++ retrieve_all() method from the original tDelta library.
#'
#' @param delta A DeltaParser object
#' @return Character string with debug information
#' @export
retrieve_all_specs <- function(delta) {
  delta$retrieve_all_specs()
}

#' Retrieve ALL information for debugging
#'
#' This matches the C++ retrieve_all() method from the original tDelta library,
#' combining characters, items, and specifications output.
#'
#' @param delta A DeltaParser object
#' @return Character string with all debug information
#' @export
retrieve_all <- function(delta) {
  delta$retrieve_all()
}

#------------------------------------------------------------------------------
# Utility Functions (EXPORTED)
#------------------------------------------------------------------------------

#' Parse a DELTA attribute string
#'
#' @param attr_string A DELTA attribute string
#' @return A list with parsed components
#' @export
parse_attribute <- function(attr_string) {
  parse_delta_attribute(attr_string)
}

#' Remove comments from a DELTA string
#'
#' @param src A string with DELTA comments
#' @return String with all comments removed
#' @export
strip_comments <- function(src) {
  strip_delta_comments(src)
}

#' Get character type constants
#'
#' @return Named integer vector of DELTA character types
#' @export
delta_char_types <- function() {
  delta_char_types()
}

#' Get package version
#'
#' @return Package version string
#' @export
delta_version <- function() {
  as.character(packageVersion("RDelta"))
}

#' Get version of the underlying C++ library
#'
#' @param delta A DeltaParser object
#' @return Version string
#' @export
get_version <- function(delta) {
  delta$get_version()
}

#' Get all debug output
#'
#' @param delta A DeltaParser object
#' @return Character string with all debug output
#' @export
debug_all <- function(delta) {
  delta$get_all_debug()
}

#------------------------------------------------------------------------------
# Test Helper Functions (for internal use)
#------------------------------------------------------------------------------

#' Get example file paths for testing
#'
#' @param dataset Character string: "viola" or "grass"
#' @return List with file paths
#' @export
get_example_files <- function(dataset = "viola") {
  if (dataset == "viola") {
    list(
      chars = system.file("extdata", "chars_viola", package = "RDelta"),
      items = system.file("extdata", "items_viola", package = "RDelta"),
      specs = system.file("extdata", "specs_viola", package = "RDelta")
    )
  } else if (dataset == "grass") {
    list(
      chars = system.file("extdata", "chars_grass", package = "RDelta"),
      items = system.file("extdata", "items_grass", package = "RDelta"),
      specs = system.file("extdata", "specs_grass", package = "RDelta")
    )
  } else {
    stop("Unknown dataset: ", dataset)
  }
}