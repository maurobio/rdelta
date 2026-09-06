## ----include = FALSE----------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  eval = FALSE
)

## ----test_rdelta, echo=TRUE, eval=FALSE---------------------------------------------------------------
# 
# library(RDelta)
# 
# #--- Get command line arguments ----------------------------------------------
# # This works in both interactive and non-interactive modes
# get_args <- function() {
#   # Get all command line arguments
#   args <- commandArgs(trailingOnly = TRUE)
# 
#   # If running interactively and no arguments were provided,
#   # prompt the user for file names
#   if (interactive() && length(args) == 0) {
#     cat("\nNo command-line arguments provided.\n")
#     cat("Please enter file names (or press Enter to use defaults):\n\n")
# 
#     chars <- readline(prompt = "Characters file [chars]: ")
#     if (chars == "") chars <- "chars"
# 
#     items <- readline(prompt = "Items file [items]: ")
#     if (items == "") items <- "items"
# 
#     specs <- readline(prompt = "Specifications file [specs] (optional): ")
#     if (specs == "") specs <- NULL
# 
#     return(list(chars = chars, items = items, specs = specs))
#   }
# 
#   # Non-interactive mode or arguments provided
#   if (length(args) >= 2) {
#     chars <- args[1]
#     items <- args[2]
#     specs <- if (length(args) >= 3) args[3] else NULL
#   } else {
#     # Default files
#     chars <- "chars"
#     items <- "items"
#     specs <- "specs"
#   }
# 
#   return(list(chars = chars, items = items, specs = specs))
# }
# 
# #--- Get file names ----------------------------------------------------------
# files <- get_args()
# 
# chars_file <- files$chars
# items_file <- files$items
# specs_file <- files$specs
# 
# #--- Creates CharList, ItemList and Specs objects and parses the
# #    corresponding text files ----------------------------------------------
# if (!is.null(specs_file)) {
#   Example <- load_delta(chars_file, items_file, specs_file)
# } else {
#   Example <- load_delta(chars_file, items_file)
# }
# 
# if (!(Example$is_parsed() && Example$is_items_parsed())) {
#   stop("Error parsing characters and/or items description files")
# }
# if (Example$has_specifications() && !Example$is_specs_parsed()) {
#   stop("Error parsing specifications file")
# }
# 
# #--- Displays header -------------------------------------------------------
# cat("==================\n")
# cat("Free Delta Project\n")
# cat("==================\n")
# cat("Test of RDelta package\n\n")
# 
# stop <- FALSE
# while (!stop) {
#   #--- Menu ----------------------------------------------------------------
#   cat("\n")
#   cat("Test Options:\n")
#   cat("1. Character list\n")
#   cat("2. Retrieve a character\n")
#   cat("3. Item list\n")
#   cat("4. Retrieve an item\n")
#   cat("5. Item/character-value matching\n")
#   cat("6. Identification\n")
#   cat("7. Specifications\n")
#   cat("8. Retrieve all (debugging)\n")
#   cat("9. Exit\n")
#   opt <- readline(prompt = "Your choice: ")
#   opt <- as.integer(opt)
# 
#   if (is.na(opt)) {
#     cat("Invalid input. Please enter a number.\n")
#     next
#   }
# 
#   if (opt == 1) {
#     #----- Displays all characters features -----
#     cat("\nCHARACTERS LIST:\n")
#     for (i in 1:Example$get_chars_nb()) {
#       cat(i, " : ", Example$get_char_feature(i), "\n")
#     }
#     cat("*** ", Example$get_chars_nb(), " characters ***\n")
# 
#   } else if (opt == 2) {
#     #----- Displays information about one character -----
#     num <- readline(prompt = "Enter character number to retrieve: ")
#     num <- as.integer(num)
#     if (is.na(num) || (num < 1) || (num > Example$char_count())) {
#       cat("Invalid character number\n")
#     } else {
#       #--- Character type
#       cat("\nType : ")
#       ct <- Example$get_char_type(num)
#       if (ct == 2) {
#         cat("unordered multistate\n")
#       } else if (ct == 3) {
#         cat("ordered multistate\n")
#       } else if (ct == 4) {
#         cat("integer numeric\n")
#       } else if (ct == 5) {
#         cat("real numeric\n")
#       } else if (ct == 8) {
#         cat("text/comment\n")
#       }
# 
#       #--- Feature
#       cat("Feature : ", Example$get_char_feature(num), "\n")
# 
#       #--- State list or unit
#       if (ct == 2 || ct == 3) {  # CT_UM or CT_OM
#         for (j in 1:Example$get_states_nb(num)) {
#           cat("State ", j, " : ", Example$get_state(num, j), "\n")
#         }
#       } else if (ct == 4 || ct == 5) {  # CT_IN or CT_RN
#         cat("Unit : ", Example$get_char_unit(num), "\n")
#       }
#     }
# 
#   } else if (opt == 3) {
#     #----- Displays all items names -----
#     cat("\nITEMS LIST:\n")
#     for (i in 1:Example$get_items_nb()) {
#       cat(i, " : ", Example$get_item_name(i), "\n")
#     }
#     cat("*** ", Example$get_items_nb(), " items ***\n")
# 
#   } else if (opt == 4) {
#     #----- Displays information about an item -----
#     num <- readline(prompt = "Enter item number to retrieve: ")
#     num <- as.integer(num)
#     if (is.na(num) || (num < 1) || (num > Example$item_count())) {
#       cat("Invalid item number\n")
#     } else {
#       # Name
#       cat("\nItem name :\n")
#       cat("  ", Example$get_item_name(num), "\n")
#       # Attributes
#       cat("Attributes :\n")
#       for (j in 1:Example$get_attributes_nb(num)) {
#         cat("  ", Example$get_attribute(num, j), "\n")
#       }
#     }
# 
#   } else if (opt == 5) {
#     #----- Matching item with character value -----
#     n1 <- readline(prompt = "Enter item number: ")
#     n1 <- as.integer(n1)
#     if (is.na(n1) || (n1 < 1) || (n1 > Example$get_items_nb())) {
#       cat("Invalid item number\n")
#       next
#     }
# 
#     n2 <- readline(prompt = "Enter character number: ")
#     n2 <- as.integer(n2)
#     if (is.na(n2) || (n2 < 1) || (n2 > Example$get_chars_nb())) {
#       cat("Invalid character number\n")
#       next
#     }
# 
#     x1 <- readline(prompt = "Enter character state/value: ")
#     x1 <- as.numeric(x1)
# 
#     if (is.na(x1)) {
#       cat("Invalid value\n")
#       next
#     }
# 
#     # Display item name
#     cat("\nItem ", Example$get_item_name(n1), " is ")
# 
#     # Compare and display result - use matches_default for simple comparison
#     if (!Example$matches_default(n1, n2, x1)) {
#       cat("not ")
#     }
#     cat("matching with\n")
# 
#     # Display character and state/value
#     cat("character : ", Example$get_char_feature(n2), "\n")
#     cat("state/value : ", x1)
#     ct <- Example$get_char_type(n2)
#     if (ct == 2 || ct == 3) {  # CT_UM or CT_OM
#       cat(" ", Example$get_state(n2, as.integer(x1)), "\n")
#     } else if (ct == 4 || ct == 5) {  # CT_IN or CT_RN
#       cat(" ", Example$get_char_unit(n2), "\n")
#     }
#     cat("\n")
# 
#   } else if (opt == 6) {
#     #----- Item identification -----
#     # Check if identification.R exists
#     if (file.exists("identification.R")) {
#       source("identification.R")
#       identification(Example)
#     } else {
#       cat("Error: identification.R file not found\n")
#     }
# 
#   } else if (opt == 7) {
#     #----- Displays specification file content -----
#     if (Example$has_specifications()) {
#       cat("\nSPECIFICATIONS:\n")
#       cat(Example$retrieve_all_specs())
#     } else {
#       cat("No specifications file\n")
#     }
# 
#   } else if (opt == 8) {
#     #----- Displays all information stored after parsing (debugging) -----
#     cat("\n========================================\n")
#     cat("     FULL PACKAGE INFORMATION\n")
#     cat("========================================\n\n")
#     cat(Example$retrieve_all())
# 
#   } else if (opt == 9) {
#     #----- Exit -----
#     stop <- TRUE
#     break
#   }
# }
# 
# cat("\nTest completed.\n")

## ----identification, echo=TRUE, eval=FALSE------------------------------------------------------------
# 
# message_identification <- function() {
#   cat("\nIDENTIFICATION\n")
#   cat("This function demonstrates the possibility using RDelta package for taxa\n")
#   cat("identification.\n")
#   cat("In this example, the identification process has been simplified a lot.\n")
#   cat("It is based on successive comparisons between observed characters values,\n")
#   cat("and the items descriptions stored in the Delta files.\n")
#   cat("Advanced features, such as values combinations, implicit values, dependant\n")
#   cat("characters are not implemented. There is no characters selection during\n")
#   cat("identification (like the 'best' command in Intkey).\n")
#   cat("Of course, the user interface is very simple, and should be improved in\n")
#   cat("a public release.\n\n")
#   cat("Usage :\n")
#   cat("To make an identification, you should have a printed version of characters\n")
#   cat("description (Delta chars file).\n")
#   cat("Select a character and a value according to the taxon you want to identify.\n")
#   cat("After each selection, the program displays all characters values already\n")
#   cat("selected, and the list of remaining taxa.\n")
#   cat("To cancel a previous selected character, select it again, and input the\n")
#   cat("UNKNOWN value (-1). You can also modify a previously selected value.\n\n")
# }
# 
# identification <- function(delta) {
#   #----- Initialisation -----
#   nbval <- RDelta::get_chars_nb(delta)
#   nbitm <- RDelta::get_items_nb(delta)
# 
#   tabval <- rep(NA, nbval)
#   tabitm <- rep(1, nbitm)
# 
#   message_identification()
# 
#   #----- User input and search matching items -----
#   while (TRUE) {
#     #--- Input character number and value
#     cat("Select a character describing the item to identify :\n")
#     cat("  Enter character number (0=exit) : ")
#     charnum <- as.integer(readline())
# 
#     if (is.na(charnum) || charnum == 0) break
# 
#     if (charnum < 1 || charnum > nbval) {
#       cat("  Invalid character number\n")
#       next
#     }
# 
#     cat("  Enter character state/value (-1=unknown) : ")
#     charval <- as.numeric(readline())
# 
#     if (is.na(charval)) {
#       cat("  Invalid value, please enter a number\n")
#       next
#     }
# 
#     #--- Stores value
#     tabval[charnum] <- if (charval == -1) NA else charval
# 
#     #--- Update items table considering selected values
#     for (i in 1:nbitm) {
#       for (j in 1:nbval) {
#         if (is.na(tabval[j])) {
#           tabitm[i] <- 1
#         } else {
#           tabitm[i] <- as.integer(RDelta::matches(delta, i, j, tabval[j]))
#         }
#         if (tabitm[i] == 0) break
#       }
#     }
# 
#     #--- Displays selected characters and values, and remaining items list
#     cat("\nCHARACTERS SELECTION\n")
#     for (j in 1:nbval) {
#       if (!is.na(tabval[j])) {
#         cat(j, ". ", RDelta::get_char_feature(delta, j), "\n", sep = "")
#         cat("    state/value : ", tabval[j], sep = "")
# 
#         if (RDelta::get_char_type(delta, j) == 2) {
#           cat(" - ", RDelta::get_state(delta, j, as.integer(tabval[j])), "\n", sep = "")
#         } else if (RDelta::get_char_type(delta, j) == 4) {
#           cat(" ", RDelta::get_char_unit(delta, j), "\n", sep = "")
#         } else {
#           cat("\n")
#         }
#       }
#     }
# 
#     cat("ITEMS REMAINING\n")
#     for (i in 1:nbitm) {
#       if (tabitm[i] == 1) {
#         cat(delta$get_item_name(i), "\n")
#       }
#     }
#     cat("\n")
#   }
# }
# 
# #==============================================================================
# # Example usage (commented out):
# #
# # library(RDelta)
# # delta <- load_delta("chars_viola", "items_viola")  # read DELTA files
# # identification(delta)
# #==============================================================================

