# DeltaR 0.1.0

## New Features
* Initial CRAN release
* Full support for DELTA character list files
* Full support for DELTA item description files  
* Support for DELTA specifications files
* Rcpp-based C++ integration for high performance
* Comprehensive R interface with data frames and lists
* Search and matching functionality for taxonomic identification

## Data Import
* `load_delta()` - Load DELTA files into R
* Support for both required (chars, items) and optional (specs) files

## Data Exploration
* `characters_df()` - View character definitions as data frame
* `items_df()` - View item information as data frame
* `get_item()` - Retrieve complete item data

## Search and Analysis
* `find_matching_items()` - Find items matching character values
* `item_matches()` - Test if an item matches specific values

## Utility Functions
* `parse_attribute()` - Parse DELTA attribute strings
* `strip_comments()` - Remove DELTA comments
* `delta_char_types()` - Get character type constants

## Example Data
* Viola (Viola spp.) dataset
* Grass (Poaceae) dataset

## Acknowledgements
Based on the tDelta C++ library by Denis Ziegler, part of the Free DELTA project.