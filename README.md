# DeltaR
Parse and Query DELTA-Format Taxonomic Descriptions.

Provides an R interface to the tDelta C++ library for parsing     DELTA (***DE**scription **L**anguage for **TA**xonomy*) format files. Supports loading character lists, item descriptions, and specifications, with efficient searching and matching of items based on character values. It also supports the generation of indentification keys, diagnoses, natural-language descriptions,    and clustering and ordination analyses by means of the DELTA programs CONFOR, KEY, and DIST.



## Installation

Installing from CRAN:

`install.packages("DeltaR")`

Installing fro GitHub:

`remotes::install_github("maurobio/deltar")`

Some of the package functions assumes that the DELTA system CONFOR, KEY, or DIST program executables are available in the system PATH or in the current working directory. Get these programs from here: https://github.com/maurobio/freedelta/tree/main/classic_delta

## Examples

#### Load a DELTA dataset:

`library(DeltaR)`
`delta <- load_delta("chars", "items", "specs")```

#### List characters:

`cat("\nCHARACTERS LIST:\n")`
`for (i in 1:Example$get_chars_nb()) {`
	`cat(i, " : ", Example$get_char_feature(i), "\n")`
`}`
`cat("*** ", Example$get_chars_nb(), " characters ***\n")`

#### List items:

`cat("\nITEMS LIST:\n")`
`for (i in 1:Example$get_items_nb()) {`
	`cat(i, " : ", Example$get_item_name(i), "\n")`
`}`
`cat("*** ", Example$get_items_nb(), " items ***\n")`

#### Create a dichotomous key (with default parameters):

`key(delta)`

`natlan(delta)`

Create a natural-language description:

`natlan(delta)`