# qtalrkit 0.9.4

* A fix to `get_compressed_data()` to avoid the infinite loop that was caused by the confirmation prompt.

# qtalrkit 0.9.3

* updates `create_data_origin()` to use base R. It also adds functionality to optionally return the data origin as a data frame and to overwrite the file if it already exists.

# qtalrkit 0.9.2

* Adds `curate_enntt_data()` function to curate data from the Europarl Corpus of Native, Non-Native, and Translated Text (ENNTT).

* Updates recipes

# qtalrkit 0.9.1

* Changes mirror to "https://gutenberg.pglaf.org/" for `get_gutenberg_works()` function
* Organizes the output to include the LCC classification as part of the data written to disk for the `get_gutenberg_works()` function

# qtalrkit 0.9.0

* Updated version number to reflect that the package is now in beta
* Added `curate_swda_data()` function to curate data from the Switchboard Dialog Act Corpus

# qtalrkit 0.0.4.0

* Adds `get_gutenberg_works()` function to import data from Project Gutenberg

# qtalrkit 0.0.3.400

* Fixes warnings on `calc_assoc_metrics()`
* Updates Date on DESCRIPTION file
* Adds test-add_pkg_to_bib.R

# qtalrkit 0.0.3.000

* Adds `calc_assoc_metrics` to calculate (pmi, dice, G) for a given type bigram

# qtalrkit 0.0.3.210

* Removes `calc_dispersion_metrics()` function and replaces it with `calc_type_metric()` which includes frequency and dispersion metrics.

# qtalrkit 0.0.3.200

* Fixes bug in `get_compressed_data()` that caused the function to create dot file copies of the original files

# qtalrkit 0.0.3.100

* Adds `idf` measure to `calc_dispersion_metrics()`

# qtalrkit 0.0.3.000

* Adds `calc_dispersion_metrics()` function to calculate dispersion metrics

# qtalrkit 0.0.2.000

* Added `get_talkbank_data()` function to import data from TalkBank
* Added internal `confirm_permissions()` function to confirm that users are aware of the permissions required to use data
* Updated `get_*()` functions to use `confirm_permissions()` internally
* Changed `get_outliers()` to `find_outliers()` to be more consistent with other functions

# qtalrkit 0.0.1.9400

* Updated `create_data_dictionary()` to provide default scaffold structure for data dictionary, in lieu of OpenAI model. This scaffold is to be updated manually by the user.

# qtalrkit 0.0.1.9300

* Added `create_data_origin()` function. This creates a .csv file to scaffold a data origin file

# qtalrkit 0.0.1.9200

* Adds project template for RStudio: "Minimal Reproducible Project"

# qtalrkit 0.0.1.9100

* Adjusted `create_data_dictionary()` to produce results more in line with the QTALR textbook

# qtalrkit 0.0.1.9000

* Added `get_outliers()` function
* Added Instructor Guide

# qtalrkit 0.0.1.0000

* Added R tutorial 0

# qtalrkit 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
