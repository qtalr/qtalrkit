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
