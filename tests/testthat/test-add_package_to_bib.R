context("add_pkg_to_bib")

test_that("add_pkg_to_bib writes package name to bib file", {
  # Create a temporary file for testing
  bib_file <- tempfile()

  # Call the function with a package name
  add_pkg_to_bib("dplyr", bib_file)

  # Read the contents of the bib file
  bib_contents <- readLines(bib_file)

  # Check that the package name was written to the bib file
  expect_true("@Manual{R-dplyr," %in% bib_contents)

  # Clean up the temporary file
  unlink(bib_file)
})
