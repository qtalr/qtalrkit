#' Retrieves Gutenberg works based on specified criteria and saves the data to a CSV file.
#'
#' @param target_dir The directory where the CSV file will be saved.
#' @param lcc_subject A character vector specifying the Library of Congress Classification (LCC) subjects to filter the works.
#' @param birth_year An optional integer specifying the minimum birth year of authors to include.
#' @param death_year An optional integer specifying the maximum death year of authors to include.
#' @param force A logical value indicating whether to overwrite existing data if it already exists.
#' @param confirmed A logical value indicating whether to skip the confirmation prompt if the number of works is greater than 1000.
#'
#' @return None
#'
#' @details This function retrieves Gutenberg works based on the specified LCC subjects and optional author birth and death years.
#' It checks if the data already exists in the target directory and provides an option to overwrite it.
#' The function also creates the target directory if it doesn't exist.
#' If the number of works is greater than 1000 and the 'confirmed' parameter is not set to TRUE, it prompts the user for confirmation.
#' The retrieved works are filtered based on public domain rights in the USA and availability of text.
#' The resulting works are downloaded and saved as a CSV file in the target directory.
#'
#' For more information on Library of Congress Classification (LCC) subjects, refer to the \href{https://www.loc.gov/catdir/cpso/lcco/}{Library of Congress Classification Guide}.
#'
#' @examples
#' \dontrun{
#' # Retrieve works with LCC subject "Political Theory" and save to "/path/to/works_fiction.csv"
#' get_gutenberg_works("/path/to", "JC")
#' }
#' @import gutenbergr
#' @importFrom dplyr filter
#'
#' @export
get_gutenberg_works <- function(target_dir, lcc_subject, birth_year = NULL, death_year = NULL, force = FALSE, confirmed = FALSE) {
  # Load packages
  require(gutenbergr)
  # Function implementation
}
get_gutenberg_works <- function(target_dir, lcc_subject, birth_year = NULL, death_year = NULL, force = FALSE, confirmed = FALSE) {
  # Parameter validation
  if (is.null(lcc_subject) || length(lcc_subject) == 0) {
    stop("lcc_subject must be provided and non-empty.")
  }

  # Create path to target_file from target_dir and lcc_subject
  file_name <- paste0("works_", tolower(paste0(lcc_subject, collapse = "_")), ".csv")
  target_file <- file.path(target_dir, file_name)

  # Check to see if the data already exists
  if (file.exists(target_file) && !force) {
    message("Data already exists at ", target_file, "\nUse 'force = TRUE' to overwrite existing data.")
    return(invisible())
  }

  # Ensure the directory exists
  dir.create(path = target_dir, recursive = TRUE, showWarnings = FALSE)

  # Get authors within years
  authors <- gutenbergr::gutenberg_authors

  if (!is.null(birth_year)) {
    authors <- authors |> dplyr::filter(.data$birthdate > birth_year)
  }

  if (!is.null(death_year)) {
    authors <- authors |> dplyr::filter(.data$deathdate < death_year)
  }

  # Get LCC subjects
  subjects <- gutenbergr::gutenberg_subjects |>
    dplyr::filter(.data$subject_type == "lcc", .data$subject %in% lcc_subject)

  # Get works based on authors and subjects
  works <- gutenbergr::gutenberg_metadata |>
    dplyr::filter(
      .data$gutenberg_author_id %in% authors$gutenberg_author_id,
      .data$gutenberg_id %in% subjects$gutenberg_id
    )

  # Check the number of works if not confirmed
  if (!confirmed && nrow(works) > 1000) {
    repeat {
      response <- readline(prompt = "The number of works is greater than 1000. Do you want to continue? [y/n]: ")
      if (tolower(response) == "n") {
        message("Operation aborted by the user.\nYou can use the 'confirmed = TRUE' parameter to skip this prompt in the future.")
        return(invisible())
      } else if (tolower(response) == "y") {
        break
      } else {
        message("Please enter 'y' to continue or 'n' to abort.")
      }
    }
  }

  # Download works
  results <- works |>
    dplyr::filter(.data$rights == "Public domain in the USA.", .data$has_text == TRUE) |>
    gutenbergr::gutenberg_download(meta_fields = c("title", "author", "gutenberg_author_id", "gutenberg_bookshelf"))

  # Write works to disk
  readr::write_csv(results, file = target_file)
  message("Data saved to ", target_file)
}
