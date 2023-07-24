#' Create data origin file
#'
#' This function creates a data frame with attributes about the origin of the data,
#' writes it to a CSV file at the specified file path, and returns the data frame.
#'
#' @param file_path A character string specifying the file path where the data origin file should be saved.
#' @return A tibble containing the data origin information.
#' @export
#'
#' @examples
#' \dontrun{
#' create_data_origin("data_origin.csv")
#' }
#'
#' @importFrom tibble tribble
#' @importFrom readr write_csv
create_data_origin <- function(file_path) {
  # Check to see if `file_path` is a character string
  if (!is.character(file_path)) stop("`file_path` must be a character string.")

  # Check to see if file exists at `file_path`
  if (file.exists(file_path)) {
    message("File already exists at `file_path`. Use `force = TRUE` to overwrite.") #nolint
    invisible(NULL) # end function early
  }

  # Create a data frame with the data origin information
  data_origin <-
    tibble::tribble(
      ~attribute, ~description,
      "Resource name", "The name of the resource.",
      "Data source", "URL, DOI, etc.",
      "Data sampling frame", "Language, language variety, modality, genre, etc.",
      "Data collection date(s)", "The dates the data was collected.",
      "Data format", ".txt, .csv, .xml, .html, etc.",
      "Data schema", "Relationships between data elements: files, folders, etc.",
      "License", "CC BY, CC BY-SA, etc.",
      "Attribution", "Citation information."
    )

  # Write the data origin to a file
  data_origin |> readr::write_csv(file = file_path)

  # Return message
  message("Data origin file created at `file_path`.")
}
