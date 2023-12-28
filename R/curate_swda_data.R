#' Curate SWDA Data
#'
#' This function curates the SWDA (Switchboard Dialog Act) data by processing
#' all the .utt files in the specified directory.
#'
#' @param dir_path The path to the directory containing the .utt files.
#'
#' @return A data frame containing the curated SWDA data.
#'
#' @examples
#' \dontrun{
#' curate_swda_data("/path/to/directory")
#' }
#' @importFrom purrr map_dfr
#' @importFrom tibble as_tibble
#'
#' @export
curate_swda_data <- function(dir_path) {
  files <-
    list.files(
      path = dir_path,
      pattern = ".utt",
      full.names = TRUE,
      recursive = TRUE
    )

  data_df <-
    files |>
    purrr::map_dfr(curate_swda_file, .progress = TRUE) |>
    as_tibble()

  return(data_df)
}
