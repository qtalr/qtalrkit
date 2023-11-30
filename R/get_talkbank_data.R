#' Downloads TalkBank data and saves it to disk
#'
#' Downloads the utterances, transcripts, participants, tokens, and token types data
#' from the TalkBank database and saves it to disk in the specified target directory.
#'
#' @param corpus_name The name of the TalkBank corpus to download data from.
#' @param corpus_path The path to the TalkBank corpus to download data from.
#' @param target_dir The directory to save the downloaded data to.
#' @param force If `TRUE`, the data will be downloaded even if it already exists on disk.
#' @param confirmed If `TRUE`, the user has confirmed that they have permission to use the data.
#' If `FALSE`, the function will prompt the user to confirm permission.
#' Setting this to `TRUE` is useful for reproducible workflows.
#'
#' @return A message indicating whether the data was acquired or already existed on disk.
#'
#' @importFrom TBDBr getUtterances getTranscripts getParticipants getTokens getTokenTypes
#' @importFrom tidyr unnest
#' @importFrom dplyr everything
#' @importFrom readr write_csv
#' @importFrom fs dir_exists dir_create file_exists path
#'
#' @examples
#' \dontrun{
#' # Download CABNC data from the Conversation Bank to a directory called "data"
#' get_talkbank_data(corpus_name = "ca", corpus_path = c("ca", "CABNC"), target_dir = "data")
#' }
#' @export
get_talkbank_data <- function(corpus_name, corpus_path, target_dir, force = FALSE, confirmed = FALSE) {
  # Confirm that the user has permission to use the data
  if (!confirmed) {
    confirmed <- confirm_permission()
  }
  # If not, stop the script
  if (!confirmed) {
    return(message("Aborted."))
  }
  # Check if TBDBr path is valid
  if (!TBDBr::validPath(c(corpus_name, corpus_path))) {
    return(message("Invalid path to TalkBank Database. Check `corpus_name` and `corpus_path` again and try again."))
  }
  # Check if the target directory exists
  if (!fs::dir_exists(target_dir)) {
    # Create target directory
    fs::dir_create(target_dir)
  }
  # Set up file paths names
  utterances_file <- fs::path(target_dir, "utterances.csv")
  transcripts_file <- fs::path(target_dir, "transcripts.csv")
  participants_file <- fs::path(target_dir, "participants.csv")
  tokens_file <- fs::path(target_dir, "tokens.csv")
  token_types_file <- fs::path(target_dir, "token_types.csv")
  # Check if the file doesn't exist
  # if not, acquire and write it to disk
  if (!fs::file_exists(utterances_file) || force) {
    TBDBr::getUtterances(corpusName = corpus_name, corpora = corpus_path) |>
      tidyr::unnest(cols = dplyr::everything()) |>
      readr::write_csv(utterances_file)
    message("Acquired: ", utterances_file)
  } else {
    message("Already acquired: ", utterances_file)
  }
  if (!fs::file_exists(transcripts_file) || force) {
    TBDBr::getTranscripts(corpusName = corpus_name, corpora = corpus_path) |>
      tidyr::unnest(cols = dplyr::everything()) |>
      readr::write_csv(transcripts_file)
    message("Acquired: ", transcripts_file)
  } else {
    message("Already acquired: ", transcripts_file)
  }
  if (!fs::file_exists(participants_file) || force) {
    TBDBr::getParticipants(corpusName = corpus_name, corpora = corpus_path) |>
      tidyr::unnest(cols = dplyr::everything()) |>
      readr::write_csv(participants_file)
    message("Acquired: ", participants_file)
  } else {
    message("Already acquired: ", participants_file)
  }
  if (!fs::file_exists(tokens_file) || force) {
    TBDBr::getTokens(corpusName = corpus_name, corpora = corpus_path) |>
      tidyr::unnest(cols = dplyr::everything()) |>
      readr::write_csv(tokens_file)
    message("Acquired: ", tokens_file)
  } else {
    message("Already acquired: ", tokens_file)
  }
  if (!fs::file_exists(token_types_file) || force) {
    TBDBr::getTokenTypes(corpusName = corpus_name, corpora = corpus_path) |>
      tidyr::unnest(cols = dplyr::everything()) |>
      readr::write_csv(token_types_file)
    message("Acquired: ", token_types_file)
  } else {
    message("Already acquired: ", token_types_file)
  }
  message("Acquisition complete.")
  message("Use `force = TRUE` to re-acquire.")
}
