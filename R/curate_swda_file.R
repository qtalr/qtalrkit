#' Extract SWDA data from a file
#'
#' This function reads a file containing SWDA data and extracts relevant information such as document ID, speaker IDs, and text.
#'
#' @param file The path to the file containing SWDA data.
#'
#' @return A data frame containing the extracted SWDA data, including document ID, speaker IDs, damsl tags, speaker turns, utterance numbers, and utterance text.
#'
#' @examples
#' \dontrun{
#' file <- "/path/to/swda_data.txt"
#' swda_data <- extract_swda_data(file)
#' head(swda_data)
#' }
curate_swda_file <- function(file) {
  # Read `file` by lines
  doc_chr <- readLines(file)

  # Extract `doc_id`, `speaker_a_id`, and `speaker_b_id`
  speaker_info_chr <- unlist(strsplit(sub(".*\\t", "", grep("\\d+_\\d+_\\d+", doc_chr, value = TRUE)), "_"))

  doc_id <- speaker_info_chr[1]
  speaker_a_id <- speaker_info_chr[2]
  speaker_b_id <- speaker_info_chr[3]

  # Extract `text`
  text_start_index <- grep("={3,}", doc_chr) + 1
  text_end_index <- length(doc_chr)

  text <- grep(".+", trimws(doc_chr[text_start_index:text_end_index]), value = TRUE)

  swda_df <- data.frame(doc_id = doc_id, text = text, stringsAsFactors = FALSE)

  # Extract column information from `text`
  swda_df$damsl_tag <- trimws(sub("^(.+?)\\s.*$", "\\1", swda_df$text))
  swda_df$speaker_turn <- sub("^.*([AB]\\.\\d+).*$", "\\1", swda_df$text)
  swda_df$utterance_num <- sub("^.*utt(\\d+).*$", "\\1", swda_df$text)
  swda_df$utterance_text <- trimws(sub("^.*:(.+)$", "\\1", swda_df$text))


  # Separate speaker_turn into distinct columns
  speaker_turn_split <- strsplit(swda_df$speaker_turn, "\\.")
  swda_df$speaker <- sapply(speaker_turn_split, "[[", 1)
  swda_df$turn_num <- sapply(speaker_turn_split, "[[", 2)

  # Link speaker with speaker_id
  swda_df$speaker_id <- ifelse(swda_df$speaker == "A", speaker_a_id, speaker_b_id)

  # Select relevant columns
  swda_df <- swda_df[, c("doc_id", "damsl_tag", "speaker_id", "speaker", "turn_num", "utterance_num", "utterance_text")
  ]
  return(swda_df)
}
