#' Calculate Type Metrics for Text Data
#'
#' This function calculates type metrics for tokenized text data.
#'
#' @param data A data frame containing the tokenized text data
#' @param type The variable in `data` that contains the type
#'             (e.g., term, lemma) to analyze.
#' @param documents The variable in `data` that contains the document IDs.
#' @param frequency A character vector indicating which
#'                  frequency metrics to use. If NULL (default),
#'                  only the `type` and `n` are returned.
#'                  Other options:
#'                  'all',
#'                  'rf' calculates relative frequency,
#'                  'orf' calculates observed relative frequency.
#'                  Can specify multiple options: c("rf", "orf").
#' @param dispersion A character vector indicating which
#'                   dispersion metrics to use. If NULL (default),
#'                   only the `type` and `n` are returned.
#'                   Other options:
#'                  'all',
#'                  'df' calculates Document Frequency.
#'                  'idf' calculates Inverse Document Frequency.
#'                  'dp' calculates Gries' Deviation of Proportions.
#'                  Can specify multiple options: c("df", "idf").
#'
#' @return A data frame with columns:
#'   - `type`: The unique types from the input data.
#'   - `n`: The frequency of each type across all documents.
#'   Optionally (based on the `frequency` and `dispersion` arguments):
#'   - `rf`: The relative frequency of each type across all documents.
#'   - `orf`: The observed relative frequency (per 100) of each
#'            type across all documents.
#'   - `df`: The document frequency of each type.
#'   - `idf`: The inverse document frequency of each type.
#'   - `dp`: Gries' Deviation of Proportions of each type.
#'
#' @export
#'
#' @references
#' Gries, Stefan Th. (2023). Statistical Methods in Corpus Linguistics.
#' In Readings in Corpus Linguistics: A Teaching and Research Guide
#' for Scholars in Nigeria and Beyond, pp. 78-114.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   term = c("word1", "word1", "word2", "word2", "word2", "word3"),
#'   documents = c("doc1", "doc2", "doc1", "doc1", "doc2", "doc2")
#' )
#' calc_type_metrics(
#'   data = data,
#'   type = term,
#'   documents = documents,
#'   frequency = c("rf", "orf"),
#'   dispersion = c("df", "idf")
#' )
#' }
#'
#' @importFrom rlang enquo quo_is_missing quo_is_symbol
#' @importFrom dplyr count
#' @importFrom tidytext cast_sparse
#' @importFrom tibble tibble
calc_type_metrics <- function(data, type, documents, frequency = NULL, dispersion = NULL) { # nolint
  # Check if data is a data.frame
  if (!is.data.frame(data)) {
    stop("The argument 'data' must be a data frame.")
  }

  # Check if type and documents exist in data
  if (!all(c(deparse(substitute(type)), deparse(substitute(documents))) %in% names(data))) {
    stop("The variables specified in 'type' and 'documents' must exist in 'data'.")
  }

  # If frequency is not NULL, check if it's a character vector and if all of its values are allowed
  if (!is.null(frequency) && (!is.character(frequency) || !all(frequency %in% c("all", "rf", "orf")))) {
    stop("The argument 'frequency' must be a character vector containing any combination of: 'all', 'rf', 'orf'")
  }

  # If dispersion is not NULL, check if it's a character vector and if all of its values are allowed
  if (!is.null(dispersion) && (!is.character(dispersion) || !all(dispersion %in% c("all", "df", "idf", "dp")))) {
    stop("The argument 'dispersion' must be a character vector containing any combination of: 'all', 'df', 'idf', or 'dp'.")
  }

  # Create a Sparse Term-Document Matrix (TDM)
  tdm <-
    data |>
    dplyr::count({{ type }}, {{ documents }}) |>
    tidytext::cast_sparse({{ type }}, {{ documents }}, n)

  # Convert frequencies to row proportions
  row_sums <- Matrix::rowSums(tdm)
  tdm_normalized <- tdm / row_sums

  # Calculate the proportion of each document in the corpus
  col_sums <- Matrix::colSums(tdm)
  corpus_parts <- col_sums / sum(tdm)

  # Initialize an empty data frame
  output_df <- tibble(type = rownames(tdm), n = row_sums)
  
  # Calculate metrics based on user choice

  # Relative Frequency (RF)
  if ("all" %in% frequency || "rf" %in% frequency) {
    output_df$rf <- row_sums / sum(tdm)
  }

  # Observed Relative Frequency (ORF)
  if ("all" %in% frequency || "orf" %in% frequency) {
    output_df$orf <- (row_sums / sum(tdm)) * 100
  }

  # Document Frequency (DF)
  if ("all" %in% dispersion || "df" %in% dispersion) {
    output_df$df <- calc_df(tdm)
  }

  # Inverse Document Frequency (IDF)
  if ("all" %in% dispersion || "idf" %in% dispersion) {
    output_df$idf <- calc_idf(tdm)
  }

  # Gries' Deviation of Proportions (DP)
  if ("all" %in% dispersion || "dp" %in% dispersion) {
    output_df$dp <- calc_dp(tdm_normalized, corpus_parts)
  }
  return(output_df)
}
