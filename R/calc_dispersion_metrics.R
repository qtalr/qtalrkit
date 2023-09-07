#' Calculate Dispersion Metrics for Text Data
#'
#' This function calculates dispersion metrics for tokenized text data.
#'
#' @param data A data frame containing the tokenized text data
#' @param type The variable in `data` that contains the type (e.g., term, lemma) to analyze.
#' @param documents The variable in `data` that contains the document IDs.
#' @param metric A character string indicating which metric to calculate:
#'               'all' (default), 'df', or 'dp'.
#'               'df' calculates Document Frequency.
#'               'dp' calculates Gries' Deviation of Proportions.
#'
#' @return A data frame with columns:
#'   - `type`: The unique types from the input data.
#'   - `n`: The frequency of each type across all documents.
#'   - `df`: Document Frequency, if `metric = 'all'` or `metric = 'df'`.
#'   - `idf`: Inverse Document Frequency, if `metric = 'all'` or `metric = 'idf'`.
#'   - `dp`: Gries' Deviation of Proportions, if `metric = 'all'` or `metric = 'dp'`.
#' @export
#'
#' @references
#' Gries, Stefan Th. (2023). Statistical Methods in Corpus Linguistics.
#' In Readings in Corpus Linguistics: A Teaching and Research Guide for Scholars in Nigeria and Beyond, pp. 78-114.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   term = c("word1", "word1", "word2", "word2", "word2", "word3"),
#'   documents = c("doc1", "doc2", "doc1", "doc1", "doc2", "doc2")
#' )
#' calc_dispersion_metrics(data, type, documents, metric = "all")
#' }
#'
#' @importFrom rlang enquo quo_is_missing quo_is_symbol
#' @importFrom dplyr count
#' @importFrom tidytext cast_sparse
#' @importFrom tibble tibble
calc_dispersion_metrics <- function(data, type, documents, metric = "all") {
  # Check if data is a data.frame
  if (!is.data.frame(data)) {
    stop("The argument 'data' must be a data frame.")
  }

  # Check if type and documents exist in data
  if (!all(c(deparse(substitute(type)), deparse(substitute(documents))) %in% names(data))) {
    stop("The variables specified in 'type' and 'documents' must exist in 'data'.")
  }

  # Check if metric is a character and one of the allowed values
  if (!is.character(metric) || !metric %in% c("all", "df", "idf", "dp")) {
    stop("The argument 'metric' must be a character string: 'all', 'df', 'idf', or 'dp'.")
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
  if (metric == "all" || metric == "df") {
    output_df$df <- calc_df(tdm)
  }

  if (metric == "all" || metric == "idf") {
    output_df$idf <- calc_idf(tdm)
  }

  if (metric == "all" || metric == "dp") {
    output_df$dp <- calc_dp(tdm_normalized, corpus_parts)
  }

  return(output_df)
}
