#' Calculate Association Metrics for Bigrams
#'
#' This function calculates various association metrics (PMI, Dice's Coefficient, Lambda-Rank)
#' for bigrams in a given corpus. The data frame must contain document and token indices,
#' as well as a 'type' variable representing the tokens.
#'
#' @param data A data frame containing the corpus.
#' @param doc_index A string name of the column in 'data' which represents the document index.
#' @param token_index A string name of the column in 'data' which represents the token index.
#' @param type A string name of the column in 'data' which represents the tokens or terms.
#' @param association A character vector specifying which metrics to calculate. Can be any combination of 'pmi' (Pointwise Mutual Information), 'dice_coeff' (Dice's Coefficient), 'g_score' (G-score), or 'all' (calculate all metrics). Default is 'all'.
#' @param verbose A logical value indicating whether to keep the intermediate probability columns ('p_xy', 'p_x', 'p_y') in the result. Default is FALSE.
#'
#' @return A data frame with one row per bigram and columns for each calculated metric.
#' If 'verbose' is TRUE, the intermediate probabilities used in the calculations are also included.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' data <- tibble::tibble(
#'   doc_index = c(1, 1, 1, 2),
#'   token_index = c(1, 2, 3, 1),
#'   type = c("word1", "word2", "word3", "word2")
#' )
#' calc_assoc_metrics(data, doc_index, token_index, type)
#' }
#'
#' @importFrom dplyr arrange count filter group_by left_join mutate select summarize ungroup
#' @export
calc_assoc_metrics <- function(data, doc_index, token_index, type, association = "all", verbose = FALSE) {
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    stop("The argument 'data' must be a data frame.")
  }

  # Check if doc_index is a column name in data
  if (!deparse(substitute(type)) %in% names(data)) {
    stop("The variable specified in 'doc_index' must exist in 'data'.")
  }

  # Check if token_index is a column name in data
  if (!deparse(substitute(type)) %in% names(data)) {
    stop("The variable specified in 'token_index' must exist in 'data'.")
  }

  # Check if type is a column name in data
  if (!deparse(substitute(type)) %in% names(data)) {
    stop("The variable specified in 'type' must exist in 'data'.")
  }

  # Check if association is a character vector and if all of its values are allowed
  if (!is.character(association) || !all(association %in% c("all", "pmi", "dice_coeff", "g_score"))) {
    stop("The argument 'association' must be a character vector containing any combination of: 'all', 'pmi', 'dice_coeff', or 'g_score'.")
  }

  # Calculate the PMI of each bigram
  # Purpose: get the counts of each bigram in the corpus
  bigram_counts <-
    data |>
    arrange({{ doc_index }}, {{ token_index }}) |> # sort by document and token
    mutate(x = {{ type }}) |>
    mutate(y = lead(x)) |> # create a column for the next token
    filter(!is.na(y)) |> # remove rows where y is NA
    count(x, y, sort = TRUE) # count the frequency of each bigram

  # Purpose: get the total number of bigrams in the corpus
  total_bigrams <- nrow(bigram_counts)

  # Purpose: calculate the probability of each bigram
  p_xy <-
    bigram_counts |>
    mutate(p_xy = n / total_bigrams)

  # Purpose: calculate the probability of each word
  p_xy_x_y <-
    p_xy |> # use the bigram probabilities
    left_join(
      bigram_counts |> # join the bigram counts
        group_by(x) |> # group by x
        summarize(total_x = sum(n)) |> # calculate total x count
        mutate(p_x = total_x / total_bigrams) |> # calculate probability of x
        ungroup(), # ungroup
      by = "x" # join p_xy.x with bigram_counts.x
    ) |>
    left_join(
      bigram_counts |>
        group_by(y) |> # group by y
        summarize(total_y = sum(n)) |> # calculate total y count
        mutate(p_y = total_y / total_bigrams) |> # calculate probability of y
        ungroup(), # ungroup
      by = "y" # join p_xy.y with bigram_counts.y
    )
  # Define variables for use in the rest of the function
  x <- p_xy_x_y$x
  y <- p_xy_x_y$y
  total_x <- p_xy_x_y$total_x
  total_y <- p_xy_x_y$total_y
  p_x <- p_xy_x_y$p_x
  p_y <- p_xy_x_y$p_y

  metrics <-
    p_xy_x_y |>
    # Avoid integer overflow
    mutate(across(!c(x, y), as.numeric))

  # Calculate metrics based on user choice
  if ("all" %in% association || "pmi" %in% association) {
    # Calculate the PMI of each bigram
    metrics <-
      metrics |>
      mutate(pmi = log(p_xy / (p_x * p_y)))
  }

  if ("all" %in% association || "dice_coeff" %in% association) {
    # Calculate Dice's Coefficient of each bigram
    metrics <-
      metrics |>
      mutate(dice_coeff = 2 * p_xy / (p_x + p_y))
  }

  if ("all" %in% association || "g_score" %in% association) {
    # Calculate the G-Score of each bigram
    metrics <-
      metrics |>
      mutate(g_score = 2 * log(p_xy) - log(p_x) - log(p_y))
  }

  # Remove the intermediate columns
  metrics <- metrics |>
    select(-total_x, -total_y)

  if (!verbose) {
    metrics <- metrics |>
      select(-p_xy, -p_x, -p_y)
  }

  # Return the metrics
  return(metrics)
}
