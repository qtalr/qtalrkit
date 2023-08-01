#' Identify Outliers in a Numeric Variable
#'
#' This function identifies outliers in a numeric variable of a data.frame using the interquartile range (IQR) method.
#'
#' @param data A data.frame object.
#' @param variable_name A symbol representing a numeric variable in `data`.
#'
#' @return A data.frame containing the outliers in `variable_name`.
#' @export
#'
#' @examples \dontrun{
#' data(mtcars)
#' find_outliers(mtcars, mpg)
#' find_outliers(mtcars, wt)
#' }
#' @import dplyr
#' @importFrom rlang enquo quo_is_missing quo_is_symbol
#' @importFrom stats quantile
find_outliers <- function(data, variable_name) {
  # Check if `data` is a data.frame
  tryCatch(
    {
      data |> pull(1)
    },
    error = function(e) {
      stop("The first argument must be a data.frame.")
    }
  )
  # Check if `variable_name` is unquoted
  if (rlang::quo_is_missing(rlang::enquo(variable_name))) {
    stop("The second argument must be unquoted.")
  }
  # Check if `variable_name` is a symbol
  if (!rlang::quo_is_symbol(rlang::enquo(variable_name))) {
    stop("The second argument must be a symbol.")
  }
  # Check if `variable_name` exists in `data`
  tryCatch(
    {
      data |> pull({{ variable_name }})
    },
    error = function(e) {
      stop("The second argument must be a variable in the data.")
    }
  )
  # Check if `variable_name` is numeric
  if (!is.numeric(data |> pull({{ variable_name }}))) {
    stop("The second argument must be a numeric variable.")
  }
  # Calculate the quartiles using summary() function
  quartiles <-
    data |> summarize(
      q1 = stats::quantile({{ variable_name }}, 0.25),
      q3 = stats::quantile({{ variable_name }}, 0.75)
    )
  # Extract the quartiles
  q1 <- quartiles$q1
  q3 <- quartiles$q3
  # Calculate the interquartile range (IQR)
  iqr <- q3 - q1
  # Calculate the upper and lower fences
  upper_fence <- q3 + 1.5 * iqr
  lower_fence <- q1 - 1.5 * iqr
  # Filter the dataset by fences to identify outliers
  outliers <- 
    data |> 
    dplyr::filter({{ variable_name }} > upper_fence |
      {{ variable_name }} < lower_fence)
  # Check if there are any outliers
  if (nrow(outliers) == 0) {
    message("No outliers found.")
  } else {
    # Return the outliers
    return(outliers)
  }
}