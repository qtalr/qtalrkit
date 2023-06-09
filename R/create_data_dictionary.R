#' Create a Data Dictionary for a Given Dataset
#'
#' The data dictionary returned includes the variable name, human-readable name, description, and variable type. The function uses OpenAI's chat completions models to generate the descriptions for each of the variables in the dataset. The data dictionary is returned as a data frame and written to a file in CSV format.
#'
#' @param data A data frame to create a data dictionary for.
#' @param file_path A character string specifying the file path to write the data dictionary to.
#' @param sample_n An integer specifying the number of rows to sample from the data frame.
#' @param grouping A character vector specifying the grouping variables to use when sampling the data frame.
#' @param model A character vector specifying the OpenAI chat completion model to use for generating the variable descriptions. Defaults to "gpt-3.5-turbo". See \code{\link[openai]{create_chat_completion}} for more information.
#'
#' @returns A data frame containing the variable name, human-readable name, description, and variable type.
#'
#' @import dplyr
#' @import glue
#' @import openai
#' @import stringr
#' @importFrom readr read_csv write_csv
#' @importFrom utils capture.output
#'
#' @examples \dontrun{
#' create_data_dictionary(mtcars, "mtcars_data_dictionary.csv")
#' }
#' @export
create_data_dictionary <- function(data, file_path, sample_n = 5, grouping = NULL, model = "gpt-3.5-turbo") {
  # Set the OpenAI API key
  if (is.na(Sys.getenv()["OPENAI_API_KEY"])) message("Set your OPENAI_API_KEY environment variable.")

  # Set the instructions for the prompt
  prompt_instructions <- "I have a dataset I would like you to create a data dictionary for. The information I want is the `variable`, `name` (human readable), `variable_type` (one of 'categorical', 'ordinal', or 'numeric'), and `description`. Here's a small sample of the data for you to work with. In some cases some variables in the dataset may be null. In these cases use the variable name to predict the other information. Please return your data dictionary in `.csv` format. Remember to enclose text in quotes and only return the `.csv` info, no explanations."

  # Get a the first 5 rows of the data frame
  data_sample <-
    data |>
    dplyr::slice_sample(n = sample_n, by = dplyr::all_of(grouping)) |>
    dplyr::mutate_if(is.character, stringr::str_trunc, width = 50) # truncate character variables to 50 characters

  # Convert the data sample to R code as a string
  prompt_data <-
    data_sample |>
    dput() |> # convert to R code
    capture.output() |> # capture the output
    paste(collapse = " ") # collapse the output into a single string

  # Combine the instructions and the data sample into a single string
  prompt <- glue::glue("{prompt_instructions}\n\n{prompt_data}")

  # Use openai to generate the descriptions for each of the variables in the `data_sample` data frame. This will be used to create the data dictionary.
  response <- openai::create_chat_completion(model = model, messages = list(list("role" = "user", "content" = prompt)), max_tokens = 500)

  # Create a data frame with the variable names, human-readable names, and descriptions
  data_dict <-
    response$choices["message.content"] |> # get the response from the API
    as.character() |> # convert to a character vector
    readr::read_csv() |> # read the data dictionary as a data frame
    suppressMessages() # suppress messages

  # Write the data dictionary to a file
  data_dict |> readr::write_csv(file = file_path)

  # Return the data dictionary
  return(data_dict)
}
