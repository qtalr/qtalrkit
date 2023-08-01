#' Confirm permission to use data
#'
#' This function is for internal use only.
#' 
#' This function confirms that the user has permission to use the data. 
#' If not, the script is returns FALSE and stops.
#' 
#' @keywords internal
#' @return TRUE if the user confirms permission, FALSE otherwise
confirm_permission <- function() {
  # Confirm that the user has permission to use the data
  # If not, stop the script
  message("Are you aware of the permissions to use this data?")
  message("Please type 'yes' or 'no' and press enter.")
  permission <- readline()
  if (permission %in% c("Y", "y", "yes", "Yes", "YES")) {
    message("Continuing...")
    return(TRUE)
  } else if (permission %in% c("N", "n", "no", "No", "NO")) {
    message("Please review the documentation and try again.")
    return(FALSE)
  } else {
    confirm_permission()
  }
}