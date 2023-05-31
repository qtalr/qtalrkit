#' Download a Compressed File and Decompress its Contents
#'
#' Possible file types include .zip, .gz, .tar, and .tgz
#'
#' @param url A character vector representing the full url to the compressed file
#' @param target_dir The directory where the compressed file should be downloaded
#' @param force An optional argument which forcefully overwrites existing data
#'
#' @returns Download and extract the compressed data file
#'
#' @importFrom utils download.file unzip untar
#' @importFrom tools file_ext
#'
#' @examples \dontrun{
#' get_compressed_data(url = "http://www.test.com/file.zip", target_dir = "./")
#' }
#' @export
get_compressed_data <- function(url, target_dir, force = FALSE) {
  # Get the extension of the target file
  ext <- tools::file_ext(url)
  # Check to see if the target file is a compressed file
  if (!ext %in% c("zip", "gz", "tar", "tgz")) stop("Target file given is not supported")
  # Check to see if the data already exists
  if (!dir.exists(target_dir) | force == TRUE) { # if data does not exist, download/ decompress
    cat("Creating target data directory \n") # print status message
    dir.create(path = target_dir, recursive = TRUE, showWarnings = FALSE) # create target data directory
    cat("Downloading data... \n") # print status message
    temp <- tempfile() # create a temporary space for the file to be written to
    utils::download.file(url = url, destfile = temp) # download the data to the temp file
    # Decompress the temp file in the target directory
    if (ext == "zip") {
      utils::unzip(zipfile = temp, exdir = target_dir, junkpaths = TRUE) # zip files
    } else {
      utils::untar(tarfile = temp, exdir = target_dir) # tar, gz files
    }
    # Clean up the filenames
    files <- list.files(target_dir) # get the files in the target directory
    new_files <- sub(pattern = "^\\.", replacement = "", files) # remove the leading period from the file names
    file.rename(from = file.path(target_dir, files), to = file.path(target_dir, new_files)) # rename the files

    cat("Data downloaded! \n") # print status message
  } else { # if data exists, don't download it again
    cat("Data already exists \n") # print status message
  }
}
