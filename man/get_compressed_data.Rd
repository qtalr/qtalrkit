% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_compressed_data.R
\name{get_compressed_data}
\alias{get_compressed_data}
\title{Download a Compressed File and Decompress its Contents}
\usage{
get_compressed_data(url, target_dir, force = FALSE, confirmed = FALSE)
}
\arguments{
\item{url}{A character vector representing the full url to the compressed file}

\item{target_dir}{The directory where the compressed file should be downloaded}

\item{force}{An optional argument which forcefully overwrites existing data}

\item{confirmed}{If \code{TRUE}, the user has confirmed that they have permission to use the data.
If \code{FALSE}, the function will prompt the user to confirm permission.
Setting this to \code{TRUE} is useful for reproducible workflows.}
}
\value{
Download and extract the compressed data file
}
\description{
Possible file types include .zip, .gz, .tar, and .tgz
}
\examples{
\dontrun{
get_compressed_data(url = "http://www.test.com/file.zip", target_dir = "./")
}
}
