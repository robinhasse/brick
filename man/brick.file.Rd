% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brick.file.R
\name{brick.file}
\alias{brick.file}
\title{Find the full file names of files in BRICK}
\usage{
brick.file(..., mustWork = TRUE)
}
\arguments{
\item{...}{character vectors, specifying subdirectory and files within brick}

\item{mustWork}{if TRUE, an error is given if there are no matching files}
}
\value{
A character vector of positive length, containing the file paths
  that matched \code{...} in BRICK.
}
\description{
This is meant to work with the installed package BRICK but also when loading
the package via \code{devtools::load_all("path/to/brick")}
}
\author{
Robin Hasse
}
