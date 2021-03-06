% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata_administrative.R
\name{add_frequency}
\alias{add_frequency}
\title{Add frequency information}
\usage{
add_frequency(id, admin_format = "id")
}
\arguments{
\item{id}{The code of the administrative metadata.}

\item{admin_format}{The return format of the administraive metadata, defaults to
\code{id} (only the id value is returned), or \code{'list'}, \code{'json'} or
\code{'data.frame'}.}
}
\description{
Add observation frequency information to the administrative metadata
of a dataset.
}
\details{
Implements SDMX Code List for Frequency 2.1 (
\href{https://sdmx.org/?page_id=3215/}{CL_FREQ}
) frequency values
}
