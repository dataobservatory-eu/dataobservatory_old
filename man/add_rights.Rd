% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata_descriptive.R
\name{add_rights}
\alias{add_rights}
\title{Add Rights}
\usage{
add_rights(
  rightsIdentifier = "CC-BY-NC-SA-4.0",
  rightsURI = NULL,
  format = "json"
)
}
\arguments{
\item{rightsIdentifier}{An identifier of the applicable use right, defaults to
\code{'CC-BY-NC-SA-4.0'}.
\href{https://spdx.org/licenses/}{SPDX License List}}

\item{format}{Defaults to \code{'json'}, in which case a JSON-formatted single character
string is returned.  The alternative is \code{'list'}.}

\item{rigthsURI}{A URL to the definition of the rigth. Automatically created from
\href{https://spdx.org/licenses/}{SPDX License List}.
Defaults to \code{NULL}, when the download will be called with \code{id}.}
}
\description{
Add access rights information to the metadata.
}
\details{
In Dublin Core, Rights Management:
\href{https://www.dublincore.org/specifications/dublin-core/cross-domain-attribute/}{A Cross-Domain Attribute Set}

DataCite Metadata Schema Optional Properties, Rights:
\href{https://support.datacite.org/docs/datacite-metadata-schema-v44-recommended-and-optional-properties}{DataCite Metadata Schema v4.4 Recommended and Optional Properties}

Usually you do not have to fill in \code{rigthsURI}. We only use
standard rights definitions in \href{https://spdx.org/licenses/}{SPDX License List}.
}
\examples{
add_rights( 'CC-BY-NC-SA-4.0')
}
