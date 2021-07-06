#' @title DataCite Properties
#'
#' @description The properties of the DataCite Metadata Schema in tabular form.
#'
#' @details There are three different levels of obligation for the metadata properties: \cr
#' - Mandatory (M) properties must be provided, \cr
#' - Recommended (R) properties are optional, but strongly recommended for interoperability \cr
#' - Optional (O) properties are optional and provide richer description. \cr
#'
#' More detailed descriptions of the properties, and their related sub-properties,
#' are provided in:
#' - \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-mandatory-properties/}{DataCite Metadata Schema v4.4 Mandatory Properties}; \cr
#' - \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-mandatory-properties/}{DataCite Metadata Schema v4.4 Recommended and Optional Metadata}.
#' @format A data frame with 20 rows and 3 variables:
#' \describe{
#'   \item{ID}{ID (a number).}
#'   \item{Property}{Description of the DataCite Field.}
#'   \item{Structure}{Additional strucutre information.}
#'   \item{Obligation}{M - Mandatory, R - Recommended, O - Optional.}
#' }
#' @source \url{https://support.datacite.org/docs/datacite-metadata-schema-v44-properties-overview}
#' @seealso datacite
"datacite_properties"
