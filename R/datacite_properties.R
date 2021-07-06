#' @title DataCite Properties
#'
#' @description The properties of the DataCite Metadata Schema in tabular form.
#'
#' @details
#' More detailed descriptions of the properties, and their related sub-properties,
#' are provided in DataCite Schema Mandatory Properties and
#' DataCite Schema Recommended and Optional Metadata.
#'
#' There are three different levels of obligation for the metadata properties:
#' - Mandatory (M) properties must be provided, \cr
#' - Recommended (R) properties are optional, but strongly recommended for interoperability \cr
#' - Optional (O) properties are optional and provide richer description. \cr
#'
#' @format A data frame with 20 rows and 3 variables:
#' \describe{
#'   \item{ID}{ID (a number).}
#'   \item{Property}{Description of the DataCite Field.}
#'   \item(Structure){Additional strucutre information.}
#'   \item{Oblibation}{M - Mandatory, R - Recommended, O - Optional.}
#' }
#' @source \url{https://support.datacite.org/docs/datacite-metadata-schema-v44-properties-overview}
#' @seealso datacite
"datacite_properties"
