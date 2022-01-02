#' @title Add administrative metadata
#' @details
#' SDMX Code list for Frequency 2.1 (\code{CL_FREQ}) frequency values
#' Version 2.1 – 2021-02-13
#' @param id The code of the administrative metadata.
#' @param .f The constructor function of the administrative metadata.
#' @param admin_format The return format of the administraive metadata, defaults to
#' \code{id} (only the id value is returned), or \code{'list'}, \code{'json'} or
#' \code{'data.frame'}.
#' @param name Name of the administrative metadata.
#' @param description Description of the administrative metadata.
#' @importFrom jsonlite toJSON
#' @return An id, a list, a data.frame or a json formatted administrative metadata record.

add_admin_metadata <- function(id,
                               .f= 'cl_freq',
                               admin_format = 'id',
                               name = NA_character_,
                               description = NA_character_) {


  assertthat::assert_that (
    admin_format %in% c("id", "list", "json",
                        "data.frame", "data frame"),
    msg = "The 'admin_format' must be id, json, list, or data frame.")

  codelist <- get (.f)(id = id, name = name)

  var_name <- case_when (
    .f == "cl_freq"  ~ "freq",
    .f == 'cl_obs_status' ~ "obs_status",
    .f == 'cl_unit_not_validated' ~ "unit",
    TRUE ~ NA_character_ )

  error_msg <- glue("The '{var_name}'='{id}' is not a valid parameter.\nValid values are {paste( codelist$id, collapse = ', ')}")


  assertthat::assert_that ( all(id %in% codelist$id),
                            msg = error_msg
  )


  codelist <- codelist [ codelist$id %in% id, ]

  if ( admin_format == 'id') {
    return (id) } else if ( admin_format == "json") {
      apply ( codelist, 1, jsonlite::toJSON  )
    }  else if ( admin_format == 'list') {
      if (length(id)==1) {
        as.list(codelist)
      } else {
        apply ( codelist, 1, as.list)
      }
    } else {
      codelist
    }
}


#' @title Add frequency information
#' @description Add observation frequency information to the administrative metadata
#' of a dataset.
#' @details
#' Implements SDMX Code List for Frequency 2.1 (
#' \href{https://sdmx.org/?page_id=3215/}{CL_FREQ}
#' ) frequency values
#' @inheritParams add_admin_metadata
#' @export

add_frequency <- function(id, admin_format = 'id') {
  add_admin_metadata(id, 'cl_freq', admin_format = admin_format)
}

#' @title Add unit information
#' @description Add observation frequency information to the administrative metadata
#' of a dataset.
#' @details
#' Uses unit codelists.  It has an unvalidated version for free unit data entry.
#' @inheritParams add_admin_metadata
#' @param validate Defaults to \code{FALSE}.
#' @examples
#' add_unit ("NR", "number", admin_format = "json", validate = FALSE)
#' @export
add_unit <- function(id, name, admin_format = 'id', validate = FALSE) {

  add_admin_metadata(id, name,
                     .f = 'cl_unit_not_validated',
                     admin_format = admin_format)
}

#' @title Add observation status
#'
#' @description Add observation status coding, such as actual, missing, estimated or
#' imputed values to a dataset.
#'
#' @details
#' Implements SDMX Code List for Observation Status 2.2 (
#' \href{https://sdmx.org/?sdmx_news=new-version-of-code-list-for-observation-status-version-2-2}{CL_OBS_STATUS)})
#' frequency values.
#' Version 2.2, 2021-02-13.
#' @inheritParams add_admin_metadata
#' @export

add_obs <- function(id, admin_format = 'id') {

  add_admin_metadata(id,
                     .f='cl_obs_status',
                     admin_format = admin_format)
}

#' @title Add session information
#'
#' @description Converts \code{\link{[utils](sessionInfo)}} into a JSON text.
#' @importFrom utils sessionInfo
#' @importFrom tibble enframe
#' @importFrom jsonlite toJSON
#' @importFrom tidyr unite
#' @return A JSON text.
#' @export
add_sessioninfo <- function() {
  si <- sessionInfo()
  tmp_si <- si [vapply (si, is.character, logical(1))]

  tibble::enframe(unlist(tmp_si),
          name = 'session_variable') %>%
    jsonlite::toJSON(flatten = TRUE)

  #jsonlite::toJSON ( tmp_df, flatten = TRUE, auto_unbox = TRUE )

  #jsonlite::toJSON ( si [vapply (si, is.character, logical(1))], flatten = TRUE, auto_unbox = TRUE)

    }

#' @importFrom tibble tribble
#' @importFrom jsonlite toJSON
#' @keywords internal
cl_freq <- function(...) {

  cl_freq <- tibble::tribble(
    ~id,  ~name, ~description, ~iso8106,
    'A','Annual','To be used for data collected or disseminated every year','P1Y',
    'A2','Biennial','To be used for data collected or disseminated every two years','P2Y',
    'A3','Triennial','To be used for data collected or disseminated every three years','P3Y',
    'A4','Quadrennial','To be used for data collected or disseminated every four years','P4Y',
    'A5','Quinquennial','To be used for data collected or disseminated every five years','P5Y',
    'A10','Decennial','To be used for data collected or disseminated every ten years','P10Y',
    'A20','Bidecennial','To be used for data collected or disseminated every twenty years','P20Y',
    'A30','Tridecennial','To be used for data collected or disseminated every thirty years','P30Y',
    'A_3','Three times a year','To be used for data collected or disseminated three times a year','',
    'S','Half-yearly or semester','To be used for data collected or disseminated every semester','P0.5Y',
    'Q','Quarterly','To be used for data collected or disseminated every quarter','',
    'M','Monthly','To be used for data collected or disseminated every month','P1M',
    'M2','Bimonthly','To be used for data collected or disseminated every two months','P2M',
    'M_2','Semimonthly','To be used for data collected or disseminated twice a month','P0.5M',
    'M_3','Three times a month','To be used for data collected or disseminated three times a month','',
    'W','Weekly','To be used for data collected or disseminated every week','P1W',
    'W2','Biweekly','To be used for data collected or disseminated every two weeks','P2W',
    'W3','Triweekly','To be used for data collected or disseminated every three weeks','P3W',
    'W4','Four-weekly','To be used for data collected or disseminated every four weeks','P4W',
    'W_2','Semiweekly','To be used for data collected or disseminated twice a week','P0.5W',
    'W_3','Three times a week','To be used for data collected or disseminated three times a week','',
    'D','Daily','To be used for data collected or disseminated  every day','P1D',
    'D_2','Twice a day','To be used for data collected or disseminated twice a day','P0.5D',
    'H','Hourly','To be used for data collected or disseminated  every hour','PT1H',
    'H2','Bihourly','To be used for data collected or disseminated every two hours','PT2H',
    'H3','Trihourly','To be used for data collected or disseminated every three hours','PT3H',
    'B','Daily - business week','Similar to daily, however there are no observations for Saturdays and Sundays (so, neither "missing values" nor "numeric values" should be provided for Saturday and Sunday) ','',
    'N','Minutely','While N denotes minutely, usually, there may be no observations every minute (for several series the frequency is usually "irregular" within a day/days). And though observations may be sparse (not collected or disseminated every minute), missing values do not need to be given for the minutes when no observations exist: in any case the time stamp determines when an observation is observed','PT1M',
    'I','Irregular','To be used with irregular time series that stores data for a sequence of arbitrary timepoints. Irregular time series are appropriate when the data arrives unpredictably, such as when the application records every stock trade or when random events are recorded (the interval between each element can be a different length) ','',
    'OA','Occasional annual','The event occurs occasionally with an infrequent update that could span from 1 year to several years between events. It implies a survey that experiences a gap for several years prior to the next survey update (this is commonly linked to funding available to run a specific survey (i.e. health surveys), whereas a regular annual survey refers typically to programs that are funded regularly and fall under the Statistics Act, and therefore never experience a gap)','',
    'OM','Occasional monthly','The event occurs occasionally with an infrequent update that could span from 1 month to several months between events. It implies a survey that experiences a gap for several months prior to the next survey update','',
    '_O','Other','To be used when the qualitative or quantitative values that a variable takes in a data set is associated to multiple occurrences with frequency other than the already defined ones (for example every 5 hours and 32 minutes etc.)','',
    '_U','Unspecified','To be used when a set of values are reported within a time range but not associated to sub ranges. Often this could happen in case of missing or sparse information.','',
    '_Z','Not applicable','To be used when the qualitative or quantitative values that a variable takes in a data set is not associated to multiple occurrences (only single occurrence exists) one can use the _Z as frequency', ''
  )


  related_iso_identifier <-   tibble(
    RelatedItem = "ISO 8601 date and time format (correspondence table)",
    relationType = "Cites",
    relatedItemIdentifier = "https://www.iso.org/iso-8601-date-and-time-format.html",
    relatedItemIdentifierType = "URL") %>%
    bind_rows (
      tibble(RelatedItem = "SDMX Code List for Frequency",
                relationType = "IsDocumentedBy",
                relatedItemIdentifierType = "URL",
                relatedItemIdentifier = "https://sdmx.org/?page_id=3215/" )
    )
  cl_freq$RelatedItem <-  jsonlite::toJSON(related_iso_identifier,
                                           Date = "ISO8601")

  cl_freq
}

#' @importFrom tibble tribble
#' @keywords interna
cl_obs_status <- function(...) {
  obs_status <- tibble::tribble (
    ~id, ~name, ~description,
    'A',	'Normal value',	'To be used as default value if no value is provided or when no special coded qualification is assumed. Usually, it can be assumed that the source agency assigns sufficient confidence to the provided observation and/or the value is not expected to be dramatically revised.',
    'B', 'Time series break',	'Observations are characterised as such when different content exists or a different methodology has been applied to this observation as compared with the preceding one (the one given for the previous period).',
    'D',	'Definition differs',	'Used to indicate slight deviations from the established methodology (footnote-type information); these divergences do not imply a break in time series. ',
    'E',	'Estimated value',	'Observation obtained through an estimation methodology (e.g. to produce back-casts) or based on the use of a limited amount of data or ad hoc sampling and through additional calculations (e.g. to produce a value at an early stage of the production stage while not all data are available). It may also be used in case of experimental data (e.g. in the context of a pilot ahead of a full scale production process) or in case of data of (anticipated/assessed) low quality. If needed, additional information can be provided through free text using the COMMENT_OBS attribute at the observation level or at a higher level. This code is to be used when the estimation is done by a sender agency. When the imputation is carried out by a receiver agency in order to replace or fill gaps in reported data series, the flag to use is I “Value imputed by a receiving agency”.',
    'F',	'Forecast value',	'Value deemed to assess the magnitude which a quantity will assume at some future point of time (as distinct from "estimated value" which attempts to assess the magnitude of an already existent quantity).',
    'G',	'Experimental value',	'Data collected on the basis of definitions or (alternative) collection methods under development. Data not of guaranteed quality as normally expected from provider.',
    'I',	'Value imputed by a receiving agencyImputed value (CCSA definition)',	'Observation imputed by international organisationsa receiving agency to replace or fill gaps in national reported data series, in line with the recommendations of the United Nations Committee for the Coordination of Statistical Activities (CCSA).',
    'K',	'Data included in another category',	'This code is used when data for a given category are missing and are included in another category, sub-total or total. Generally where code “K” is used there should be a corresponding code "W - Includes data from another category" assigned to the over-covered category. Implementers and data reporters should use the COMMENT_OBS observation-level attribute to specify under which category the data are included.',
    'W',	'Includes data from another category',	'This code is used when data include another category, or go beyond the scope of the data collection and are therefore over-covered. Generally, where code "W" is used there should be a corresponding code "K - Data included in another category" assigned to the category which is under-covered. Implementers and data reporters should use the COMMENT_OBS observation-level attribute to specify which additional data are included.',
    'O',	'Missing value',	'This code is to be used when no breakdown is made between the reasons why data are missing. Data can be missing due to many reasons: data cannot exist, data exist but are not collected (e.g. because they are below a certain threshold or subject to a derogation clause), data are unreliable, etc.',
    'M',	'Missing value - data cannot exist',	'Used to denote empty cells resulting from the impossibility to collect a statistical value (e.g. a particular education level or type of institution may be not applicable to the education system of a given country).',
    'P',	'Provisional value',	'An observation is characterised as provisional when the source agency – while it bases its calculations on its standard production methodology – considers that the data, almost certainly, are expected to be revised.',
  )

  related_codebook <- tibble(RelatedItem = "SDMX Code List for Observation Status",
                    relationType = "IsDocumentedBy",
                    relatedItemIdentifier = "https://sdmx.org/?sdmx_news=new-version-of-code-list-for-observation-status-version-2-2/",
                    relatedItemIdentifierType = "URL") %>% jsonlite::toJSON()

  obs_status$RelatedItem <- related_codebook

  obs_status
}

#' @importFrom tibble tribble
#' @keywords internal
cl_unit_not_validated <- function(id, name,
                                  description = NA_character_,
                                  RelatedItem = NA_character_) {

  tibble::tribble (
    ~id,  ~name, ~description, ~RelatedItem,
    id, name, description, RelatedItem)
}

#' @keywords internal
resource_types_datacite <- function() {
  resource_types <- c('Audiovisual',
                  'Book',
                  'BookChapter',
                  'Collection',
                  'ComputationalNotebook',
                  'ConferencePaper',
                  'ConferenceProceeding',
                  'DataPaper',
                  'Dataset',
                  'Dissertation',
                  'Event',
                  'Image',
                  'InteractiveResource',
                  'Journal',
                  'JournalArticle',
                  'Model',
                  'OutputManagementPlan',
                  'PeerReview',
                  'PhysicalObject',
                  'Preprint',
                  'Report',
                  'Service',
                  'Software',
                  'Sound',
                  'Standard',
                  'Text',
                  'Workflow',
                  'Other'
  )

  resource_types
}

#' @keywords internal
relation_type_datacite <- function() {

  c("isCitedBy", "Cites",
    "isSupplementTo", "IsSupplementedBy",
    "IsDerivedFrom",
    "isSupplementedBy", "isNewVersionOf", "isPreviousVersionOf",
    "IsOriginalFormOf", "IsVariantFormOf", "Documents", "IsDocumentedBy",
    "References", "IsReferencedBy", "IsPublishedIn",
    "IsPreviousVersionOf", "IsNewVersionOf", "IsVersionOf", "HasVersion",
    "IsMetadataFor", "HasMetadata",
    "isPartOf", "IsSourceOf", "hasPart",
    "compiles", "isCompiledBy",
    "isIdenticalTo",
    "isAlternateIdentifier",
    "Obsoletes", "IsObsoletedBy",
    "Requires", "IsRequiredBy",
    "Reviews", "IsReviewedBy")
}

#' @importFrom jsonlite toJSON
#' @keywords internal
cl_method <-function () {

  zoo_citation <- citation("zoo")[[1]]
  forecast_citation <- citation("forecast")[[1]]


  zoo_related_item <- tibble(
    RelatedItem = zoo_citation$title,
    relationType = "isCompiledBy",
    relatedItemIdentifierType = "DOI",
    relatedItemIdentifier = zoo_citation$doi) %>% jsonlite::toJSON() %>% as.character()

  forecast_related_item <- tibble(
    RelatedItem = forecast_citation$title,
    relationType = "isCompiledBy",
    relatedItemIdentifierType = "URL",
    relatedItemIdentifier = forecast_citation$url) %>% jsonlite::toJSON() %>% as.character()

  dataobservaotry_related_item <- tibble(
    RelatedItem = "dataobservaotry",
    relatedItemType = "Software",
    relationType = "isCompiledBy",
    relatedItemIdentifier = "10.5281/zenodo.5034752",
    relatedItemIdentifierType = "DOI"
  ) %>% jsonlite::toJSON() %>% as.character()

  eurostat_related_item <- tibble(
    RelatedItem = "Retrieval and Analysis of Eurostat Open Data with the eurostat Package ",
    relatedItemType = "Software",
    relationType = "isCompiledBy",
    relatedItemIdentifier = "https://ropengov.github.io/eurostat/",
    relatedItemIdentifierType = "URL"
  ) %>% jsonlite::toJSON() %>% as.character()

 cl_method <-  tibble::tribble(
    ~id, ~name, ~description, ~RelatedItem,
    "A", "Actual values", "No method applied.", eurostat_related_item,
    "O", "Missing values", "No method applied.", dataobservaotry_related_item,
    "locf", "Last Observation Carry Forward", "Replacing each missing item with the most recent non-missing prior to it.", zoo_related_item,
    "nocb", "Next Observation Carry Backwards", "Replacing each missing item with the next non-missing prior after it.", zoo_related_item,
    "approx", "Linear Approximation", "Replacing each missing item with interpolated values.", zoo_related_item,
    "forecast", "Forecasted for the variable.", "Forecasted with a time-series model.", forecast_related_item,
    "backcast", "Backcasted for the variable.", "Backcasted with a time-series model.", forecast_related_item,
    "Backcast ETS(A,A,N)", "Forecasted with exponentional smoothing", "Exponential smoothing with Holt’s linear method with additive errors.", forecast_related_item,
    "Backcast ETS(A,Ad,N)", "Forecasted with exponentional smoothing", "Exponential smoothing with the additive damped trend method.", forecast_related_item,
    "Backcast ETS(M,A,N)", "Forecasted with exponentional smoothing", "Exponential smoothing with the additive Holt’s linear method with multiplicative errors.", forecast_related_item,
    "Backcast ETS(M,N,N)", "Forecasted with exponentional smoothing", "Exponential smoothing with simple exponential smoothing with multiplicative errors.", forecast_related_item,
    "Backcast ETS(M,Ad,N)", "Forecasted with exponentional smoothing", "Exponential smoothing (M, Ad, N)", forecast_related_item,
    "Backcast ETS(A,N,N)", "Forecasted with exponentional smoothing", "Exponential smoothing (Additive trend, no seasonal component).", forecast_related_item,
    "Forecast ETS(A,A,N)", "Forecasted with exponentional smoothing", "Exponential smoothing with Holt’s linear method with additive errors.", forecast_related_item,
    "Forecast ETS(A,Ad,N)", "Forecasted with exponentional smoothing", "Exponential smoothing with the additive damped trend method.", forecast_related_item,
    "Forecast ETS(M,A,N)", "Forecasted with exponentional smoothing", "Exponential smoothing with the additive Holt’s linear method with multiplicative errors.", forecast_related_item,
    "Forecast ETS(M,N,N)", "Forecasted with exponentional smoothing", "Exponential smoothing with simple exponential smoothing with multiplicative errors.", forecast_related_item,
    "Forecast ETS(M,Ad,N)", "Forecasted with exponentional smoothing", "Exponential smoothing (M, Ad, N)", forecast_related_item,
    "Forecast ETS(A,N,N)", "Forecasted with exponentional smoothing", "Exponential smoothing (Additive trend, no seasonal component).", forecast_related_item

    )

 cl_method


}


