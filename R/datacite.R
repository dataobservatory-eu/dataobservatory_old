#' @title DataCite object
#'
#' @description A metadata object with DataCite mandatory, recommended and optional
#' fields.
#' @details
#' Use \code{\link{datacite_dataset}} to fill up a DataCite record with adding only
#' those parameters that have no default values. In this case, a given \code{\link{dataset}}
#' objects:
#' - Title will be added as a string.
#' - Date will be added with \code{\link{add_dates}} \cr
#' - Rigths will be set to the default value by \code{\link{add_rights}} \cr
#' - Size will be added by \code{\link{add_size}} \cr
#' - GeoLocation will be added by \code{\link{add_geolocation}} \cr
#'
#' @details
#' Dublin Core:
#' \href{https://www.dublincore.org/specifications/dublin-core/cross-domain-attribute/}{A Cross-Domain Attribute Set}
#'
#' Mandatory Properties:
#' \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-mandatory-properties/}{DataCite Metadata Schema v4.4 Mandatory Properties}
#'
#' Recommended and Optional Properties:
#' \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-recommended-and-optional-properties/}{DataCite Metadata Schema v4.4 Recommended and Optional Properties}
#'
#' @param dataset_code A short, unique, programatically usable, memorizable dataset
#' ID, given by an observatory curator.
#' @param Identifier Identifiers created by \code{\link{add_identifiers}}
#' @param Creator Creator field created by \code{\link{add_creators}}.
#' @param Title The title of the dataset.
#' @param Publisher The publisher of the dataset.
#' @param PublicationYear The publication year given as a four-digit number.
#' @param ResourceType The type of the resource, defaults to \code{"Dataset"}.
#' @param Subject The subject
#' @param Contributor The Contributor field created by \code{\link{add_contributors}}.
#' @param Date The Date filed created by \code{\link{add_dates}}.
#' @param Language Defaults to \code{'eng'}.
#' @param AlternateIdentifier Defaults to NA_character_. It should have type subproperty.
#' @param RelatedIdentifier Defaults to NA_character_.
#' Related identifier should have type and relation type sub-properties.
#' @param Size The size of the dataset, as measured in CSV format in bytes.
#' @param Format The format(s) of the dataset.
#' @param Version The version of the dataset.
#' @param Rights The rights related to the datset, created by \code{\link{add_rights}}.
#' @param Description The description of the dataset.
#' @param Geolocation The geographical dimension of the observation in the dataset,
#' created by \code{\link{add_geolocation}}.
#' @return A data frame of DataCite properties are variable columns.
#' @seealso datacite_properties
#' @examples
#' data("small_population")
#'
#' small_population_dataset <- dataset (
#'          x= small_population,
#'          dataset_code = "small_population_total",
#'          dataset_title = "Population of Small European Countries",
#'          freq = "A",
#'          unit = "NR",
#'          unit_name = "number")
#'
#' datacite_dataset (
#'         dataset = small_population_dataset,
#'         dataset_code = "small_population_total",
#'         Title = "Population on 1 January by age and sex",
#'         Subject = "Demography",
#'         Creator = add_creators("Jane", "Doe", "Reprex")
#'         )
#' @export

datacite <- function (
  dataset_code,
  Identifier = NA_character_,         # DataCite mandatory
  Creator,                            # DataCite mandatory
  Title,                              # DataCite mandatory
  Publisher = NA_character_,          # DataCite mandatory
  PublicationYear = NA_real_,         # DataCite mandatory
  ResourceType = "Dataset",           # DataCite mandatory
  Subject,                            # DataCite recommended + Dublin Core
  Contributor = NA_character_,        # DataCite recommended
  Date = as.character(Sys.Date()),    # DataCite recommended
  Language = "eng",                   # DataCite optional + Dublin Core
  AlternateIdentifier = NA_character_,# DataCite optional
  RelatedIdentifier  = NA_character_, # DataCite recommended
  Size = NA_character_,               # DataCite optional
  Format = NA_character_,             # DataCite optional + Dublin Core
  Version = NA_character_,            # DataCite optional
  Rights = add_rights(),              # DataCite optional + Dublin Core
  Description = NA_character_,        # DataCite recommended
  GeoLocation = NA_character_,        # DataCite recommended + Dublin Core coverage
  FundingReference = NA_character_ ,  # DataCite optional
  RelatedItem = NA_character_         # DataCite optional + Dublin Core
) {

  datacite <- datacite_new(
    dataset_code = dataset_code,           # DataCite mandatory
    Identifier = Identifier,               # DataCite mandatory
    Creator = Creator,                     # DataCite mandatory
    Title = Title,                         # DataCite mandatory
    Publisher = Publisher,                 # DataCite mandatory
    PublicationYear = PublicationYear,     # DataCite mandatory
    ResourceType = ResourceType,           # DataCite mandatory
    Subject = Subject,                     # DataCite recommended + Dublin Core
    Contributor = Contributor,             # DataCite recommended
    Date = Date,                           # DataCite recommended
    Language = Language,                         # DataCite optional + Dublin Core
    AlternateIdentifier = 	AlternateIdentifier, # DataCite optional
    RelatedIdentifier = RelatedIdentifier,       # DataCite recommended
    Size = Size,                                 # DataCite optional
    Format = Format,                       # DataCite optional + Dublin Core
    Version = Version,                     # DataCite optional
    Rights = Rights,                       # DataCite optional + Dublin Core
    Description = Description,             # DataCite recommended
    GeoLocation = GeoLocation,             # DataCite recommended + Dublin Core coverage
    FundingReference = FundingReference,   # DataCite optional
    RelatedItem = RelatedItem              # DataCite optional + Dublin Core
  )

  datacite
}

#' @rdname datacite
#' @param dataset A dataset object. Its Size, Geolocation and Date properties
#' will be added to the metadata object.
#' @param description A basic description for the Abstract property of the
#' description.
#' @param keywords In the \code{Other} sub-field of the \code{Description} metadata property,
#' we record keywords for structuring our observatory.
#' @inheritParams datacite
#' @examples
#' data("small_population")
#' small_population_dataset <- dataset (
#'   x = small_population,
#'   dataset_code = "small_population_total",
#'   dataset_title = "Population of Small European Countries",
#'   freq = "A",
#'   unit = "NR",
#'   unit_name = "number")
#'
#' #The final DataCite properties are Capitalized:
#'
#' small_population_datacite <- datacite_dataset(
#'   dataset = small_population_dataset,
#'   keywords = c("greendeal", "Demography", "Testing"),
#'   description = "Example dataset with three small countries",
#'   Subject = "Demography",
#'   Creator = "Joe, Doe"
#'   )
#' @export

datacite_dataset <- function(dataset,
                             dataset_code = NULL,
                             description = NULL,
                             keywords = NULL,
                             Title = NULL,
                             Subject = NULL,
                             Creator,
                             Contributor = NULL ) {

  if ( is.null(dataset_code)) {
    dataset_code <- attr(dataset, "dataset_code")
    assertthat::assert_that(
      !is.null(dataset_code),
      msg = "The dataset_code must not be NULL."
      )
  }
  if ( is.null(Title)) {
    Title <- attr(dataset, "Title")
    assertthat::assert_that(
      !is.null(Title),
      msg = "The Title must not be NULL."
    )
    }
  if ( is.null(Subject)) {
    Subject <- attr(dataset, "Subject")
    assertthat::assert_that(
      !is.null(Subject),
      msg = "The Subject must not be NULL."
    )
    }

  if ( is.null(description)) {
    Abstract = "Title" } else {
      Abstract = description
    }

  dates_to_add <- add_dates (
    Date = Sys.Date(),
    EarliestObservation = attr(dataset, "earliest_actual_observation"),
    LatestObservation =   attr(dataset, "latest_actual_observation")
    )


  if (!is.null(keywords)) {
    Description <- add_description(
      Abstract = Abstract,
      Other = create_json_text(
        list (  id = paste0("keyword", 1:length(keywords)),
                name = keywords )
      )
    )
  } else {
    Description <- add_description(
      Abstract = Abstract
    )
  }

  datacite (
    dataset_code = dataset_code,
    Title = Title,
    Subject = Subject,
    Creator = Creator,
    Contributor = NA_character_,
    Date = dates_to_add,
    Description = Description,
    Size = add_size(dataset),
    GeoLocation = add_geolocation(dataset)
  )
}

#' @title Datacite constructor
#' @inheritParams datacite
#' @keywords internal
datacite_new    <- function (
  dataset_code,
  Identifier = NA_character_,          # DataCite mandatory
  Creator,                             # DataCite mandatory
  Title,                               # DataCite mandatory
  Publisher = NA_character_,           # DataCite mandatory
  PublicationYear = NA_real_,          # DataCite mandatory
  ResourceType = "Dataset",            # DataCite mandatory
  Subject,                             # DataCite recommended + Dublin Core
  Contributor = NA_character_,         # DataCite recommended
  Date = add_dates(),                  # DataCite recommended
  Language = "eng",                    # DataCite optional + Dublin Core
  AlternateIdentifier = NA_character_, # DataCite Optional
  RelatedIdentifier  = NA_character_,  # DataCite recommended
  Size = NA_character_,                # DataCite optional
  Format = NA_character_,              # DataCite optional + Dublin Core
  Version = NA_character_,             # DataCite optional
  Rights = add_rights(),               # DataCite optional + Dublin Core
  Description = NA_character_,         # DataCite recommended
  GeoLocation = NA_character_,         # DataCite recommended + Dublin Core coverage
  FundingReference = NA_character_ ,   # DataCite optional
  RelatedItem = NA_character_          # DataCite optional + Dublin Core
) {

  ## Setting Default Options -----------------
  if ( is.na(Identifier)) {
    Identifier = as.character(dataset_code)
  }

  if ( is.na(PublicationYear)) {
    PublicationYear = as.numeric(substr(as.character(Sys.Date()),1,4))
  }

  if ( is.na(Publisher)) {
    Publisher = as.character("Reprex")
  }

  validate_datacite(
    dataset_code = dataset_code,           # DataCite mandatory
    Identifier = Identifier,               # DataCite mandatory
    Creator = Creator,                     # DataCite mandatory
    Title = Title,                         # DataCite mandatory
    Publisher = Publisher,                 # DataCite mandatory
    PublicationYear = PublicationYear,     # DataCite mandatory
    ResourceType = ResourceType,           # DataCite mandatory
    Subject = Subject,                     # DataCite recommended + Dublin Core
    Contributor = Contributor,             # DataCite recommended
    Date = Date,                           # DataCite recommended
    Language = Language,                   # DataCite optional + Dublin Core
    AlternateIdentifier = AlternateIdentifier, # DataCite Optional
    RelatedIdentifier = RelatedIdentifier, # DataCite recommended
    Size = Size,                           # DataCite optional
    Format = Format,                       # DataCite optional + Dublin Core
    Version = Version,                     # DataCite optional
    Rights = Rights,                       # DataCite optional + Dublin Core
    Description = Description,             # DataCite recommended
    GeoLocation = GeoLocation,             # DataCite recommended + Dublin Core coverage
    FundingReference = FundingReference,   # DataCite optional
    RelatedItem = RelatedItem              #DataCite optional + Dublin Core
  )

  datacite <-tibble::tibble (
    dataset_code = dataset_code,           # DataCite mandatory
    Identifier = Identifier,               # DataCite mandatory
    Creator = Creator,                     # DataCite mandatory
    Title = Title,                         # DataCite mandatory
    Publisher = Publisher,                 # DataCite mandatory
    PublicationYear = PublicationYear,     # DataCite mandatory
    ResourceType = ResourceType,           # DataCite mandatory
    Subject = Subject,                     # DataCite recommended + Dublin Core
    Contributor = Contributor,             # DataCite recommended
    Date = Date,                           # DataCite recommended
    Language = Language,                   # DataCite optional + Dublin Core
    AlternateIdentifier = AlternateIdentifier, # DataCite Optional
    RelatedIdentifier = RelatedIdentifier, # DataCite recommended
    Size = Size,                           # DataCite optional
    Format = Format,                       # DataCite optional + Dublin Core
    Version = Version,                     # DataCite optional
    Rights = Rights,                       # DataCite optional + Dublin Core
    Description = Description,             # DataCite recommended
    GeoLocation = GeoLocation,             # DataCite recommended + Dublin Core coverage
    FundingReference = FundingReference,   # DataCite optional
    RelatedItem = RelatedItem              # DataCite optional + Dublin Core
  )

  class(datacite) <- c("datacite", class(datacite) )

  datacite
}


#' @rdname datacite
#' @export
is.datacite <- function (x) inherits(x, "datacite")

#' @rdname datacite
#' @param n The number of observations to print.
#' @importFrom dplyr mutate_all everything
#' @importFrom tidyr pivot_longer
#' @importFrom jsonlite fromJSON
#' @export
print.datacite <- function(x, ... ) {

  attributes ( x )


  main_title <- x$Title
  if (is.json(main_title)) {
    title_list <- jsonlite::fromJSON(main_title)
    main_title <- title_list$Title
  }

  cat("DataCite information for", main_title, "\n")
  json_items <- vapply (x, is.json, logical(1))
  not_json_items <- names(x)[!json_items]

  long_form <- x %>%
               dplyr::mutate_all ( as.character ) %>%
               pivot_longer ( dplyr::everything(),
                              values_to = "Value",
                              names_to = "Property")


  print (long_form)


}


#' @title Validate a datacite object.
#' @inheritParams datacite
#' @keywords internal
validate_datacite <- function(
  dataset_code,      # DataCite mandatory
  Identifier,        # DataCite mandatory
  Creator,           # DataCite mandatory
  Title,             # DataCite mandatory
  Publisher,         # DataCite mandatory
  PublicationYear,   # DataCite mandatory
  ResourceType,      # DataCite mandatory
  Subject,           # DataCite recommended + Dublin Core
  Contributor,       # DataCite recommended
  Date,              # DataCite recommended
  Language,          # DataCite optional + Dublin Core
  AlternateIdentifier, #DataCite otional
  RelatedIdentifier, # DataCite recommended
  Size,              # DataCite optional
  Format,            # DataCite optional + Dublin Core
  Version,           # DataCite optional
  Rights,            # DataCite optional + Dublin Core
  Description,       # DataCite recommended
  GeoLocation,       # DataCite recommended + Dublin Core coverage
  FundingReference,  # DataCite optional
  RelatedItem        # DataCite optional + Dublin Core
) {

  assertthat::assert_that(
    is.character(dataset_code) & length(dataset_code) == 1,
    msg = "The code parameter must be a character string of length 1."
  )

  assertthat::assert_that(
    is.character(Identifier) & length(Identifier) == 1,
    msg = "The Identifier parameter must be a character string of length 1 (can be json, too.)"
  )

  assertthat::assert_that(
    is.character(Creator) & length(Creator) == 1,
    msg = "The Creator parameter must be a character string of length 1 (can be json, too.)"
  )

  assertthat::assert_that(
    is.character(Contributor) & length(Contributor) == 1,
    msg = "The Contributor parameter must be a character string of length 1 (can be json, too.)"
  )

  assertthat::assert_that(
    is.character(Date) & length(Date) == 1,
    msg = "The Date parameter must be a character string of length 1 (can be json, too.)"
  )


  assertthat::assert_that(
    is.character(Title) & length(Title) == 1,
    msg = "The 'Title' parameter must be a character string of length 1 (can be json, too.)"
  )

  assertthat::assert_that(
    is.character(Description) & length(Description) == 1,
    msg = "The 'Description' field must be a character string of length 1 (can be json, too.)"
  )

  assertthat::assert_that(
    nchar(as.character(PublicationYear))==4 & PublicationYear %% 1 ==0,
    msg = "The PublicationYear must be a four-digit number without decimals."
  )

  assertthat::assert_that(
    ResourceType == "Dataset",
    msg = "The ResourceType must be set to 'Dataset'."
  )

  assertthat::assert_that(
    nchar(as.character(Language))==3,
    msg = "The 'Language' field must be a three-character language code."
  )

  assertthat::assert_that(
    is.character(RelatedIdentifier) & length(RelatedIdentifier) == 1,
    msg = "The 'RelatedIdentifier' field must be a character string of length 1 (can be json, too.)"
  )

  assertthat::assert_that(
    is.character(Rights) & length(Rights) == 1,
    msg = "The 'Rights' field must be a character string of length 1 in json format, if not missing."
  )

  assertthat::assert_that(
    is.character(Size) & length(Size) == 1,
    msg = "The 'Size' field must be a character string of length 1 (can be json, too.)"
  )

  assertthat::assert_that(
    is.character(Format) & length(Format) == 1,
    msg = "The 'Format'field must be a character string of length 1 (can be json, too.)"
  )

  assertthat::assert_that(
    is.character(Version) & length(Version) == 1,
    msg = "The 'Version' field must be a character string of length 1."
  )

  assertthat::assert_that(
    is.character(GeoLocation) & length(GeoLocation) == 1,
    msg = "The 'GeoLocation' field must be a character string of length 1 (can be json, too.)"
  )

  assertthat::assert_that(
    is.character(FundingReference) & length(FundingReference) == 1,
    msg = "The 'FundingReference' field must be a character string of length 1 (can be json, too.)"
  )

  assertthat::assert_that(
    is.character(RelatedItem) & length(RelatedItem) == 1,
    msg = "The 'RelatedItem' field must be a character string of length 1 (can be json, too.)"
  )
}
