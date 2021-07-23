#' @title Add Creators
#'
#' @description Add one or more creator. The main researchers involved in
#' producing the data, or the authors of the publication, in priority order.
#'
#' @details
#' To supply multiple creators, repeat this property.\cr
#' Allowed values, examples, other constraints: \cr
#' May be a corporate/institutional or personal name. \cr
#' Dublin Core:
#' \href{https://www.dublincore.org/specifications/dublin-core/cross-domain-attribute/}{A Cross-Domain Attribute Set}
#'
#' DataCite Metadata Schema Mandatory Properties:
#' \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-mandatory-properties#2-creator}{DataCite Metadata Schema v4.4 Mandatory Properties 2 Creator}
#'
#' @inheritParams validate_name
#' @param givenName The given name(s) of the creator or contributor.
#' @param affiliation Optional affiliation of person.
#' @param orcid Optional ORCiD ID of the person.
#' @param gnd Optional GND identifier of the person.
#' @param creators Earlier created list of creators.  Defaults to \code{NULL}.
#' If the list exists, then the function will append and not re-create the list.
#' @param format Defaults to \code{'json'}, in which case a JSON-formatted single character
#' string is returned.  The alternative is \code{'list'}.
#' @family persons functions
#' @return A list of creators, in a list or in a JSON formatted character string.
#' @export

add_creators <- function(
  givenName, familyName,
  organizationalName = NULL,
  affiliation = NULL,
  orcid = NULL,
  gnd = NULL,
  nameType = "Person",
  creators = NULL,
  format = "json"){

  validated_name <- validate_name ( givenName = givenName,
                                    familyName = familyName,
                                    organizationalName = organizationalName,
                                    nameType = nameType )


  creator <- list(creatorName = validated_name$name,
                  nameType = validated_name$nameType )
  if(!is.null(affiliation)) creator <- c(creator, affiliation = affiliation)
  if(!is.null(orcid)) creator <- c(creator, orcid = orcid)
  if(!is.null(gnd)) contributor <- c(contributor, gnd = gnd)

  if ( is.null(creators)) {
    creators <- list(creator )
  } else {
    creators[[length(creators)+1]] <- creator
  }

  if (format=="json") {
    create_json_text(creators)
  } else {
    creators
  }
}

#' @title Add Contributors
#'
#' @description Add one or more contributor.
#'
#' @details
#' The institution or person responsible for collecting, managing,
#' distributing, or otherwise contributing to the development of the resource.
#' In DataCite, recommended for discovery:
#' \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-recommended-and-optional-properties#7-contributor}{DataCite Metadata Schema v4.4 Recommended and Optional Properties 7 Contributor}
#'
#'
#' @inheritParams add_creators
#' @param contributors Earlier created list of contributors.  Defaults to \code{NULL}.
#' If the list exists, then the function will append and not re-create the list.
#' @param contributorType A mandatory type of contribution from the person or organization.
#' Must be one of :ContactPerson,  DataCollector, DataCurator, DataManager, Distributor, Editor, Funder,
#' HostingInstitution, Producer, ProjectLeader, ProjectManager, ProjectMember, RegistrationAgency,
#' RegistrationAuthority, RelatedPerson, Researcher, ResearchGroup, RightsHolder, Supervisor, Sponsor,
#' WorkPackageLeader, Other.
#' @family persons functions
#' @return A list of contributors, in a list or in a JSON formatted character string.
#' @export
add_contributors <- function(givenName, familyName,
                            contributorType,
                            affiliation = NULL,
                            orcid = NULL, gnd = NULL,
                            nameType = "Person",
                            contributors = NULL,
                            format = "json"){

  allowedTypes <- c(
    # DataCite types
    "ContactPerson", "DataCollector", "DataCurator",
    "DataManager","Distributor", "Editor", "Funder",
    "HostingInstitution", "Producer", "ProjectLeader",
    "ProjectManager", "ProjectMember", "RegistrationAgency",
    "RegistrationAuthority", "RelatedPerson",
    "Researcher", "ResearchGroup", "RightsHolder","Supervisor",
    "Sponsor", "WorkPackageLeader", "Other")

  if(!(contributorType %in% allowedTypes)){
    stop(sprintf("The contributor type should be one value among values [%s]",
                 paste(allowedTypes, collapse=",")))
  }

  validated_name <- validate_name ( givenName = givenName,
                                    familyName = familyName,
                                    organizationalName = organizationalName,
                                    nameType = nameType )


  contributor <- list(contributorName = validated_name$name,
                      nameType = validated_name$nameType ,
                      contributorType = contributorType)

  if(!is.null(affiliation)) contributor <- c(contributor, affiliation = affiliation)
  if(!is.null(orcid)) contributor <- c(contributor, orcid = orcid)
  if(!is.null(gnd)) contributor <- c(contributor, gnd = gnd)

  if ( is.null(contributors)) {
    contributors <- list(contributor )
  } else {
    contributors[[length(contributors)+1]] <- contributor
  }

  if (format=="json") {
    create_json_text(contributors)
  } else {
    contributors
  }
}

#' @title Validate Names
#' @param givenName The given name(s) of the creator or contributor.
#' @param familyName The family name of the creator or contributor.
#' @param organizationalName If the \code{"nameType = 'Organizational'"} than
#' this parameter must be used.
#' @param nameType Defaults to \code{"Person"}, alternative is \code{"Organizational"}.
#' @importFrom snakecase to_title_case
#' @return A validated name as a character string.
#' @keywords internal
validate_name <- function (givenName,
                           familyName,
                           organizationalName,
                           nameType) {

  nameType <- snakecase::to_title_case(nameType)
  nameType <- ifelse ( grepl("Organi", nameType),
                       yes = "Organizational",
                       no  = nameType)

  allowedNameTypes <- c("Person", "Organizational")
  assertthat::assert_that (
    nameType %in% allowedNameTypes,
    msg = sprintf("The name type should be one value among values ['%s'].",
                  paste(allowedNameTypes, collapse="', '"))
  )

  if ( nameType == "Person") {
    name = paste(snakecase::to_title_case(familyName),
                 snakecase::to_title_case(givenName),
                 sep = ", ")
  } else {
    assertthat::assert_that (
      !is.null(organizationalName),
      msg = "You must set an organizational name if the nameType is not 'Person'"
    )
    assertthat::assert_that (
      !is.na(organizationalName),
      msg = "You must set an organizational name if the nameType is not 'Person'"
    )

    name <- organizationalName
  }
  list ( name = name,
         nameType = nameType)
}

#' @title Add description
#'
#' @description All additional information that does not
#' fit in any of the other categories. May be used for technical information.
#'
#' @details
#' See \href{https://support.datacite.org/docs/schema-optional-properties-v41#17-description}{DataCite Metadata Schema v4.4 Recommended and Optional Properties 14 Description}
#'
#' @param Abstract
#' A brief description of the resource and the context in which the resource was created.
#' Recommended for discovery.\cr
#' Use \code{<br>} to indicate a line break for improved rendering of multiple paragraphs,
#' but otherwise no html markup.
#' @param Methods The methodology employed for the study or research.
#' Recommended for discovery.
#' @param SeriesInformation Information about a repeating series, such as volume,
#' issue, number.
#' @param TableofContents A listing of the Table of Contents.
#' Use  \code{<br>} to indicate a line break for improved rendering of multiple paragraphs,
#' but otherwise no html markup.
#' @param TechnicalInfo Detailed information that may be associated with design,
#' implementation, operation, use, and/or maintenance of a process or system.
#' Defaults to adding \code{\link{add_sessioninfo}}.
#' @param Other Other description information that does not fit into an existing category.
#' @importFrom jsonlite toJSON
#' @return
#' @examples
#' add_description ("My Description")
#' @export

add_description <- function (
  Abstract,
  Methods = NULL,
  SeriesInformation = NULL,
  TableOfContents = NULL,
  TechnicalInfo = NULL,
  Other = NULL,
  format = "json") {

  Abstract <- as.character(Abstract)
  assertthat::assert_that( length(Abstract)==1,
                           msg = "The Absract property must be a single character string.")

  assertthat::assert_that( (is.null(Other)) | (length(Other)==1),
                           msg = "The Other property must be a single character string.")

  Description = list ( Abstract = Abstract )
  if(!is.null(Methods)) Description$Methods <- Methods
  if(!is.null(SeriesInformation)) Description$SeriesInformation <- SeriesInformation
  if(!is.null(TableOfContents)) Description$TableOfContents<-TableOfContents
  if(is.null(TechnicalInfo)) Description$TechnicalInfo <- add_sessioninfo()
  if(!is.null(Other)) Description$Other <- Other

  if ( format == "json") {
    jsonlite::toJSON(Description)
  } else {
    Description
  }
}

#' @title Add identifiders
#'
#' @description Add a unique string that identifies a resource.
#'
#' @details
#' The Identifier is a unique string that identifies a resource.
#' For software, determine whether the identifier is for a specific version of a piece of software,
#' or for all versions
#'
#' In Dublin Core, Resource Identifier:
#' \href{https://www.dublincore.org/specifications/dublin-core/cross-domain-attribute/}{A Cross-Domain Attribute Set}
#'
#' DataCite Metadata Schema Mandatory Properties, Identifer:
#' \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-mandatory-properties#1-identifier}{DataCite Metadata Schema v4.4 Mandatory Properties 1 Identifier}
#' @param id An identifier string.
#' @param URI A Uniform Resource Identifier (URI) is a unique sequence of characters that
#' identifies a logical or physical resource used by web technologies.
#' URIs may be used to identify anything, including real-world objects,
#' such as people and places, concepts, or information resources such as web pages and books.
#' @param DOI This is the only controlled item in DataCite.
#' @param dataset_code A short, unique, programatically usable, memorizable dataset
#' ID, given by an observatory curator.
#' @param Version A resource version, for example, for an R package.
#' @param idAtSource The id used in a source document.
#' @param Other Other identifier.
#' @param identifiers A list of identifiers that were set earlier. If not the default
#' \code{NULL} value, the function adds identifiers to this pre-exsisting list of
#' identifiers.
#' @param format Defaults to \code{'json'}, in which case a JSON-formatted single character
#' string is returned.  The alternative is \code{'list'}.
#' @return A list, a json text of identifier(s).
#' @export

add_identifiers <- function (
  id = NULL,
  dataset_code = NULL,
  URI = NULL,
  DOI = NULL,
  Version = NULL,
  idAtSource = NULL,
  Other = NULL,
  identifiers = NULL,
  format = "json") {

  assertthat::assert_that(
    format %in% c("list", "json"),
    msg = "The 'format' parameter must be either 'list' or 'json'."
  )

  Identifiers <- list (
    id = id,
    dataset_code = dataset_code,
    URI = URI,
    DOI = DOI,
    Version = Version,
    idAtSource = idAtSource,
    Other = Other
  )

  assertthat::assert_that(
    ! all( vapply ( Identifiers, is.null, logical(1))),
    msg = "All identifiers are NULL. At least one of them must be given."
  )

  Identifiers <- Identifiers [!vapply ( Identifiers, is.null, logical(1))]

  if ( !is.null(identifiers) ) {
    # There are earlier identifiders to add to a list
    if (all(vapply(identifiers, is.json, logical(1)))) {
      assertthat::assert_that(
        all(vapply(identifiers, validate_related_item, logical(1))),
        msg = "Every related item must be strictly formatted."
      )

      if ( is.json(identifiers) ) {
        identifiers <-jsonlite::fromJSON(identifiers)

        if ( format == "list") append ( list (identifiers), list(Identifiers)) else {

           jsonlite::toJSON( append ( list (identifiers), list(Identifiers)) )
        }
      }
    }
  } else if ( format == "list") {
    Identifiers
  } else if ( format == "json") {
    jsonlite::toJSON (as_tibble(Identifiers))
    } else {
    stop("The parameter='format' must be either 'list' or 'json'")
  }
}

#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @keywords internal
validate_identifier <- function (Identifier) {
  allowed_identifier_types <- c("id", "dataset_code", "URI", "DOI", "Version", "idAtSource", "Other")
  allowed_types <- paste(allowed_identifier_types, collapse = ", ")

  error_msg_wrong_parameters <- glue::glue(
    "The identifier object may have the following fields:{ allowed_types }.")
  error_msg_few_parameters <- glue::glue(
    "The identifier object may have at least two of the following fields:{ allowed_types }.")

  if ( is.json(Identifier) ) {
    identifier_list <- jsonlite::fromJSON(Identifier)
  } else {
    identifier_list <- Identifier
  }

  assertthat::assert_that(is.list(identifier_list),
                          msg = "The identifier must be given as a list or a json object.")

  assertthat::assert_that(length(identifier_list)>1,
                          msg = error_msg_few_parameters)

  not_allowed_id <- names(identifier_list) [! names(identifier_list) %in% allowed_identifier_types]
  error_msg_wrong_id <- glue::glue("{not_allowed_id} is not among allowed fields:{ allowed_types }.")

    assertthat::assert_that(
    length(not_allowed_id)==0,
    msg = error_msg_wrong_id )

}

#' @title Add related item
#'
#' @description Add observation status coding, such as actual, missing, estimated or
#' imputed values to a dataset.
#'
#' @details
#' In Dublin Core, Relation:
#' \href{https://www.dublincore.org/specifications/dublin-core/cross-domain-attribute/}{A Cross-Domain Attribute Set}
#'
#' DataCite Metadata Schema Optional Properties, relatedItem:
#' \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-recommended-and-optional-properties}{DataCite Metadata Schema v4.4 Recommended and Optional Properties}
#'
#' @param relatedItem The human readable title or name of the award (grant) as a character string.
#' @param relationType Any of \code{isCitedBy}, \code{Cites}, \code{isSupplementTo}, \code{IsSupplementedBy}, \code{IsDerivedFrom}, \code{isSupplementedBy}, \code{isNewVersionOf}, \code{isPreviousVersionOf}, \code{IsOriginalFormOf}, \code{IsVariantFormOf}, \code{Documents}, \code{IsDocumentedBy}, \code{References}, \code{IsReferencedBy}, \code{IsPublishedIn}, \code{IsPreviousVersionOf}, \code{IsNewVersionOf}, \code{IsVersionOf}, \code{HasVersion}, \code{IsMetadataFor}, \code{HasMetadata}, \code{isPartOf}, \code{IsSourceOf}, \code{hasPart}, \code{compiles}, \code{isCompiledBy}, \code{isIdenticalTo}, \code{isAlternateIdentifier}, \code{Obsoletes}, \code{IsObsoletedBy}, \code{Requires}, \code{IsRequiredBy}, \code{Reviews}, \code{IsReviewedBy}. \cr
#' @param relatedItemType \code{Audiovisual}, \code{Book}, \code{BookChapter}, \code{Collection}, \code{ComputationalNotebook}, \code{ConferencePaper}, \code{ConferenceProceeding}, \code{DataPaper}, \code{Dataset}, \code{Dissertation}, \code{Event}, \code{Image}, \code{InteractiveResource}, \code{Journal}, \code{JournalArticle}, \code{Model}, \code{OutputManagementPlan}, \code{PeerReview}, \code{PhysicalObject}, \code{Preprint}, \code{Report}, \code{Service}, \code{Software}, \code{Sound}, \code{Standard}, \code{Text}, \code{Workflow}, \code{Other}
#' @param relatedItemIdentifierType One of \code{ARK}, \code{arXiv}, \code{bibcode}, \code{DOI}, \code{EAN13}, \code{EISSN}, \code{Handle}, \code{IGSN}, \code{ISBN}, \code{ISSN}, \code{ISTC}, \code{LISSN}, \code{LSID}, \code{PMID}, \code{PURC}, \code{UPC}, \code{URL}, \code{URN}, \code{w3id}.
#' @param relatedItemIdentifer The identifider as a character string.
#'  \code{\link{add_identifiers}}.
#' @importFrom jsonlite toJSON
#' @examples
#' first_rel_item <- add_related_items (
#'      RelatedItem = "First Related Item",
#'      relatedItemType = "Dataset",
#'      relationType = "IsDerivedFrom",
#'      relatedItemIdentifierType = "Handle",
#'      relatedItemIdentifier = "first_rel_item",
#'      related_items = NULL,
#'   format = 'json' )
#'
#' add_related_items (
#'     RelatedItem = "Second Related Item",
#'     relatedItemType = "Dataset",
#'     relationType = "IsDerivedFrom",
#'     relatedItemIdentifierType = "Handle",
#'     relatedItemIdentifier = "second_rel_item",
#'     related_items = first_rel_item,
#'     format = 'json' )
#' @export
add_related_items <- function (
  RelatedItem,
  relatedItemType = "Dataset",
  relationType = "IsDerivedFrom",
  relatedItemIdentifier,
  relatedItemIdentifierType,
  related_items = NULL,
  format = 'json' ) {

  resource_types <- resource_types_datacite()
  relation_types <- relation_type_datacite()
  related_id_types <- related_identifiers_datacite()

  not_one_of_id_types <- paste (related_id_types, collapse = ", ")

  assertthat::assert_that(
    length(relatedItemIdentifierType) == 1 & tolower(relatedItemIdentifierType) %in% tolower(related_id_types),
    msg = glue::glue("relatedItemType must be one of {not_one_of_id_types}")
  )

  not_one_of_resources <- paste (resource_types, collapse = ", ")

  assertthat::assert_that(
    length(relatedItemType) == 1 & relatedItemType %in% resource_types,
    msg = glue::glue("relatedItemType must be one of {not_one_of_resources}")
  )

  not_one_of_relations <- paste (relation_types, collapse = ", ")

  assertthat::assert_that(
    length(relationType) == 1 & relationType %in% relation_types,
    msg = glue::glue("relationType must be one of {not_one_of_relations}")
  )


  RelatedItem <- tibble::tibble (
    scheme = relatedItemIdentifierType,
    id = relatedItemIdentifier,
    relationType = relationType,
    relatedItemType = relatedItemType
  )

  RelatedItem.JSON <- jsonlite::toJSON (jsonlite::unbox(RelatedItem))


  if ( !is.null(related_items) ) {
    if (all(vapply(related_items, is.json, logical(1)))) {

      if ( is.json(related_items) ) {
        RelatedItem  <- paste(c(RelatedItem.JSON, related_items), collapse = ", ")
      }

      if ( format == "list") {
        append(list(related_items), list(RelatedItem))
      } else {
        RelatedItem
      }
    }
  } else if ( format == "list") {
    RelatedItem
  }  else if ( format=="json") {
    RelatedItem.JSON
  } else{
    stop("The parameter='format' must be either 'list' or 'json'")
  }
}

#' @inheritParams add_related_items
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that
#' @keywords internal
validate_related_item <- function(RelatedItem) {

  if (is.json(RelatedItem)) RelatedItem <- jsonlite::fromJSON(RelatedItem)

  related_item_fields <- c("RelatedItem", "relatedItemType", "relationType", "relatedItemIdentifier")

  assertthat::assert_that(
    all(names (RelatedItem) %in% related_item_fields),
    msg = paste0("The related items must have the following fields: ",
                 paste(related_item_fields, collapse = ", "))
  )
}

#' @title Add Titles
#'
#' @description Add a main title, and optional subtitle, alternative- or translated title,
#' or other title.
#'
#' @details
#' Dublin Core:
#' \href{https://www.dublincore.org/specifications/dublin-core/cross-domain-attribute/}{A Cross-Domain Attribute Set}
#'
#' DataCite Metadata Schema Mandatory Properties:
#' \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-mandatory-properties}{DataCite Metadata Schema v4.4 Mandatory Properties}
#'
#' The \code{Title} field is mandatory in both DataCite and Dublin Core.  DataCite allows multiple
#' titles to be used, these can be added as a list.
#'
#'
#' @param title A character string containing the title of the Dataset.
#' It is mandatory and has no default value. This is part of the Dublin Core and
#' DataCite mandatory fields.
#' @param AlternativeTitle Defaults to \code{NULL}, and it is part of the optional
#' fields of DataCite.
#' @param Subtitle Defaults to \code{NULL}.
#' @param TranslatedTitle Defaults to \code{NULL}.
#' @param Other Defaults to \code{NULL}. Try avoiding its use.
#' @param titles A list of earlier set title(s).
#' @inheritParams add_creators
#' @family descriptive_metadata functions
#' @return A list of titles.
#' @examples{
#' add_titles(title = "My Title",
#'            AlternativeTitle = "Alternative Title",
#'            Subtitle = "This is a subtitle",
#'            TranslatedTitle = "Meine Titel",
#'            Other = "Other title")
#' }
#' @export

add_titles <- function (Title = NULL,
                        AlternativeTitle = NULL,
                        Subtitle = NULL,
                        TranslatedTitle = NULL,
                        Other = NULL,
                        titles = NULL,
                        format = "json") {


  assertthat::assert_that(
    format %in% c("list", "json"),
    msg = "The 'format' parameter must be either 'list' or 'json'."
  )

  if ( is.null(Title)) {
    # This can be used to add further titles, if titles already exists
    if (is.null(titles)) {
      stop("The main Title field must not be left empty.")
    }
  } else {
    title <- snakecase::to_title_case(as.character(Title))
    titles <- list ( Title = Title )
  }

  if ( !is.null(AlternativeTitle)) titles$AlternativeTitle <- AlternativeTitle
  if ( !is.null(Subtitle)) titles$Subtitle <- Subtitle
  if ( !is.null(TranslatedTitle)) titles$TranslatedTitle <- TranslatedTitle
  if ( !is.null(Other)) titles$Other <- Other

  if (format=="json") {
    jsonlite::toJSON(titles)
  } else {
    titles
  }
}


#' @title Add Dates
#'
#' @description Add dates to the administrative and descriptive metadata.
#'
#' @details See the controlled date items in
#' \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-recommended-and-optional-properties#8a-datetype}{DataCite Metaadata Schema 4.4}.
#'#'
#' @param Date The main date of the resource, unless otherwise stated, should equal
#' first issue.
#' @param Updated The date of the last update to the resource, when the resource is being added to. May be a range.
#' @param UpdatedAtSource The date of the last update to the resource at its original source. May be a range.
#' @param StructuralChangeAtSource The date of the last structural data chanage at source.
#' @param FirstObservation The date of the first actual observation (which is not estimated.)
#' @param LastObservation The date of the last actual observation (which is not forecasted.)
#' @param Accepted The date that the publisher accepted the resource into their system.
#' To indicate the start of an embargo period, use Submitted or Accepted, as appropriate.
#' @param Available  The date the resource is made publicly available. May be a range.
#' To indicate the end of an embargo period, use Available.
#' @param Copyrighted The specific, documented date at which the resource receives
#' a copyrighted status, if applicable.
#' @param Collected The date or date range in which the resource content was collected.
#' To indicate precise or particular timeframes in which research was conducted.
#' @param Created The date the resource itself was put together;
#' this could refer to a timeframe in ancient history, be a date range or a single date for a final component,
#' e.g., the finalized file with all of the data.
#' Recommended for discovery.
#' @param Issued The date that the resource is published or distributed e.g. to a data centre.
#' @param Submitted The date the creator submits the resource to the publisher.
#' This could be different from Accepted if the publisher then applies a selection process.
#' To indicate the start of an embargo period, use Submitted or Accepted, as appropriate.
#' Recommended for discovery.
#' @param Valid The date or date range during which the dataset or resource is accurate.
#' @param Withdrawn The date the resource is removed. It's good practice to indicate the reason for retraction
#' of withdrawal in the descriptionType.
#' @param format format Defaults to \code{'json'}, in which case a JSON-formatted single character
#' string is returned.  The alternative is \code{'list'} or \code{'character'} (for single Date only.)
#' @examples
#' add_dates()
#' @export

add_dates <- function ( Date = Sys.Date(),
                        Updated = Sys.Date(),
                        UpdatedAtSource = NULL,
                        Collected = NULL,
                        EarliestObservation = NULL,
                        LatestObservation = NULL,
                        StructuralChangeAtSource = NULL,
                        Accepted = NULL,
                        Available = NULL,
                        Created = NULL,
                        Copyrighted = NULL,
                        Issued = NULL,
                        Submitted  = NULL,
                        Valid = NULL,
                        Withdrawn = NULL,
                        dates = NULL,
                        format = "json") {

  assertthat::assert_that(
    format %in% c("list", "json"),
    msg = "The 'format' parameter must be either 'list','json' or 'character'."
  )

  if ( is.null(Date)) {
    # This can be used to add further titles, if titles already exists
    if (is.null(dates)) {
      stop("The main Date field must not be left empty.")
    }
  } else {
    Updated <- as.character(Updated)
    dates <- list ( Updated = Updated )
  }

  if (!is.null(UpdatedAtSource)) dates$UpdatedAtSource <- UpdatedAtSource
  if (!is.null(Collected)) dates$Collected <- Collected
  if (!is.null(EarliestObservation)) dates$EarliestObservation <- EarliestObservation
  if (!is.null(LatestObservation)) dates$LatestObservation <- LatestObservation
  if (!is.null(LatestObservation)) dates$LatestObservation <- LatestObservation
  if (!is.null(Accepted)) dates$Accepted <- Accepted
  if (!is.null(Copyrighted)) dates$Copyrighted <- Copyrighted
  if (!is.null(Issued)) dates$Issued <- Issued
  if (!is.null(Submitted)) dates$Submitted <- Submitted
  if (!is.null(Valid)) dates$Valid <- Valid
  if (!is.null(Withdrawn)) dates$Withdrawn <- Withdrawn

  dates

  if (format=="json") {
    jsonlite::toJSON(dates)
  } else if (format=="list") {
    dates
  } else {
    as.character(Updated)
  }
}


#' @title Add Rights
#'
#' @description Add access rights information to the metadata.
#' @details
#' In Dublin Core, Rights Management:
#' \href{https://www.dublincore.org/specifications/dublin-core/cross-domain-attribute/}{A Cross-Domain Attribute Set}
#'
#' DataCite Metadata Schema Optional Properties, Rights:
#' \href{https://support.datacite.org/docs/datacite-metadata-schema-v44-recommended-and-optional-properties}{DataCite Metadata Schema v4.4 Recommended and Optional Properties}
#'
#'
#' Usually you do not have to fill in \code{rigthsURI}. We only use
#'     standard rights definitions in \href{https://spdx.org/licenses/}{SPDX License List}.
#'
#'
#' @param rightsIdentifier An identifier of the applicable use right, defaults to
#' \code{'CC-BY-NC-SA-4.0'}.
#' \href{https://spdx.org/licenses/}{SPDX License List}
#' @param rigthsURI A URL to the definition of the rigth. Automatically created from
#' \href{https://spdx.org/licenses/}{SPDX License List}.
#' Defaults to \code{NULL}, when the download will be called with \code{id}.
#' @inheritParams add_creators
#' @examples
#' add_rights( 'CC-BY-NC-SA-4.0')
#' @export

add_rights <- function ( rightsIdentifier = 'CC-BY-NC-SA-4.0',
                         rightsURI = NULL,
                         format = "json") {

  assertthat::assert_that(
    format %in% c("list", "json"),
    msg = "The 'format' parameter must be either 'list','json' or 'character'."
  )


  if (is.null(rightsURI)) {
    rightsURL <- glue::glue("https://spdx.org/licenses/{rightsIdentifier}.html")
  } else {
    rightURL <- rightsURI
  }

  assertthat::assert_that(
    RCurl::url.exists(rightsURL),
    msg = glue::glue("The rigthsURI={rightsURL} is not available")
  )

  Rigths <- list ( rightsIdentifier = 'CC-BY-NC-SA-4.0',
                   rightsURI = rightsURL)

  if ( format == "list") {
    Rigths
  } else {
    jsonlite::toJSON(Rigths)
  }
}

#' @title Add Size
#'
#' @description Add the file size of the dataset (in CSV format.)
#' @details The file will be written to a temporary directory and its sized measured.
#' It also serves validation purposes for the dataset.
#' @param dat The dataset that needs size information.
#' @return A string explaining the file size in CSV format.
#' @family descriptive_metadata functions
#' @export

add_size <- function(dat) {
  tmp <- tempfile()
  write.csv(dat, tmp)
  size_value <- paste0( file.size(tmp), " bytes in CSV format")
  file.remove(tmp)
  size_value
}


#' @title Add Geolocation
#'
#' @description Add the spatial region or named place where
#' the data was gathered or about which the data is focused.
#'
#' @param dat The dataset that needs size information.
#' @param geo_var Defaults to \code{geo}.
#' @inheritParams add_creators
#' @family descriptive_metadata functions
#' @return A string explaining the file size in CSV format.
#' @export

add_geolocation <- function(dat,
                            geo_var = 'geo',
                            geo_name_var = NULL,
                            format = 'json') {

  assertthat::assert_that(
    format %in% c("list", "json"),
    msg = "The 'format' parameter must be either 'list','json' or 'character'."
  )


  geoLocationPlace <- NA_character_
  geoCodes <- NA_character_

  if (!is.null(geo_name_var)) {
    if ( geo_name_var %in% names(dat)) {
      geoLocationPlace <- paste(unique(as.character(unlist(dat[, geo_name_var]))), collapse="|")
    }
  }


  if ( geo_var %in% names(dat)) {
    geoCodes = paste(unique(as.character(unlist(dat[, geo_var]))), collapse="|")
  }

  GeoLocation <-list ( geoLocationPlace = geoLocationPlace,
                       geoCodes = geoCodes )

  if ( format == "list") {
    GeoLocation
  } else {
    jsonlite::toJSON(GeoLocation)
  }
}


#' @title Valid related identifiers
#' @keywords internal
related_identifiers_datacite <- function() {
  c("ARK", "arXiv", "bibcode", "DOI", "EAN13", "EISSN", "Handle",
    "IGSN", "ISBN", "ISSN", "ISTC", "LISSN", "LSID", "PMID", "PURC", "UPC",
    "URL", "URN", "w3id")
}

