metadata_eurostat <- function ( datasets,
                              Identifier,
                              Creator,
                              Contributor,
                              Title,
                              Description = NA_character_,
                              Publisher = "Green Deal Data Observatory, Reprex",
                              PublicationYear = 2021,
                              ResourceType = "Dataset",
                              Date = Sys.Date(),
                              Language = "eng",
                              RelatedIdentifier = NA_character_,
                              Size = NA_character_,
                              Format = NA_character_,
                              Version = NA_character_,
                              Rights,
                              GeoLocation = NA_character_,
                              FundingReference = NA_character_ ) {


  code = unique (datasets$dataset_code)[2]

  Description <- datasets %>%
    dplyr::distinct ( .data$dataset_code ) %>%
    mutate ( GeoLocation = NA_character_ )

  codes <-unique (Description$dataset_code)


  for ( i in seq_along(codes) ) {

    dataset <- datasets %>% filter ( .data$dataset_code == codes[i] )

    countries_df <- tibble::tibble(
      geo = dataset$geo,
      geo_name = countrycode::countrycode(dataset$geo, origin = 'iso2c', destination = 'country.name')
    ) %>%
      mutate ( geo_name = ifelse(is.na(.data$geo_name), "missing geographical name", .data$geo_name))




    Description$GeoLocation[i] <- add_geolocation(dataset %>%
                                                 left_join ( countries_df, by = 'geo'),
                                               geo_name_var = "geo_name")

  }


  metadata_create( dataset_code,
                   Identifier,
                   Creator,
                   Contributor,
                   Title,
                   Description = NA_character_,
                   Publisher = "Green Deal Data Observatory, Reprex",
                   PublicationYear = 2021,
                   ResourceType = "Dataset",
                   Date = Sys.Date(),
                   Language = "eng",
                   RelatedIdentifier = NA_character_,
                   Size = NA_character_,
                   Format = NA_character_,
                   Version = NA_character_,
                   Rights,
                   GeoLocation = NA_character_,
                   FundingReference = NA_character_ )
}
