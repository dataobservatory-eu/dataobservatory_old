
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dataobservatory

<!-- badges: start -->

[![Project Status: Active. The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5068765)](https://doi.org/10.5281/zenodo.5068765)
[![Codecov test
coverage](https://codecov.io/gh/dataobservatory-eu/dataobservatory/branch/master/graph/badge.svg)](https://codecov.io/gh/dataobservatory-eu/dataobservatory?branch=master)
[![Follow
author](https://img.shields.io/twitter/follow/antaldaniel.svg?style=social)](https://twitter.com/intent/follow?screen_name=antaldaniel)
[![Follow
rOpenGov](https://img.shields.io/twitter/follow/ropengov.svg?style=social)](https://twitter.com/intent/follow?screen_name=ropengov)
<!-- badges: end -->

The goal of dataobservatory is to facilitate the automated
documentation, and the automated recording of descriptive and
administrative (statistical processing) metadata for datasets. It also
helps recording information about the computational environment to
increase reproducability.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dataobservatory-eu/dataobservatory")
```

## The dataset Class

The `dataset` S3 class is an extension of the data frame and tibble
class. It has some important metadata attributes that facilitate the
automated documentation of the dataset. Furthermore, it has an adequate
`print` and `summary` method.

    #> Registered S3 method overwritten by 'quantmod':
    #>   method            from
    #>   as.zoo.data.frame zoo
    #> Registered S3 method overwritten by 'tune':
    #>   method                   from   
    #>   required_pkgs.model_spec parsnip

``` r
small_population_dataset
#> Population of Small European Countries
#> dataset code:  small_population_total 
#> Actual observation range: [2009-01-01]-[2020-01-01], updated on 2021-07-04.
#> Geographic coverage:  LI, AD, SM 
#> The first 10 observations of 23 (unit: number):
#> 
#>              dataset_code       time geo value unit obs_status freq
#> 1  small_population_total 2020-01-01  LI 38747   NR          A    A
#> 2  small_population_total 2019-01-01  AD 76177   NR          A    A
#> 3  small_population_total 2019-01-01  LI 38378   NR          A    A
#> 4  small_population_total 2018-01-01  LI 38114   NR          A    A
#> 5  small_population_total 2018-01-01  SM 34453   NR          A    A
#> 6  small_population_total 2017-01-01  LI 37810   NR          A    A
#> 7  small_population_total 2016-01-01  LI 37622   NR          A    A
#> 8  small_population_total 2015-01-01  LI 37366   NR          A    A
#> 9  small_population_total 2014-01-01  LI 37129   NR          A    A
#> 10 small_population_total 2013-01-01  AD 76246   NR          A    A
#> 
#> Source: greendeal.dataobservatory.eu
```

``` r
summary(small_population_dataset)
#> Population of Small European Countries
#> dataset code:  small_population_total 
#> Actual observation range: [2009-01-01]-[2020-01-01], updated on 2021-07-04.
#> Geographic coverage:  LI, AD, SM 
#> 
#>       time                value      
#>  Min.   :2009-01-01   Min.   :31269  
#>  1st Qu.:2011-01-01   1st Qu.:35742  
#>  Median :2013-01-01   Median :37366  
#>  Mean   :2013-07-10   Mean   :47298  
#>  3rd Qu.:2016-07-02   3rd Qu.:57462  
#>  Max.   :2020-01-01   Max.   :84484  
#> Source: greendeal.dataobservatory.eu
```

``` r
small_population_datacite <- datacite_dataset(
  dataset = small_population_dataset,
  Subject = "Demography",
  Creator = "Joe, Doe")
```

## Descriptive Metadata

The `datacite` class (see `?datacite()`) is a modification of a data
frame (tibble) object, and it creates all the mandatory and recommended
fields of the
[DataCite](https://support.datacite.org/docs/datacite-metadata-schema-v44-properties-overview)
metadata schema for a dataset. It also covers all the properties in the
more general Dublin Core standard, but in some cases, the property name
is different (and follows the DataCite naming convention.)

The descriptive metadata can be added with the `datacite()` constructor
(see: `?datacite` ) or the `datacite_dataset()` helper function. or read
the [DataCite Descriptive
Metadata](http://r.dataobservatory.eu/articles/datacite.html) vignette
article.

The `datacite` class can automatically connected to many scientific
repositories, including Zenodo. In later versions, this will enable the
user to upload the new created dataset (version) and receive a digital
object identifier (version), or DOI(version) for the dataset.

See more about the metadata concepts applied in the [FAIR Data and the
Added Value of Rich
Metadata](https://contributors.dataobservatory.eu/FAIR-data.html)
chatper of the [Automated Observatory Contributors’
Handbook](https://contributors.dataobservatory.eu/).

``` r
print(small_population_datacite)
#> DataCite information for Population of Small European Countries 
#> # A tibble: 20 x 2
#>    Property        Value                                                        
#>    <chr>           <chr>                                                        
#>  1 dataset_code    "small_population_total"                                     
#>  2 Identifier      "small_population_total"                                     
#>  3 Creator         "Joe, Doe"                                                   
#>  4 Title           "Population of Small European Countries"                     
#>  5 Publisher       "Reprex"                                                     
#>  6 PublicationYear "2021"                                                       
#>  7 ResourceType    "Dataset"                                                    
#>  8 Subject         "Demography"                                                 
#>  9 Contributor      <NA>                                                        
#> 10 Date            "{\"Updated\":[\"2021-07-04\"],\"EarliestObservation\":[\"20~
#> 11 Language        "eng"                                                        
#> 12 RelatedIdentif~  <NA>                                                        
#> 13 Size            "1576 bytes in CSV format"                                   
#> 14 Format           <NA>                                                        
#> 15 Version          <NA>                                                        
#> 16 Rights          "{\"rightsIdentifier\":[\"CC-BY-NC-SA-4.0\"],\"rightsURI\":[~
#> 17 Description      <NA>                                                        
#> 18 GeoLocation     "{\"geoLocationPlace\":[null],\"geoCodes\":[\"LI|AD|SM\"]}"  
#> 19 FundingReferen~  <NA>                                                        
#> 20 RelatedItem      <NA>
```

## Administrative Metadata

The statistical processing information can be added with the not fully
implemented `codebook` class. Read the [The codebook
Class](http://r.dataobservatory.eu/articles/codebook.html) vignette
article.

The `codebook` S3 class (not yet fully documented and does not have yet
and independent constructor) records the statistical processing metadata
of a dataset.

It contains a full codebook following SDMX statistical metadata codelist
standards, furthermore, it records the Session Information of all
processing steps, and adds to the descriptive metadata the R packages or
software code that generated the results.

For example, the annual observations follow the [SDMX Code List for
Frequency 2.1 (CL\_FREQ)](https://sdmx.org/?page_id=3215/)) definition,
and they can be translated to the `ISO 8106` time metadata standard,
too.

``` r
add_frequency("A", "list")
#> $id
#> [1] "A"
#> 
#> $name
#> [1] "Annual"
#> 
#> $description
#> [1] "To be used for data collected or disseminated every year"
#> 
#> $iso8106
#> [1] "P1Y"
#> 
#> $RelatedItem
#> [1] "{\"RelatedItem\":[\"SDMX Code List for Frequency\"],\"relatedItemType\":[\"Dataset\"],\"relationType\":[\"IsDocumentedBy\"],\"relatedItemIdentifier\":[\"{\\\"id\\\":[\\\" CL_FREQ\\\"],\\\"dataset_code\\\":{},\\\"URI\\\":[\\\"https://sdmx.org/?page_id=3215/\\\"],\\\"DOI\\\":{},\\\"Version\\\":[\\\"2.1\\\"],\\\"idAtSource\\\":{},\\\"Other\\\":{}}\"]}"
```

``` r
add_sessioninfo()
#> [1] "{\"platform\":[\"x86_64-w64-mingw32/x64 (64-bit)\"],\"locale\":[\"LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252\"],\"running\":[\"Windows 10 x64 (build 17763)\"],\"RNGkind\":[\"Mersenne-Twister\",\"Inversion\",\"Rejection\"],\"basePkgs\":[\"stats\",\"graphics\",\"grDevices\",\"utils\",\"datasets\",\"methods\",\"base\"],\"matprod\":[\"default\"],\"BLAS\":[\"\"],\"LAPACK\":[\"\"],\"system.codepage\":[\"1250\"],\"codepage\":[\"1252\"]}"
```

## Contributor Code of Conduct

Please note that the `dataobservatory` project is released with a
[Contributor Code of
Conduct](https://www.contributor-covenant.org/version/2/0/code_of_conduct/).
By contributing to this project, you agree to abide by its terms.
