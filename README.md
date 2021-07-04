
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dataobservatory

<!-- badges: start -->

[![Project Status: Active. The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![Follow
author](https://img.shields.io/twitter/follow/antaldaniel.svg?style=social)](https://twitter.com/intent/follow?screen_name=antaldaniel)
[![Follow
rOpenGov](https://img.shields.io/twitter/follow/ropengov.svg?style=social)](https://twitter.com/intent/follow?screen_name=ropengov)
<!-- badges: end -->

The goal of dataobservatory is to …

## Installation

You can install the released version of dataobservatory from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("dataobservatory")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dataobservatory-eu/dataobservatory")
```

## Example

This is a basic example which shows you how to solve a common problem:

    #> Registered S3 method overwritten by 'quantmod':
    #>   method            from
    #>   as.zoo.data.frame zoo
    #> Registered S3 method overwritten by 'tune':
    #>   method                   from   
    #>   required_pkgs.model_spec parsnip

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

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

Please note that the `dataobservatory` project is released with a
[Contributor Code of
Conduct](https://www.contributor-covenant.org/version/2/0/code_of_conduct/).
By contributing to this project, you agree to abide by its terms.
