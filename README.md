






<!-- README.md is generated from README.Rmd. Please edit that file -->

# Understanding Society Data Cleaning

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

<!-- badger::badge_doi("10.17605/OSF.IO/43N7P", "green") -->

[![](https://img.shields.io/badge/doi-10.17605/OSF.IO/6K9NV-green.svg)](https://doi.org/10.17605/OSF.IO/6K9NV)
<!-- badges: end -->

## Motivation

The motivation for `ukhlsclean` is to develop a set of standard
functions for processing raw data from the UK Household Longitudinal
Study, also known as Understanding Society, which is an ongoing
longitudinal survey of individuals and households representative of the
UK population. The survey collects a wide variety of demographic,
behavioural, health, and labour market information.

## Installation

`ukhlsclean` is open source and [available on
GitHub](https://github.com/STAPM/ukhlsclean). If you are on a Windows
machine you will need to [install
Rtools](https://www.rdocumentation.org/packages/installr/versions/0.22.0/topics/install.Rtools).  
Once that is sorted, you can install the latest version or a specified
version from GitHub with:

``` r
#install.packages("devtools")
#install.packages("getPass")
#install.packages("git2r")

devtools::install_git(
  "https://github.com/STAPM/ukhlsclean", 
  ref = "x.x.x",
  build_vignettes = FALSE
)

# Where uname is your Gitlab user name.
# ref is the version you want to install - remove for the latest version
```

## Citation

Morris D, and Kai Le Chen, R (2023). ukhlsclean: An R Package for Health
Survey Data Wrangling. R package version \[x.x.x\]. University of
Sheffield. <https://github.com/STAPM/ukhlsclean> . doi:
<https://doi.org/10.17605/OSF.IO/6K9NV>

## Projects

Some examples of projects making use of the `ukhlsclean` package are:

1.  [Smoking, drinking, and work
    outcomes](https://gitlab.com/SPECTRUM_Sheffield/projects/work-productivity).
    Here the package is used to create a clean longitudinal dataset of
    smoking and drinking behaviours, labour market outcomes, and the
    covariates needed to model labour market outcomes.
