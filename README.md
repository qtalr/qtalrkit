
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Quantitative Text Analysis for Linguistics Resources Kit

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/qtalr/qtalrkit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/qtalr/qtalrkit/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This website contains the documentation for the R package `qtalrkit` and
supplementary materials for the [“An Introduction to Quantitative Text
Analysis for Linguistics: Reproducible Research using
R”](https://qtalr.github.io/book) textbook.

## `qtalrkit` package

### Installation

<!-- You can install the released version of qtalrkit from [CRAN](https://CRAN.R-project.org) with: -->

You can install a development version of `qtalrkit` from
[GitHub](https://github.com/) with:

``` r
install.packages("remotes")
remotes::install_github("qtalr/qtalrkit")
```

### Load

Then load the package with:

``` r
library(qtalrkit)
```

Please consult the [package
documentation](https://qtalr.github.io/qtalrkit/reference) for more
information.

## Supplementary materials

### Swirl lessons

**Installation**

The swirl lessons can be downloaded within an R console by running:

``` r
install.packages("swirl")
library("swirl")
install_course_github("qtalr", "Lessons")
```

**Load and run**

To load and start a lesson run:

``` r
swirl()
```

and follow the instructions to get started and to select a lesson.

> **Updating the lessons**
>
> If you need to update the lessons, run:
>
> ``` r
> library("swirl")
>
> # Uninstall the course
> uninstall_course("Lessons")
>
> # Reinstall the course
> install_course_github("qtalr", "Lessons")
> ```

### Recipes and Guides

Other supplementary reading are appear in the [materials
section](https://qtalr.github.io/qtalrkit/articles) of this site.
