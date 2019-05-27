
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reportcards

## Overview

reportcards is a set of tools to assist in ecological report card
analytics.

The broad analysis pipeline described in this vignette is as follows:
The typical report card analysis pipeline invovles the following stages:
1. Import data - ecological data - guidelines data - hierarchical
structural data 2. Prepare data - add hierarchical structures to the
ecological data - merge all ecological data and guidelines 3. Explore
the data (QAQC) 4. Generate indices 5. Aggregate and summarise Scores
and Grades

The tools provided in this package are to facilitate stages 4 and 5.
Nevertheless, fabricated data are also included to provide
examples/templates for inputs into the routines. Furthermore, there is a
vignette that provides a full demonstration of using the provided data
to generate report cards scores and grades.

## Installation

At this stage, only the development version of this package is
available. The easiest way to install the package is from Github.

``` r
# install.packages("devtools")
devtools::install_github("AIMS/reportcards")
```
