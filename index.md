# eatPrepTBA

The goal of eatPrepTBA is to provide wrapper functions to interact with
the IQB Studio and the IQB Testcenter APIs. Moreover, it provides some
routines for preparing and blending data from these different resources
to manage TBA studies.

## Package governance

This package was originally authored and created by Philipp Franikowski.
**Current package maintenance** is provided by Karoline A. Sachse,
Kristoph Schumann, and Jakob Schaefer.

## Installation

You can install the development version of eatPrepTBA from
[GitHub](https://github.com/iqb-research/eatPrepTBA) with:

``` r

install.packages("pak") # once
pak::pak("iqb-research/eatPrepTBA")
```

## Getting started

Start with the [Studio
introduction](https://iqb-research.github.io/eatPrepTBA/articles/eatPrepTBA.html)
to connect to a workspace and retrieve units. For response coding,
missing values, and a scaling data set, see the [standard
workflow](https://iqb-research.github.io/eatPrepTBA/articles/standard-workflow.html)
(in German).

### Workshop materials

The [eatPrepTBA
workshop](https://sachseka.github.io/eatPrepTBAworkshop/) (in German;
Karoline Sachse, IQB Methoden-Team, 26 October 2026) provides slides, a
handout, and commented R scripts with synthetic example data. It covers
package structure and APIs, response coding and scaling data, selected
log analyses, and codebooks. Basic R knowledge is required; the local
examples need no IQB-Studio or Testcenter account.
