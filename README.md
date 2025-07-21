<!-- README.md is generated from README.Rmd. Please edit that file -->

# 📊 HIViz  <img src="man/figures/hex.png" align="right" width="22%"/>

An R package for interactive visualization and exploration of key
HIV/AIDS indicators, including prevalence, incidence, mortality, and
treatment coverage, via a Shiny dashboard.

<!-- badges: start -->
![CRAN](https://www.r-pkg.org/badges/version/HIViz) ![Monthly
downloads](https://cranlogs.r-pkg.org/badges/last-month/HIViz) ![Total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/HIViz)
![License](https://img.shields.io/badge/license-GPL--3-yellow?style=flat)

<!-- badges: end -->
## 📁 Required Dataset Structure

The input dataset must include the following columns with exact names:

- country: (e.g., Iran, Spain)
- year: Numeric year (e.g., 2010, 2021)
- sex: Gender classification (e.g., Male, Female)
- age_group: Age brackets (e.g., 15–24, 25–34)
- hiv_prevalence: Estimated HIV prevalence (%)
- hiv_incidence: Number of new HIV cases
- aids_deaths: AIDS-related deaths
- plhiv: People living with HIV
- art_coverage: ART (antiretroviral therapy) coverage rate
- testing_coverage: HIV testing coverage (%)
- causes: Categorical variable describing cause of transmission (e.g.,
  unprotected sex, drug use, mother-to-child)

All column names must be spelled exactly as listed. The causes column
should contain clear, human-readable labels for transmission modes.

## Installation

The development version of the 'rencher' package is available for installation. You can install it from 
[Github](https://github.com/Atefehrashidi/HIViz) using:

``` r
# install.packages("remotes")
remotes::install_github("AtefehRashidi/HIViz")
```

## 🤝 Contributing

Issues and pull requests are welcome. Please open an issue to report
bugs or suggest enhancements.
