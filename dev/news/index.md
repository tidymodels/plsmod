# Changelog

## plsmod (development version)

- Transition from the magrittr pipe to the base R pipe.

## plsmod 1.0.0

CRAN release: 2022-09-06

- Small release to correct HTML tags for CRAN.

## plsmod 0.2.0

CRAN release: 2022-03-09

- Model definition functions
  (e.g. [`pls()`](https://parsnip.tidymodels.org/reference/pls.html))
  were moved to the parsnip package.

## plsmod 0.1.2

CRAN release: 2022-02-07

- Release to stop CRAN failures on r-devel from mixOmics.

## plsmod 0.1.1

CRAN release: 2020-10-28

- Small updates so that `plsmod` can be run in parallel using psock
  clusters

- Updates for encoding requirements related to current version of
  `parsnip`.

## plsmod 0.1.0

CRAN release: 2020-10-07

- The `num_terms` argument was removed in favor of `predictor_prop`,
  which is the maximum *proportion* of original predictors that can have
  non-zero coefficients for each PLS component. This makes it easier to
  tune and consistent with `recipes::step_pls()`.

- Fixed a bug that could prevent
  [`multi_predict()`](https://parsnip.tidymodels.org/reference/multi_predict.html)
  from being invoked.

## plsmod 0.0.1

CRAN release: 2020-04-14

- Added a `NEWS.md` file to track changes to the package.
