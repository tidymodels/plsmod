# Tidy methods for pls and spls objects

Tidy methods for pls and spls objects

## Usage

``` r
# S3 method for class 'mixo_pls'
tidy(x, ...)

# S3 method for class 'mixo_spls'
tidy(x, ...)
```

## Arguments

- x:

  An object with class `mixo_pls` or `mixo_spls`.

- ...:

  Not currently used.

## Value

A tibble with columns `terms` (the predictor names), `value` (the
loadings), `type` (either "predictors" or "outcomes"), and `component`
(the component number).
