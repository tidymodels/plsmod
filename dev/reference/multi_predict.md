# Model predictions across many sub-models

Model predictions across many sub-models

## Usage

``` r
# S3 method for class '`_mixo_pls`'
multi_predict(object, new_data, num_comp = NULL, type = NULL, ...)

# S3 method for class '`_mixo_spls`'
multi_predict(object, new_data, num_comp = NULL, type = NULL, ...)

# S3 method for class '`_mixo_plsda`'
multi_predict(object, new_data, num_comp = NULL, type = NULL, ...)

# S3 method for class '`_mixo_splsda`'
multi_predict(object, new_data, num_comp = NULL, type = NULL, ...)
```

## Arguments

- object:

  An object of class `model_fit`

- new_data:

  A rectangular data object, such as a data frame.

- num_comp:

  An integer vector for the number of PLS terms to retain.

- type:

  A single character value or `NULL`. Possible values are "numeric",
  "class", or "prob". When `NULL`,
  [`predict()`](https://rdrr.io/r/stats/predict.html) will choose an
  appropriate value based on the model's mode.

- ...:

  Not currently used.
