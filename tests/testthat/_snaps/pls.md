# check_args() works

    Code
      spec <- set_mode(set_engine(parsnip::pls(num_comp = -1), "mixOmics"),
      "regression")
      fit(spec, mpg ~ ., data = mtcars)
    Condition
      Error in `fit()`:
      ! `num_comp` must be a whole number larger than or equal to 0 or `NULL`, not the number -1.

