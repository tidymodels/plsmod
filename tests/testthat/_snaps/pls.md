# check_args() works

    Code
      spec <- parsnip::pls(num_comp = -1) %>% set_engine("mixOmics") %>% set_mode(
        "regression")
      fit(spec, mpg ~ ., data = mtcars)
    Condition
      Error in `fit()`:
      ! `num_comp` must be a whole number larger than or equal to 0 or `NULL`, not the number -1.

