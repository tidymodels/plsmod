test_that('check_args() works', {
  skip_on_cran()
  skip_if_not_installed("parsnip", "1.2.1.9001")

  expect_snapshot(
    error = TRUE,
    {
      spec <- parsnip::pls(num_comp = -1) |>
        set_engine("mixOmics") |>
        set_mode("regression")
      fit(spec, mpg ~ ., data = mtcars)
    }
  )
})
