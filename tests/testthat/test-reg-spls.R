test_that("mixOmics multivariate model fitting", {
  skip_if_not_installed("modeldata")
  skip_on_cran()

  # ------------------------------------------------------------------------------

  multi_model <- mixOmics::spls(
    meats_x_tr,
    meats_y_tr,
    ncomp = 3,
    keepX = rep(100, 3)
  )

  # ------------------------------------------------------------------------------

  expect_no_error(
    parsnip_spls_multi <-
      parsnip::pls(num_comp = 3, predictor_prop = 1) |>
      set_engine("mixOmics") |>
      set_mode("regression") |>
      fit_xy(x = meats_x_tr, y = meats_y_tr)
  )

  expect_equal(parsnip_spls_multi$fit$loadings, multi_model$loadings)

  expect_no_error(
    parsnip_spls_multi_num <- predict(
      parsnip_spls_multi,
      as.data.frame(meats_x_te)
    )
  )

  mo_spls_pred <- predict(multi_model, meats_x_te)$predict[,, 3]
  mo_spls_pred <- tibble::as_tibble(mo_spls_pred)
  names(mo_spls_pred) <- paste0(".pred_", names(mo_spls_pred))

  expect_equal(
    as.data.frame(parsnip_spls_multi_num),
    as.data.frame(mo_spls_pred),
    ignore_attr = TRUE
  )

  # ----------------------------------------------------------------------------
  # multi-predict

  expect_no_error(
    parsnip_spls_multi_pred_num <-
      multi_predict(
        parsnip_spls_multi,
        as.data.frame(meats_x_te),
        num_comp = 1:2
      )
  )
  expect_equal(nrow(parsnip_spls_multi_pred_num), nrow(meats_x_te))
  expect_equal(nrow(parsnip_spls_multi_pred_num$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_spls_multi_pred_num$.pred[[1]]),
    c("num_comp", ".pred_water", ".pred_fat", ".pred_protein")
  )

  mo_spls_pred_9 <- t(predict(multi_model, meats_x_te)$predict[9, , 1:2])
  mo_spls_pred_9 <- tibble::as_tibble(mo_spls_pred_9)
  names(mo_spls_pred_9) <- paste0(".pred_", names(mo_spls_pred_9))

  expect_equal(
    as.data.frame(parsnip_spls_multi_pred_num$.pred[[9]][, -1]),
    as.data.frame(mo_spls_pred_9),
    ignore_attr = TRUE
  )
})

# ------------------------------------------------------------------------------

test_that("mixOmics univariate model fitting", {
  skip_if_not_installed("modeldata")
  skip_on_cran()

  # ------------------------------------------------------------------------------

  uni_model <- mixOmics::spls(
    meats_x_tr,
    meats_y_tr[[1]],
    ncomp = 3,
    keepX = rep(100, 3)
  )

  # ------------------------------------------------------------------------------

  expect_no_error(
    parsnip_spls_uni <-
      parsnip::pls(num_comp = 3, predictor_prop = 1) |>
      set_engine("mixOmics") |>
      set_mode("regression") |>
      fit_xy(x = meats_x_tr, y = meats_y_tr[[1]])
  )

  expect_equal(parsnip_spls_uni$fit$loadings, uni_model$loadings)

  expect_no_error(
    parsnip_spls_uni_num <- predict(
      parsnip_spls_uni,
      as.data.frame(meats_x_te)
    )
  )

  mo_spls_pred <- predict(uni_model, meats_x_te)$predict[,, 3]
  mo_spls_pred <- tibble::as_tibble(mo_spls_pred)
  names(mo_spls_pred) <- ".pred"

  expect_equal(
    as.data.frame(parsnip_spls_uni_num),
    as.data.frame(mo_spls_pred),
    ignore_attr = TRUE
  )

  # ----------------------------------------------------------------------------
  # multi-predict

  expect_no_error(
    parsnip_spls_multi_pred_num <-
      multi_predict(
        parsnip_spls_uni,
        as.data.frame(meats_x_te),
        num_comp = 1:2
      )
  )
  expect_equal(nrow(parsnip_spls_multi_pred_num), nrow(meats_x_te))
  expect_equal(nrow(parsnip_spls_multi_pred_num$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_spls_multi_pred_num$.pred[[1]]),
    c("num_comp", ".pred")
  )

  mo_spls_pred_9 <- predict(uni_model, meats_x_te)$predict[9, , 1:2]
  mo_spls_pred_9 <- tibble::tibble(.pred = mo_spls_pred_9)

  expect_equal(
    as.data.frame(parsnip_spls_multi_pred_num$.pred[[9]][, -1]),
    as.data.frame(mo_spls_pred_9),
    ignore_attr = TRUE
  )
})
