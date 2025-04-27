test_that("mixOmics classification model fitting", {
  skip_if_not_installed("modeldata")
  skip_on_cran()

  # ------------------------------------------------------------------------------

  uni_model <- mixOmics::splsda(
    pen_x_tr,
    pen_y_tr,
    ncomp = 3,
    keepX = rep(2, 3)
  )

  # ------------------------------------------------------------------------------

  pls_spec <-
    parsnip::pls(num_comp = 3, predictor_prop = 0.5) |>
    set_engine("mixOmics") |>
    set_mode("classification")

  # ------------------------------------------------------------------------------

  expect_error(
    parsnip_spls_da <-
      parsnip::pls(num_comp = 3, predictor_prop = 0.5) |>
      set_engine("mixOmics") |>
      set_mode("classification") |>
      fit_xy(x = pen_x_tr, y = pen_y_tr),
    regexp = NA
  )

  expect_equal(parsnip_spls_da$fit$loadings, uni_model$loadings)

  expect_error(
    parsnip_spls_da_class <- predict(parsnip_spls_da, as.data.frame(pen_x_te)),
    regexp = NA
  )

  uni_model_pred <- predict(uni_model, pen_x_te)$class$mahalanobis.dist[, 3]
  uni_model_pred <- unname(uni_model_pred)
  uni_model_pred <- factor(uni_model_pred, levels = species_lvls)

  expect_equal(names(parsnip_spls_da_class), ".pred_class")
  expect_equal(parsnip_spls_da_class[[1]], uni_model_pred)

  expect_error(
    parsnip_spls_da_prob <- predict(
      parsnip_spls_da,
      as.data.frame(pen_x_te),
      type = "prob"
    ),
    regexp = NA
  )

  uni_model_pred <- predict(uni_model, pen_x_te)$predict[,, 3]
  uni_model_pred <- tibble::as_tibble(uni_model_pred)
  names(uni_model_pred) <- paste0(".pred_", names(uni_model_pred))

  expect_equal(names(parsnip_spls_da_class), ".pred_class", ignore_attr = TRUE)

  # ----------------------------------------------------------------------------
  # multi-predict classes

  expect_error(
    parsnip_spls_da_class_mp <- multi_predict(
      parsnip_spls_da,
      as.data.frame(pen_x_te),
      num_comp = 2:3
    ),
    regexp = NA
  )
  expect_equal(nrow(parsnip_spls_da_class_mp), nrow(pen_x_te))
  expect_equal(nrow(parsnip_spls_da_class_mp$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_spls_da_class_mp$.pred[[1]]),
    c("num_comp", ".pred_class")
  )

  mo_spls_pred_9 <- predict(uni_model, pen_x_te)$class$mahalanobis.dist[9, 2:3]
  mo_spls_pred_9 <-
    tibble::tibble(
      num_com = 2:3,
      .pred_class = factor(mo_spls_pred_9, levels = species_lvls)
    )

  expect_equal(
    as.data.frame(parsnip_spls_da_class_mp$.pred[[9]]),
    as.data.frame(mo_spls_pred_9),
    ignore_attr = TRUE
  )

  # ----------------------------------------------------------------------------
  # multi-predict probs

  expect_error(
    parsnip_spls_da_prob_mp <-
      multi_predict(
        parsnip_spls_da,
        as.data.frame(pen_x_te),
        type = "prob",
        num_comp = 2:3
      ),
    regexp = NA
  )
  expect_equal(nrow(parsnip_spls_da_prob_mp), nrow(pen_x_te))
  expect_equal(nrow(parsnip_spls_da_prob_mp$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_spls_da_prob_mp$.pred[[1]]),
    c("num_comp", paste0(".pred_", species_lvls))
  )

  mo_spls_pred_9 <- predict(uni_model, pen_x_te)$predict[9, , 2:3]
  mo_spls_pred_9 <- t(apply(mo_spls_pred_9, 2, plsmod:::smax))
  mo_spls_pred_9 <- as.data.frame(mo_spls_pred_9)
  mo_spls_pred_9 <-
    tibble::tibble(
      num_com = 2:3,
      .pred_setosa = mo_spls_pred_9$Adelie,
      .pred_versicolor = mo_spls_pred_9$Chinstrap,
      .pred_virginica = mo_spls_pred_9$Gentoo
    )

  expect_equal(
    as.data.frame(parsnip_spls_da_prob_mp$.pred[[9]]),
    as.data.frame(mo_spls_pred_9),
    ignore_attr = TRUE
  )
})
