library(tibble)
library(modeldata)
library(mixOmics)
data("penguins")
penguins <- na.omit(penguins)

## -----------------------------------------------------------------------------

for_test <- 1:10
y_tr <- penguins$species[-for_test]

x_vars <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")
x_tr <- penguins[-for_test, x_vars]
x_te <- penguins[ for_test, x_vars]

uni_model <- mixOmics::plsda(x_tr, y_tr, ncomp = 3)

# ------------------------------------------------------------------------------

pls_spec <-
  parsnip::pls(num_comp = 3) %>%
  set_engine("mixOmics") %>%
  set_mode("classification")

# ------------------------------------------------------------------------------

test_that("classification model fitting", {
  # Failures on r-devel are upstream in mixOmics. See
  # https://github.com/mixOmicsTeam/mixOmics/issues/162
  skip_on_cran()
  skip_if(grepl("development", R.version$status))

  expect_error(
    parsnip_pls_da <-
      parsnip::pls(num_comp = 3) %>%
      set_engine("mixOmics") %>%
      set_mode("classification") %>%
      fit_xy(x = x_tr, y = y_tr),
    regexp = NA
  )

  expect_equal(parsnip_pls_da$fit$loadings, uni_model$loadings)

  expect_error(
    parsnip_pls_da_class <- predict(parsnip_pls_da, as.data.frame(x_te)),
    regexp = NA
  )

  uni_model_pred <- predict(uni_model, x_te)$class$mahalanobis.dist[, 3]
  uni_model_pred <- unname(uni_model_pred)
  uni_model_pred <- factor(uni_model_pred, levels = levels(penguins$species))

  expect_equal(names(parsnip_pls_da_class), ".pred_class")
  expect_equal(parsnip_pls_da_class[[1]], uni_model_pred)

  expect_error(
    parsnip_pls_da_prob <- predict(parsnip_pls_da, as.data.frame(x_te), type = "prob"),
    regexp = NA
  )

  uni_model_pred <- predict(uni_model, x_te)$predict[, , 3]
  uni_model_pred <- tibble::as_tibble(uni_model_pred)
  names(uni_model_pred) <- paste0(".pred_", names(uni_model_pred))

  expect_equivalent(names(parsnip_pls_da_class), ".pred_class")

  # ----------------------------------------------------------------------------
  # multi-predict classes

  expect_error(
    parsnip_pls_da_class_mp <- multi_predict(parsnip_pls_da, as.data.frame(x_te), num_comp = 2:3),
    regexp = NA
  )
  expect_equal(nrow(parsnip_pls_da_class_mp), nrow(x_te))
  expect_equal(nrow(parsnip_pls_da_class_mp$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_pls_da_class_mp$.pred[[1]]),
    c("num_comp", ".pred_class")
  )

  mo_pls_pred_9 <- predict(uni_model, x_te)$class$mahalanobis.dist[9, 2:3]
  mo_pls_pred_9 <-
    tibble(
      num_com = 2:3,
      .pred_class = factor(mo_pls_pred_9, levels = levels(penguins$species))
    )

  expect_equivalent(
    as.data.frame(parsnip_pls_da_class_mp$.pred[[9]]),
    as.data.frame(mo_pls_pred_9)
  )

  # ----------------------------------------------------------------------------
  # multi-predict probs

  expect_error(
    parsnip_pls_da_prob_mp <-
      multi_predict(
        parsnip_pls_da,
        as.data.frame(x_te),
        type = "prob",
        num_comp = 2:3
      ),
    regexp = NA
  )
  expect_equal(nrow(parsnip_pls_da_prob_mp), nrow(x_te))
  expect_equal(nrow(parsnip_pls_da_prob_mp$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_pls_da_prob_mp$.pred[[1]]),
    c("num_comp", paste0(".pred_", levels(penguins$species)))
  )

  mo_pls_pred_9 <- predict(uni_model, x_te)$predict[9, , 2:3]
  mo_pls_pred_9 <- t(apply(mo_pls_pred_9, 2, plsmod:::smax))
  mo_pls_pred_9 <- as.data.frame(mo_pls_pred_9)
  mo_pls_pred_9 <-
    tibble(
      num_com = 2:3,
      .pred_setosa = mo_pls_pred_9$Adelie,
      .pred_versicolor = mo_pls_pred_9$Chinstrap,
      .pred_virginica = mo_pls_pred_9$Gentoo
    )

  expect_equivalent(
    as.data.frame(parsnip_pls_da_prob_mp$.pred[[9]]),
    as.data.frame(mo_pls_pred_9)
  )
})
