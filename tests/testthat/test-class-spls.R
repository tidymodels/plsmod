context("spls classification models")

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

uni_model  <- mixOmics::splsda(x_tr, y_tr, ncomp = 3, keepX = rep(2, 4))

# ------------------------------------------------------------------------------

pls_spec <-
  plsmod::pls(num_comp = 3, predictor_prop = 0.5) %>%
  set_engine("mixOmics") %>%
  set_mode("classification")

# ------------------------------------------------------------------------------
# Model fitting tests

test_that('classification model fitting', {
  expect_error(
    parsnip_spls_da <-
      plsmod::pls(num_comp = 3, predictor_prop = 0.5) %>%
      set_engine("mixOmics") %>%
      set_mode("classification") %>%
      fit_xy(x = x_tr, y = y_tr),
    regexp = NA
  )

  expect_equal(parsnip_spls_da$fit$loadings, uni_model$loadings)

  expect_error(
    parsnip_spls_da_class <- predict(parsnip_spls_da, as.data.frame(x_te)),
    regexp = NA
  )

  uni_model_pred <- predict(uni_model, x_te)$class$mahalanobis.dist[,3]
  uni_model_pred <- unname(uni_model_pred)
  uni_model_pred <- factor(uni_model_pred, levels = levels(penguins$species))

  expect_equal(names(parsnip_spls_da_class), ".pred_class")
  expect_equal(parsnip_spls_da_class[[1]], uni_model_pred)

  expect_error(
    parsnip_spls_da_prob <- predict(parsnip_spls_da, as.data.frame(x_te), type = "prob"),
    regexp = NA
  )

  uni_model_pred <- predict(uni_model, x_te)$predict[,,3]
  uni_model_pred <- tibble::as_tibble(uni_model_pred)
  names(uni_model_pred) <- paste0(".pred_", names(uni_model_pred))

  expect_equivalent(names(parsnip_spls_da_class), ".pred_class")


  # ----------------------------------------------------------------------------
  # multi-predict classes

  expect_error(
    parsnip_spls_da_class_mp <- multi_predict(parsnip_spls_da, as.data.frame(x_te), num_comp = 2:3),
    regexp = NA
  )
  expect_equal(nrow(parsnip_spls_da_class_mp), nrow(x_te))
  expect_equal(nrow(parsnip_spls_da_class_mp$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_spls_da_class_mp$.pred[[1]]),
    c('num_comp', '.pred_class')
  )

  mo_spls_pred_9 <- predict(uni_model, x_te)$class$mahalanobis.dist[9,2:3]
  mo_spls_pred_9 <-
    tibble(num_com = 2:3,
           .pred_class = factor(mo_spls_pred_9, levels = levels(penguins$species)))

  expect_equivalent(
    as.data.frame(parsnip_spls_da_class_mp$.pred[[9]]),
    as.data.frame(mo_spls_pred_9)
  )

  # ----------------------------------------------------------------------------
  # multi-predict probs

  expect_error(
    parsnip_spls_da_prob_mp <-
      multi_predict(parsnip_spls_da,
                    as.data.frame(x_te),
                    type = "prob",
                    num_comp = 2:3),
    regexp = NA
  )
  expect_equal(nrow(parsnip_spls_da_prob_mp), nrow(x_te))
  expect_equal(nrow(parsnip_spls_da_prob_mp$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_spls_da_prob_mp$.pred[[1]]),
    c('num_comp', paste0(".pred_", levels(penguins$species)))
  )

  mo_spls_pred_9 <- predict(uni_model, x_te)$predict[9,,2:3]
  mo_spls_pred_9 <- t(apply(mo_spls_pred_9, 2, plsmod:::smax))
  mo_spls_pred_9 <- as.data.frame(mo_spls_pred_9)
  mo_spls_pred_9 <-
    tibble(num_com = 2:3,
           .pred_setosa = mo_spls_pred_9$Adelie,
           .pred_versicolor = mo_spls_pred_9$Chinstrap,
           .pred_virginica = mo_spls_pred_9$Gentoo)

  expect_equivalent(
    as.data.frame(parsnip_spls_da_prob_mp$.pred[[9]]),
    as.data.frame(mo_spls_pred_9)
  )


})
