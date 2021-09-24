context("spls regression models")

library(mixOmics)
library(plsmod)
library(tibble)
library(modeldata)

data(meats)

## -----------------------------------------------------------------------------

for_test <- 1:10
y_tr <- meats[-for_test, c("water", "fat", "protein")]
y_te <- meats[ for_test, c("water", "fat", "protein")]

x_tr <- meats[-for_test, 1:100]
x_te <- meats[ for_test, 1:100]

multi_model  <- mixOmics::spls(x_tr, y_tr, ncomp = 3, keepX = rep(100, 100))
uni_model  <- mixOmics::spls(x_tr, y_tr[[1]], ncomp = 3, keepX = rep(100, 100))

# ------------------------------------------------------------------------------

test_that('Multivariate model fitting', {
  expect_error(
    parsnip_spls_multi <-
      parsnip::pls(num_comp = 3, predictor_prop = 1) %>%
      set_engine("mixOmics") %>%
      set_mode("regression") %>%
      fit_xy(x = x_tr, y = y_tr),
    regexp = NA
  )

  expect_equal(parsnip_spls_multi$fit$loadings, multi_model$loadings)

  expect_error(
    parsnip_spls_multi_num <- predict(parsnip_spls_multi, as.data.frame(x_te)),
    regexp = NA
  )

  mo_spls_pred <- predict(multi_model, x_te)$predict[,,3]
  mo_spls_pred <- as_tibble(mo_spls_pred)
  names(mo_spls_pred) <- paste0(".pred_", names(mo_spls_pred))

  expect_equivalent(as.data.frame(parsnip_spls_multi_num), as.data.frame(mo_spls_pred))

  # ----------------------------------------------------------------------------
  # multi-predict

  expect_error(
    parsnip_spls_multi_pred_num <-
      multi_predict(parsnip_spls_multi, as.data.frame(x_te), num_comp = 1:2),
    regexp = NA
  )
  expect_equal(nrow(parsnip_spls_multi_pred_num), nrow(x_te))
  expect_equal(nrow(parsnip_spls_multi_pred_num$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_spls_multi_pred_num$.pred[[1]]),
    c('num_comp', '.pred_water', '.pred_fat', '.pred_protein')
  )

  mo_spls_pred_9 <- t(predict(multi_model, x_te)$predict[9,,1:2])
  mo_spls_pred_9 <- as_tibble(mo_spls_pred_9)
  names(mo_spls_pred_9) <- paste0(".pred_", names(mo_spls_pred_9))

  expect_equivalent(
    as.data.frame(parsnip_spls_multi_pred_num$.pred[[9]][,-1]),
    as.data.frame(mo_spls_pred_9)
  )
})

# ------------------------------------------------------------------------------

test_that('Univariate model fitting', {
  expect_error(
    parsnip_spls_uni <-
      parsnip::pls(num_comp = 3, predictor_prop = 1) %>%
      set_engine("mixOmics") %>%
      set_mode("regression") %>%
      fit_xy(x = x_tr, y = y_tr[[1]]),
    regexp = NA
  )

  expect_equal(parsnip_spls_uni$fit$loadings, uni_model$loadings)

  expect_error(
    parsnip_spls_uni_num <- predict(parsnip_spls_uni, as.data.frame(x_te)),
    regexp = NA
  )

  mo_spls_pred <- predict(uni_model, x_te)$predict[,,3]
  mo_spls_pred <- as_tibble(mo_spls_pred)
  names(mo_spls_pred) <- ".pred"

  expect_equivalent(as.data.frame(parsnip_spls_uni_num), as.data.frame(mo_spls_pred))

  # ----------------------------------------------------------------------------
  # multi-predict

  expect_error(
    parsnip_spls_multi_pred_num <-
      multi_predict(parsnip_spls_uni, as.data.frame(x_te), num_comp = 1:2),
    regexp = NA
  )
  expect_equal(nrow(parsnip_spls_multi_pred_num), nrow(x_te))
  expect_equal(nrow(parsnip_spls_multi_pred_num$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_spls_multi_pred_num$.pred[[1]]),
    c('num_comp', '.pred')
  )

  mo_spls_pred_9 <- predict(uni_model, x_te)$predict[9,,1:2]
  mo_spls_pred_9 <- tibble(.pred = mo_spls_pred_9)

  expect_equivalent(
    as.data.frame(parsnip_spls_multi_pred_num$.pred[[9]][,-1]),
    as.data.frame(mo_spls_pred_9)
  )
})

