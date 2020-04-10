context("spls regression models")

load(test_path("reg_examples.RData"))

library(tibble)

# ------------------------------------------------------------------------------

test_that('Multivariate model fitting', {
  expect_error(
    parsnip_spls_multi <-
      pls(num_comp = 3, num_terms = 2) %>%
      set_engine("mixOmics") %>%
      set_mode("regression") %>%
      fit_xy(x = old_x, y = old_y),
    regexp = NA
  )

  expect_equal(parsnip_spls_multi$fit$loadings, mo_spls_multi$loadings)

  expect_error(
    parsnip_spls_multi_num <- predict(parsnip_spls_multi, as.data.frame(new_x)),
    regexp = NA
  )

  mo_spls_pred <- predict(mo_spls_multi, new_x)$predict[,,3]
  mo_spls_pred <- as_tibble(mo_spls_pred)
  names(mo_spls_pred) <- paste0(".pred_", names(mo_spls_pred))

  expect_equivalent(as.data.frame(parsnip_spls_multi_num), as.data.frame(mo_spls_pred))

  # ----------------------------------------------------------------------------
  # multi-predict

  expect_error(
    parsnip_spls_multi_pred_num <-
      multi_predict(parsnip_spls_multi, as.data.frame(new_x), num_comp = 1:2),
    regexp = NA
  )
  expect_equal(nrow(parsnip_spls_multi_pred_num), nrow(new_x))
  expect_equal(nrow(parsnip_spls_multi_pred_num$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_spls_multi_pred_num$.pred[[1]]),
    c('num_comp', '.pred_water', '.pred_fat', '.pred_protein')
  )

  mo_spls_pred_9 <- t(predict(mo_spls_multi, new_x)$predict[9,,1:2])
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
      pls(num_comp = 3, num_terms = 2) %>%
      set_engine("mixOmics") %>%
      set_mode("regression") %>%
      fit_xy(x = old_x, y = old_y[[1]]),
    regexp = NA
  )

  expect_equal(parsnip_spls_uni$fit$loadings, mo_spls_uni$loadings)

  expect_error(
    parsnip_spls_uni_num <- predict(parsnip_spls_uni, as.data.frame(new_x)),
    regexp = NA
  )

  mo_spls_pred <- predict(mo_spls_uni, new_x)$predict[,,3]
  mo_spls_pred <- as_tibble(mo_spls_pred)
  names(mo_spls_pred) <- ".pred"

  expect_equivalent(as.data.frame(parsnip_spls_uni_num), as.data.frame(mo_spls_pred))

  # ----------------------------------------------------------------------------
  # multi-predict

  expect_error(
    parsnip_spls_multi_pred_num <-
      multi_predict(parsnip_spls_uni, as.data.frame(new_x), num_comp = 1:2),
    regexp = NA
  )
  expect_equal(nrow(parsnip_spls_multi_pred_num), nrow(new_x))
  expect_equal(nrow(parsnip_spls_multi_pred_num$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_spls_multi_pred_num$.pred[[1]]),
    c('num_comp', '.pred')
  )

  mo_spls_pred_9 <- predict(mo_spls_uni, new_x)$predict[9,,1:2]
  mo_spls_pred_9 <- tibble(.pred = mo_spls_pred_9)

  expect_equivalent(
    as.data.frame(parsnip_spls_multi_pred_num$.pred[[9]][,-1]),
    as.data.frame(mo_spls_pred_9)
  )
})

