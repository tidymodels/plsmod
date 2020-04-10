context("pls regression models")

load(test_path("reg_examples.RData"))

library(tibble)

# ------------------------------------------------------------------------------

pls_spec <-
  pls(num_comp = 3) %>%
  set_engine("mixOmics") %>%
  set_mode("regression")

# ------------------------------------------------------------------------------

test_that('Multivariate model fitting', {
  expect_error(
    parsnip_pls_multi <-
      pls(num_comp = 3) %>%
      set_engine("mixOmics") %>%
      set_mode("regression") %>%
      fit_xy(x = old_x, y = old_y),
    regexp = NA
  )

  expect_equal(parsnip_pls_multi$fit$loadings, mo_pls_multi$loadings)

  expect_error(
    parsnip_pls_multi_num <- predict(parsnip_pls_multi, as.data.frame(new_x)),
    regexp = NA
  )

  mo_pls_pred <- predict(mo_pls_multi, new_x)$predict[,,3]
  mo_pls_pred <- as_tibble(mo_pls_pred)
  names(mo_pls_pred) <- paste0(".pred_", names(mo_pls_pred))

  expect_equivalent(as.data.frame(parsnip_pls_multi_num), as.data.frame(mo_pls_pred))

  # ----------------------------------------------------------------------------
  # multi-predict

  expect_error(
    parsnip_pls_multi_pred_num <-
      multi_predict(parsnip_pls_multi, as.data.frame(new_x), num_comp = 1:2),
    regexp = NA
  )
  expect_equal(nrow(parsnip_pls_multi_pred_num), nrow(new_x))
  expect_equal(nrow(parsnip_pls_multi_pred_num$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_pls_multi_pred_num$.pred[[1]]),
    c('num_comp', '.pred_water', '.pred_fat', '.pred_protein')
  )

  mo_pls_pred_9 <- t(predict(mo_pls_multi, new_x)$predict[9,,1:2])
  mo_pls_pred_9 <- as_tibble(mo_pls_pred_9)
  names(mo_pls_pred_9) <- paste0(".pred_", names(mo_pls_pred_9))

  expect_equivalent(
    as.data.frame(parsnip_pls_multi_pred_num$.pred[[9]][,-1]),
    as.data.frame(mo_pls_pred_9)
  )
})

# ------------------------------------------------------------------------------

test_that('Univariate model fitting', {
  expect_error(
    parsnip_pls_uni <-
      pls(num_comp = 3) %>%
      set_engine("mixOmics") %>%
      set_mode("regression") %>%
      fit_xy(x = old_x, y = old_y[[1]]),
    regexp = NA
  )

  expect_equal(parsnip_pls_uni$fit$loadings, mo_pls_uni$loadings)

  expect_error(
    parsnip_pls_uni_num <- predict(parsnip_pls_uni, as.data.frame(new_x)),
    regexp = NA
  )

  mo_pls_pred <- predict(mo_pls_uni, new_x)$predict[,,3]
  mo_pls_pred <- as_tibble(mo_pls_pred)
  names(mo_pls_pred) <- ".pred"

  expect_equivalent(as.data.frame(parsnip_pls_uni_num), as.data.frame(mo_pls_pred))

  # ----------------------------------------------------------------------------
  # multi-predict

  expect_error(
    parsnip_pls_multi_pred_num <-
      multi_predict(parsnip_pls_uni, as.data.frame(new_x), num_comp = 1:2),
    regexp = NA
  )
  expect_equal(nrow(parsnip_pls_multi_pred_num), nrow(new_x))
  expect_equal(nrow(parsnip_pls_multi_pred_num$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_pls_multi_pred_num$.pred[[1]]),
    c('num_comp', '.pred')
  )

  mo_pls_pred_9 <- predict(mo_pls_uni, new_x)$predict[9,,1:2]
  mo_pls_pred_9 <- tibble(.pred = mo_pls_pred_9)

  expect_equivalent(
    as.data.frame(parsnip_pls_multi_pred_num$.pred[[9]][,-1]),
    as.data.frame(mo_pls_pred_9)
  )
})

