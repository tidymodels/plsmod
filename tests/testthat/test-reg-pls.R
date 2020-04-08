context("pls regression models")

load(test_path("reg_examples.RData"))

# ------------------------------------------------------------------------------

pls_spec <-
  pls(num_comp = 3) %>%
  set_engine("mixOmics") %>%
  set_mode("regression")

# ------------------------------------------------------------------------------
# Interface tests



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

  mo_pls_da_pred <- predict(mo_pls_multi, new_x)$predict[,,3]
  mo_pls_da_pred <- as_tibble(mo_pls_da_pred)
  names(mo_pls_da_pred) <- paste0(".pred_", names(mo_pls_da_pred))

  expect_equivalent(as.data.frame(parsnip_pls_multi_num), as.data.frame(mo_pls_da_pred))
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

  mo_pls_da_pred <- predict(mo_pls_uni, new_x)$predict[,,3]
  mo_pls_da_pred <- as_tibble(mo_pls_da_pred)
  names(mo_pls_da_pred) <- ".pred"

  expect_equivalent(as.data.frame(parsnip_pls_uni_num), as.data.frame(mo_pls_da_pred))
})

