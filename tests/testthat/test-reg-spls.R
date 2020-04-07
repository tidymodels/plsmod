context("spls regression models")

load(test_path("reg_examples.RData"))

# ------------------------------------------------------------------------------

pls_spec <-
  pls(num_comp = 3) %>%
  set_engine("mixOmics") %>%
  set_mode("regression")

# ------------------------------------------------------------------------------
# Interface tests



# ------------------------------------------------------------------------------
# Multivariate model fitting tests

expect_error(
  parsnip_spls_multi <-
    pls(num_comp = 3, num_terms = 2) %>%
    set_engine("mixOmics") %>%
    set_mode("regression") %>%
    fit_xy(x = old_x, y = old_y),
  regexp = NA
)

expect_equal(parsnip_spls_multi$fit$loadings, mo_spls_multi$loadings)

# ------------------------------------------------------------------------------
# Multivariate numeric prediction tests

expect_error(
  parsnip_spls_multi_num <- predict(parsnip_spls_multi, as.data.frame(new_x)),
  regexp = NA
)

mo_spls_da_pred <- predict(mo_spls_multi, new_x)$predict[,,3]
mo_spls_da_pred <- as_tibble(mo_spls_da_pred)
names(mo_spls_da_pred) <- paste0(".pred_", names(mo_spls_da_pred))

expect_equal(parsnip_spls_multi_num, mo_spls_da_pred)

# ------------------------------------------------------------------------------
# Univariate model fitting tests

expect_error(
  parsnip_spls_uni <-
    pls(num_comp = 3, num_terms = 2) %>%
    set_engine("mixOmics") %>%
    set_mode("regression") %>%
    fit_xy(x = old_x, y = old_y[[1]]),
  regexp = NA
)

expect_equal(parsnip_spls_uni$fit$loadings, mo_spls_uni$loadings)

# ------------------------------------------------------------------------------
# Univariate numeric prediction tests

expect_error(
  parsnip_spls_uni_num <- predict(parsnip_spls_uni, as.data.frame(new_x)),
  regexp = NA
)

mo_spls_da_pred <- predict(mo_spls_uni, new_x)$predict[,,3]
mo_spls_da_pred <- as_tibble(mo_spls_da_pred)
names(mo_spls_da_pred) <- ".pred"

expect_equal(parsnip_spls_uni_num, mo_spls_da_pred)

