context("spls classification models")

load(test_path("class_examples.RData"))

# ------------------------------------------------------------------------------

pls_spec <-
  pls(num_comp = 3, num_terms = 2) %>%
  set_engine("mixOmics") %>%
  set_mode("classification")

# ------------------------------------------------------------------------------
# Interface tests



# ------------------------------------------------------------------------------
# Model fitting tests

test_that('classification model fitting', {
  expect_error(
    parsnip_spls_da <-
      pls(num_comp = 3, num_terms = 2) %>%
      set_engine("mixOmics") %>%
      set_mode("classification") %>%
      fit_xy(x = old_x, y = old_y),
    regexp = NA
  )

  expect_equal(parsnip_spls_da$fit$loadings, mo_spls_da$loadings)

  expect_error(
    parsnip_spls_da_class <- predict(parsnip_spls_da, as.data.frame(new_x)),
    regexp = NA
  )

  mo_spls_da_pred <- predict(mo_spls_da, new_x)$class$mahalanobis.dist[,3]
  mo_spls_da_pred <- unname(mo_spls_da_pred)
  mo_spls_da_pred <- factor(mo_spls_da_pred, levels = levels(iris$Species))

  expect_equal(names(parsnip_spls_da_class), ".pred_class")
  expect_equal(parsnip_spls_da_class[[1]], mo_spls_da_pred)

  expect_error(
    parsnip_spls_da_prob <- predict(parsnip_spls_da, as.data.frame(new_x), type = "prob"),
    regexp = NA
  )

  mo_spls_da_pred <- predict(mo_spls_da, new_x)$predict[,,3]
  mo_spls_da_pred <- tibble::as_tibble(mo_spls_da_pred)
  names(mo_spls_da_pred) <- paste0(".pred_", names(mo_spls_da_pred))

  expect_equivalent(names(parsnip_spls_da_class), ".pred_class")

})
