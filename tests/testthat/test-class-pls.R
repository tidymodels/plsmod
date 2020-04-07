context("pls classification models")

load(test_path("class_examples.RData"))

# ------------------------------------------------------------------------------

pls_spec <-
  pls(num_comp = 3) %>%
  set_engine("mixOmics") %>%
  set_mode("classification")

# ------------------------------------------------------------------------------
# Interface tests



# ------------------------------------------------------------------------------

test_that('classification model fitting', {
  expect_error(
    parsnip_pls_da <-
      pls(num_comp = 3) %>%
      set_engine("mixOmics") %>%
      set_mode("classification") %>%
      fit_xy(x = old_x, y = old_y),
    regexp = NA
  )

  expect_equal(parsnip_pls_da$fit$loadings, mo_pls_da$loadings)

  expect_error(
    parsnip_pls_da_class <- predict(parsnip_pls_da, as.data.frame(new_x)),
    regexp = NA
  )

  mo_pls_da_pred <- predict(mo_pls_da, new_x)$class$mahalanobis.dist[,3]
  mo_pls_da_pred <- unname(mo_pls_da_pred)
  mo_pls_da_pred <- factor(mo_pls_da_pred, levels = levels(iris$Species))

  expect_equal(names(parsnip_pls_da_class), ".pred_class")
  expect_equal(parsnip_pls_da_class[[1]], mo_pls_da_pred)

  expect_error(
    parsnip_pls_da_prob <- predict(parsnip_pls_da, as.data.frame(new_x), type = "prob"),
    regexp = NA
  )

  mo_pls_da_pred <- predict(mo_pls_da, new_x)$predict[,,3]
  mo_pls_da_pred <- tibble::as_tibble(mo_pls_da_pred)
  names(mo_pls_da_pred) <- paste0(".pred_", names(mo_pls_da_pred))

  expect_equivalent(names(parsnip_pls_da_class), ".pred_class")
})
