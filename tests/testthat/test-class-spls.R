context("spls classification models")

load(test_path("class_examples.RData"))

library(tibble)

# ------------------------------------------------------------------------------

pls_spec <-
  pls(num_comp = 3, num_terms = 2) %>%
  set_engine("mixOmics") %>%
  set_mode("classification")

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


  # ----------------------------------------------------------------------------
  # multi-predict classes

  expect_error(
    parsnip_spls_da_class_mp <- multi_predict(parsnip_spls_da, as.data.frame(new_x), num_comp = 2:3),
    regexp = NA
  )
  expect_equal(nrow(parsnip_spls_da_class_mp), nrow(new_x))
  expect_equal(nrow(parsnip_spls_da_class_mp$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_spls_da_class_mp$.pred[[1]]),
    c('num_comp', '.pred_class')
  )

  mo_spls_pred_9 <- predict(mo_spls_da, new_x)$class$mahalanobis.dist[9,2:3]
  mo_spls_pred_9 <-
    tibble(num_com = 2:3,
           .pred_class = factor(mo_spls_pred_9, levels = levels(iris$Species)))

  expect_equivalent(
    as.data.frame(parsnip_spls_da_class_mp$.pred[[9]]),
    as.data.frame(mo_spls_pred_9)
  )

  # ----------------------------------------------------------------------------
  # multi-predict probs

  expect_error(
    parsnip_spls_da_prob_mp <-
      multi_predict(parsnip_spls_da,
                    as.data.frame(new_x),
                    type = "prob",
                    num_comp = 2:3),
    regexp = NA
  )
  expect_equal(nrow(parsnip_spls_da_prob_mp), nrow(new_x))
  expect_equal(nrow(parsnip_spls_da_prob_mp$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_spls_da_prob_mp$.pred[[1]]),
    c('num_comp', paste0(".pred_", levels(iris$Species)))
  )

  mo_spls_pred_9 <- predict(mo_spls_da, new_x)$predict[9,,2:3]
  mo_spls_pred_9 <- t(apply(mo_spls_pred_9, 2, projections:::smax))
  mo_spls_pred_9 <- as.data.frame(mo_spls_pred_9)
  mo_spls_pred_9 <-
    tibble(num_com = 2:3,
           .pred_setosa = mo_spls_pred_9$setosa,
           .pred_versicolor = mo_spls_pred_9$versicolor,
           .pred_virginica = mo_spls_pred_9$virginica)

  expect_equivalent(
    as.data.frame(parsnip_spls_da_prob_mp$.pred[[9]]),
    as.data.frame(mo_spls_pred_9)
  )


})
