context("pls regression models")

library(tibble)
library(modeldata)
library(mixOmics)
data(meats)

## -----------------------------------------------------------------------------

for_test <- 1:10
y_tr <- meats[-for_test, c("water", "fat", "protein")]
y_te <- meats[ for_test, c("water", "fat", "protein")]

x_tr <- meats[-for_test, 1:100]
x_te <- meats[ for_test, 1:100]

multi_model  <- mixOmics::pls(x_tr, y_tr, ncomp = 3)
uni_model  <- mixOmics::pls(x_tr, y_tr[[1]], ncomp = 3)

# ------------------------------------------------------------------------------

pls_spec <-
  parsnip::pls(num_comp = 3) %>%
  set_engine("mixOmics") %>%
  set_mode("regression")

# ------------------------------------------------------------------------------

test_that('Multivariate model fitting', {
  # Failures on r-devel are upstream in mixOmics. See
  # https://github.com/mixOmicsTeam/mixOmics/issues/162
  skip_on_cran()
  skip_if(grepl("development", R.version$status))

  expect_error(
    parsnip_pls_multi <-
      parsnip::pls(num_comp = 3) %>%
      set_engine("mixOmics") %>%
      set_mode("regression") %>%
      fit_xy(x = x_tr, y = y_tr),
    regexp = NA
  )

  expect_equal(parsnip_pls_multi$fit$loadings, multi_model$loadings)

  expect_error(
    parsnip_pls_multi_num <- predict(parsnip_pls_multi, as.data.frame(x_te)),
    regexp = NA
  )

  mo_pls_pred <- predict(multi_model, x_te)$predict[,,3]
  mo_pls_pred <- as_tibble(mo_pls_pred)
  names(mo_pls_pred) <- paste0(".pred_", names(mo_pls_pred))

  expect_equivalent(as.data.frame(parsnip_pls_multi_num), as.data.frame(mo_pls_pred))

  # ----------------------------------------------------------------------------
  # multi-predict

  expect_error(
    parsnip_pls_multi_pred_num <-
      multi_predict(parsnip_pls_multi, as.data.frame(x_te), num_comp = 1:2),
    regexp = NA
  )
  expect_equal(nrow(parsnip_pls_multi_pred_num), nrow(x_te))
  expect_equal(nrow(parsnip_pls_multi_pred_num$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_pls_multi_pred_num$.pred[[1]]),
    c('num_comp', '.pred_water', '.pred_fat', '.pred_protein')
  )

  mo_pls_pred_9 <- t(predict(multi_model, x_te)$predict[9,,1:2])
  mo_pls_pred_9 <- as_tibble(mo_pls_pred_9)
  names(mo_pls_pred_9) <- paste0(".pred_", names(mo_pls_pred_9))

  expect_equivalent(
    as.data.frame(parsnip_pls_multi_pred_num$.pred[[9]][,-1]),
    as.data.frame(mo_pls_pred_9)
  )
})

# ------------------------------------------------------------------------------

test_that('Univariate model fitting', {
  skip_on_cran()
  skip_if(grepl("development", R.version$status))

  expect_error(
    parsnip_pls_uni <-
      parsnip::pls(num_comp = 3) %>%
      set_engine("mixOmics") %>%
      set_mode("regression") %>%
      fit_xy(x = x_tr, y = y_tr[[1]]),
    regexp = NA
  )

  expect_equal(parsnip_pls_uni$fit$loadings, uni_model$loadings)

  expect_error(
    parsnip_pls_uni_num <- predict(parsnip_pls_uni, as.data.frame(x_te)),
    regexp = NA
  )

  mo_pls_pred <- predict(uni_model, x_te)$predict[,,3]
  mo_pls_pred <- as_tibble(mo_pls_pred)
  names(mo_pls_pred) <- ".pred"

  expect_equivalent(as.data.frame(parsnip_pls_uni_num), as.data.frame(mo_pls_pred))

  # ----------------------------------------------------------------------------
  # multi-predict

  expect_error(
    parsnip_pls_multi_pred_num <-
      multi_predict(parsnip_pls_uni, as.data.frame(x_te), num_comp = 1:2),
    regexp = NA
  )
  expect_equal(nrow(parsnip_pls_multi_pred_num), nrow(x_te))
  expect_equal(nrow(parsnip_pls_multi_pred_num$.pred[[1]]), 2)
  expect_equal(
    names(parsnip_pls_multi_pred_num$.pred[[1]]),
    c('num_comp', '.pred')
  )

  mo_pls_pred_9 <- predict(uni_model, x_te)$predict[9,,1:2]
  mo_pls_pred_9 <- tibble(.pred = mo_pls_pred_9)

  expect_equivalent(
    as.data.frame(parsnip_pls_multi_pred_num$.pred[[9]][,-1]),
    as.data.frame(mo_pls_pred_9)
  )
})

# ------------------------------------------------------------------------------

test_that('dummy variable encodings', {
  skip_on_cran()
  skip_if(grepl("development", R.version$status))

  data(penguins, package = "modeldata")
  penguins <- na.omit(penguins)
  expect_error(
    parsnip_pls_multi <-
      parsnip::pls(num_comp = 3) %>%
      set_engine("mixOmics") %>%
      set_mode("regression") %>%
      fit(body_mass_g ~ ., data = penguins),
    regexp = NA
  )

  expect_true(!any(names(parsnip_pls_multi$fit$X) == "(Intercept)"))

  expect_error(
    predict(parsnip_pls_multi, penguins),
    regexp = NA
  )
  expect_error(
    multi_predict(parsnip_pls_multi, penguins, num_comp = 1:3),
    regexp = NA
  )
})
