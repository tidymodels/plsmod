context("mixOmics call interfaces")

# ------------------------------------------------------------------------------

tune <- function() rlang::call2("tune")

new_empty_quosure <- function(expr) {
  rlang::new_quosure(expr, env = rlang::empty_env())
}

# ------------------------------------------------------------------------------

test_that('interface with no tuning parameters', {

  reg_no_comps <-
    pls() %>%
    set_engine("mixOmics") %>%
    set_mode("regression") %>%
    translate()

  expect_true(reg_no_comps$method$fit$func["fun"] == "pls")
  expect_equal(names(reg_no_comps$method$fit$args), c("X", "Y", "mode"))

  reg_comps <-
    pls(num_comp = 12354) %>%
    set_engine("mixOmics") %>%
    set_mode("regression") %>%
    translate()

  expect_true(reg_comps$method$fit$func["fun"] == "pls")
  expect_equal(names(reg_comps$method$fit$args), c("X", "Y", "ncomp", "mode"))
  expect_equal(reg_comps$method$fit$args$ncomp, new_empty_quosure(12354))

  reg_keepx <-
    pls(num_terms = 15) %>%
    set_engine("mixOmics") %>%
    set_mode("regression") %>%
    translate()

  expect_true(reg_keepx$method$fit$func["fun"] == "spls")
  expect_equal(names(reg_keepx$method$fit$args), c("X", "Y", "keepX", "mode"))
  expect_equal(reg_keepx$method$fit$args$keepX, quote(rep(15, 2)))

  reg_both <-
    pls(num_comp = 12354, num_terms = 15) %>%
    set_engine("mixOmics") %>%
    set_mode("regression") %>%
    translate()

  expect_true(reg_both$method$fit$func["fun"] == "spls")
  expect_equal(names(reg_both$method$fit$args), c("X", "Y", "keepX", "ncomp", "mode"))
  expect_equal(reg_both$method$fit$args$ncomp, new_empty_quosure(12354))
  expect_equal(reg_both$method$fit$args$keepX, quote(rep(15, 12354)))

})

test_that('interface with tuning parameters', {

  reg_comps <-
    pls(num_comp = tune()) %>%
    set_engine("mixOmics") %>%
    set_mode("regression") %>%
    translate()

  expect_true(reg_comps$method$fit$func["fun"] == "pls")
  expect_equal(names(reg_comps$method$fit$args), c("X", "Y", "ncomp", "mode"))
  expect_equal(reg_comps$method$fit$args$ncomp, new_empty_quosure(tune()))

  reg_keepx <-
    pls(num_terms = tune()) %>%
    set_engine("mixOmics") %>%
    set_mode("regression") %>%
    translate()

  expect_true(reg_keepx$method$fit$func["fun"] == "spls")
  expect_equal(names(reg_keepx$method$fit$args), c("X", "Y", "keepX", "mode"))
  expect_equal(reg_keepx$method$fit$args$keepX, new_empty_quosure(tune()))

  reg_both <-
    pls(num_comp = tune(), num_terms = tune()) %>%
    set_engine("mixOmics") %>%
    set_mode("regression") %>%
    translate()

  expect_true(reg_both$method$fit$func["fun"] == "spls")
  expect_equal(names(reg_both$method$fit$args), c("X", "Y", "keepX", "ncomp", "mode"))
  expect_equal(reg_keepx$method$fit$args$ncomp, new_empty_quosure(tune()))
  expect_equal(reg_keepx$method$fit$args$keepX, new_empty_quosure(tune()))

})
