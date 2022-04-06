# nocov
make_pls_mixOmics <- function() {
  parsnip::set_model_engine("pls", "classification", "mixOmics")
  parsnip::set_model_engine("pls", "regression", "mixOmics")
  parsnip::set_dependency("pls", "mixOmics", "mixOmics", "classification")
  parsnip::set_dependency("pls", "mixOmics", "mixOmics", "regression")
  parsnip::set_dependency("pls", "mixOmics", "plsmod", "classification")
  parsnip::set_dependency("pls", "mixOmics", "plsmod", "regression")

  parsnip::set_model_arg(
    model = "pls",
    eng = "mixOmics",
    parsnip = "predictor_prop",
    original = "predictor_prop",
    func = list(pkg = "dials", fun = "predictor_prop"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "pls",
    eng = "mixOmics",
    parsnip = "num_comp",
    original = "ncomp",
    func = list(pkg = "dials", fun = "num_comp", range = c(1, 4)),
    has_submodel = TRUE
  )

  parsnip::set_fit(
    model = "pls",
    eng = "mixOmics",
    mode = "regression",
    value = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(pkg = "plsmod", fun = "pls_fit"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "pls",
    eng = "mixOmics",
    mode = "regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "pls",
    eng = "mixOmics",
    mode = "classification",
    value = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(pkg = "plsmod", fun = "pls_fit"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "pls",
    eng = "mixOmics",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "pls",
    eng = "mixOmics",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = single_numeric_preds,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          dist = "mahalanobis.dist"
        )
    )
  )

  parsnip::set_pred(
    model = "pls",
    eng = "mixOmics",
    mode = "regression",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          dist = "mahalanobis.dist"
        )
    )
  )

  parsnip::set_pred(
    model = "pls",
    eng = "mixOmics",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = single_class_preds,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          dist = "mahalanobis.dist"
        )
    )
  )

  parsnip::set_pred(
    model = "pls",
    eng = "mixOmics",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = single_prob_preds,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          dist = "mahalanobis.dist"
        )
    )
  )

  parsnip::set_pred(
    model = "pls",
    eng = "mixOmics",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          dist = "mahalanobis.dist"
        )
    )
  )
}

# nocov end
