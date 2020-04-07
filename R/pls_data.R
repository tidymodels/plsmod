make_pls_mixOmics <- function() {
  parsnip::set_new_model("pls")

  parsnip::set_model_mode("pls", "classification")
  parsnip::set_model_mode("pls", "regression")

  # ------------------------------------------------------------------------------

  parsnip::set_model_engine("pls", "classification", "mixOmics")
  parsnip::set_model_engine("pls", "regression",     "mixOmics")
  parsnip::set_dependency("pls", "mixOmics",       "mixOmics")

  parsnip::set_model_arg(
    model = "pls",
    eng = "mixOmics",
    parsnip = "num_terms",
    original = "keepX",
    func = list(pkg = "dials", fun = "num_terms", range = c(2, 5)),
    has_submodel = TRUE
  )
  parsnip::set_model_arg(
    model = "pls",
    eng = "mixOmics",
    parsnip = "num_comp",
    original = "ncomp",
    func = list(pkg = "dials", fun = "num_comp"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "pls",
    eng = "mixOmics",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("X", "Y"),
      func = c(pkg = "mixOmics", fun = "spls"),
      defaults = list(mode = "regression")
    )
  )

  parsnip::set_fit(
    model = "pls",
    eng = "mixOmics",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("X", "Y"),
      func = c(pkg = "mixOmics", fun = "spls"),
      defaults = list(mode = "regression")
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
        list(object = quote(object$fit),
             newdata = quote(new_data),
             dist = "mahalanobis.dist")
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
        list(object = quote(object$fit),
             newdata = quote(new_data),
             dist = "mahalanobis.dist")
    )
  )
}
