#' General Interface for Partial Least Squares (PLS)
#'
#' `pls()` is a way to generate a _specification_ of a model before
#'  fitting and allows the model to be created using R. The main
#'  arguments for the
#'  model are:
#' \itemize{
#'   \item \code{predictor_prop}: The proportion of predictors that are allowed
#'   to affect each PLS loading.
#'   \item \code{num_comp}: The number of PLS components to retain.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using `set_engine()`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param predictor_prop The maximum proportion of original predictors that can
#'  have _non-zero_ coefficients for each PLS component (via regularization).
#'  This value is used for all PLS components for X.
#' @param num_comp The number of PLS components to retain.
#' @details The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"mixOmics"`  (the default)
#' }
#'
#' @section Engine Details:
#'
#' Engines may have pre-set default arguments when executing the
#'  model fit call. The possible model calls are shown in the Examples section
#'  below.
#'
#' @examples
#' pls(num_comp = 2, predictor_prop = 0.2) %>%
#'   set_engine("mixOmics") %>%
#'   set_mode("regression") %>%
#'   translate()
#'
#' pls(num_comp = 2, predictor_prop = 1) %>%
#'   set_engine("mixOmics") %>%
#'   set_mode("classification") %>%
#'   translate()
#'
#' pls(num_comp = 6) %>%
#'   set_engine("mixOmics") %>%
#'   set_mode("regression") %>%
#'   translate()
#'
#' pls() %>%
#'   set_engine("mixOmics") %>%
#'   set_mode("classification") %>%
#'   translate()
#'
#'
#' @export
pls <-
  function(mode = "unknown", predictor_prop = NULL, num_comp = NULL) {

    args <- list(
      predictor_prop = enquo(predictor_prop),
      num_comp       = enquo(num_comp)
    )

    parsnip::new_model_spec(
      "pls",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.pls <- function(x, ...) {
  cat("PLS Model Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' @export
#' @param object A PLS model specification.
#' @param parameters A 1-row tibble or named list with _main_
#'  parameters to update. If the individual arguments are used,
#'  these will supersede the values in `parameters`. Also, using
#'  engine arguments in this object will result in an error.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @examples
#' model <- pls(predictor_prop =  0.1)
#' model
#' update(model, predictor_prop = 1)
#' update(model, predictor_prop = 1, fresh = TRUE)
#' @method update pls
#' @rdname pls
#' @export
update.pls <-
  function(object,
           parameters = NULL,
           predictor_prop = NULL, num_comp = NULL,
           fresh = FALSE, ...) {
    parsnip::update_dot_check(...)

    if (!is.null(parameters)) {
      parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(
      predictor_prop    = enquo(predictor_prop),
      num_comp  = enquo(num_comp)
    )

    args <- parsnip::update_main_parameters(args, parameters)

    if (fresh) {
      object$args <- args
    } else {
      null_args <- purrr::map_lgl(args, parsnip::null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }

    parsnip::new_model_spec(
      "pls",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

check_args.pls <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$num_comp) && args$num_comp < 0)
    rlang::abort("`num_comp` should be >= 1.")

  invisible(object)
}

