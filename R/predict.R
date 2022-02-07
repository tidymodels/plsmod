# Post-processor for when only the largest value of ncomp is being used. This
# works for regression predictions (uni- or multivariate)
single_numeric_preds <- function(results, object) {
  tmp_pred <- results$predict
  n <- dim(tmp_pred)[1]
  p <- dim(tmp_pred)[2]
  ncomp <- dim(tmp_pred)[3]
  tmp_pred <- tmp_pred[,,ncomp]
  if (p == 1) {
    res <- tibble::tibble(.pred = unname(tmp_pred))
  } else {
    res <- tibble::as_tibble(tmp_pred)
    names(res) <- paste0(".pred_", names(res))
  }
  res
}

# ------------------------------------------------------------------------------

single_class_preds <- function(results, object) {
  tmp_pred <- results$class$mahalanobis.dist
  n <- dim(tmp_pred)[1]
  ncomp <- dim(tmp_pred)[2]
  tmp_pred <- tmp_pred[,ncomp]
  res <- tibble::tibble(.pred_class = unname(tmp_pred))
  res$.pred_class <- factor(res$.pred_class, levels = object$lvl)
  res
}

smax <- function(x) exp(x)/sum(exp(x))

single_prob_preds <- function(results, object) {
  tmp_pred <- results$predict
  n <- dim(tmp_pred)[1]
  p <- dim(tmp_pred)[2]
  ncomp <- dim(tmp_pred)[3]
  tmp_pred <- tmp_pred[,,ncomp]
  tmp_pred <- apply(tmp_pred, 1, smax)
  tmp_pred <- tibble::as_tibble(t(tmp_pred))
  names(tmp_pred) <- paste0(".pred_", names(tmp_pred))
  tmp_pred
}

# ------------------------------------------------------------------------------

multi_numeric_preds <- function(object, new_data, comps = NULL) {
  tmp_pred <- predict(object$fit, new_data, dist = "mahalanobis.dist")
  tmp_pred <- tmp_pred$predict

  n <- dim(tmp_pred)[1]
  p <- dim(tmp_pred)[2]
  q <- dim(tmp_pred)[3]
  if (is.null(comps)) {
    comps <- q
  }
  comps <- comps[comps <= q]
  tmp_grid <- tibble::tibble(num_comp = comps)

  tmp_pred <- tmp_pred[, , comps, drop = FALSE]

  if (p > 1) {
    nms <- dimnames(tmp_pred)[[2]]
    new_nms <- paste0(".pred_", nms)
    tmp_pred <- purrr::map(1:n, ~ t(as.matrix(tmp_pred[.x,,])))
  } else {
    new_nms <- ".pred"
    tmp_pred <- tmp_pred[,1,]
    tmp_pred <- purrr::map(1:n, ~ data.frame(.pred = tmp_pred[.x,]))
  }

  # Make into list for each sample

  tmp_pred <-
    purrr::map(
      tmp_pred,
      ~  dplyr::bind_cols(tmp_grid, tibble::as_tibble(.x) %>% setNames(new_nms))
    )
  tibble::tibble(.pred = tmp_pred)
}

multi_class_preds <- function(object, new_data, comps = NULL) {
  tmp_pred <- predict(object$fit, new_data, dist = "mahalanobis.dist")
  tmp_pred <- tmp_pred$class$mahalanobis.dist

  n <- dim(tmp_pred)[1]
  q <- dim(tmp_pred)[2]
  if (is.null(comps)) {
    comps <- q
  }
  comps <- comps[comps <= q]
  tmp_pred <- tmp_pred[, comps, drop = FALSE]
  tmp_pred <- t(tmp_pred)
  tmp_pred <- tibble::as_tibble(tmp_pred)

   lvl <- object$lvl

  tmp_grid <- tibble::tibble(num_comp = comps)
  # Make into list for each sample
  tmp_pred <-
    purrr::map(tmp_pred,
               ~ tibble::tibble(num_comp = comps, .pred_class = factor(.x, levels = lvl)))

  tibble::tibble(.pred = tmp_pred)
}

multi_class_probs <- function(object, new_data, comps = NULL) {
  tmp_pred <- predict(object$fit, new_data, dist = "mahalanobis.dist")
  tmp_pred <- tmp_pred$predict

  n <- dim(tmp_pred)[1]
  p <- dim(tmp_pred)[2]
  q <- dim(tmp_pred)[3]
  if (is.null(comps)) {
    comps <- q
  }
  comps <- comps[comps <= q]
  tmp_pred <- tmp_pred[, , comps, drop = FALSE]

  lvl <- object$lvl
  new_nms <- paste0(".pred_", lvl)
  tmp_grid <- tibble::tibble(num_comp = comps)
  # Make into list for each sample
  tmp_pred <- purrr::map(1:n, ~ t(as.matrix(tmp_pred[.x,,])))
  # Normalize to on probability scale
  tmp_pred <- purrr::map(tmp_pred, ~ t(apply(.x, 1, smax)))

  tmp_pred <-
    purrr::map(
      tmp_pred,
      ~ dplyr::bind_cols(tmp_grid, tibble::as_tibble(.x) %>% setNames(new_nms)
        )
    )
  tibble::tibble(.pred = tmp_pred)
}


#' Model predictions across many sub-models
#' @param object An object of class `model_fit`
#' @param new_data A rectangular data object, such as a data frame.
#' @param type A single character value or `NULL`. Possible values
#'  are "numeric", "class", or "prob". When `NULL`, `predict()` will choose an
#'  appropriate value based on the model's mode.
#' @param num_comp An integer vector for the number of PLS terms to retain.
#' @param ... Not currently used.
#' @export
#' @rdname multi_predict
#' @examples
#' data(meats, package = "modeldata")
#'
#' mv_meats <-
#'   pls(num_comp = 20) %>%
#'   set_engine("mixOmics") %>%
#'   set_mode("regression") %>%
#'   fit_xy(x = meats[-(1:5), 1:100], y = meats[-(1:5), 101:103])
#'
multi_predict._mixo_pls <-
  function(object, new_data, num_comp = NULL, type = NULL, ...) {
    if (any(names(rlang::enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    ## -------------------------------------------------------------------------
    ## prepare data

    new_data <- parsnip::prepare_data(object, new_data)

    # preprocess data
    if (!is.null(object$spec$method$pred$prob$pre)) {
      new_data <- object$spec$method$pred$prob$pre(new_data, object)
    }

    ## -------------------------------------------------------------------------


    if (is.null(num_comp)) {
      num_comp <- object$fit$sncomp
    }
    num_comp <- sort(unique(num_comp))

    if (is.null(type)) {
      if (object$spec$mode == "regression") {
        type <- "numeric"
      } else {
        type <- "class"
      }
    }

    if (type == "numeric") {
      if (object$spec$mode != "regression") {
        rlang::abort("`type = 'numeric'` is for regression models.")
      }
      res <- multi_numeric_preds(object, new_data, num_comp)
    } else if (type == "class") {
      if (object$spec$mode != "classification") {
        rlang::abort("`type = 'class'` is for classification models.")
      }
      res <- multi_class_preds(object, new_data, num_comp)
    } else if (type == "prob") {
      if (object$spec$mode != "classification") {
        rlang::abort("`type = 'prob'` is for classification models.")
      }
      res <- multi_class_probs(object, new_data, num_comp)
    }
    res
  }

#' @export
#' @rdname multi_predict
multi_predict._mixo_spls <- multi_predict._mixo_pls

#' @export
#' @rdname multi_predict
multi_predict._mixo_plsda <- multi_predict._mixo_pls

#' @export
#' @rdname multi_predict
multi_predict._mixo_splsda <- multi_predict._mixo_pls

