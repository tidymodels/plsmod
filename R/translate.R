
#' @export
translate.pls <- function(x, engine = x$engine, ...) {
  x <- parsnip::translate.default(x, engine, ...)
  # Now we are using the `fit` part of `method` that uses the ordginal
  # argument names

  x <- is_da(x)

  # Evaluate the arguments in order to get the function assignment
  # and keepX arguments right.
  values <- purrr::map(x$method$fit$args, rlang::eval_tidy)

  x <- is_sparse(x, values)

  x$method$fit$args$keepX <- expand_keepx(values)

  x
}


is_da <- function(x) {
  if (x$mode == "classification") {
    x$method$fit$func["fun"] <- paste0(x$method$fit$func["fun"], "da")
  }
  x
}

is_sparse <- function(x, args) {
  if (!has_value(args$keepX)) {
    x$method$fit$func["fun"] <- gsub("^s", "", x$method$fit$func["fun"])
  }
  x
}

has_value <- function(x) {
  !is.null(x) && is.numeric(x)
}

expand_keepx <- function(x) {
  if (has_value(x$keepX)) {
    if (!has_value(x$ncomp)) {
      x$ncomp <- 2 # <- the default in the mixOmics package
    }
    res <- rlang::call2("rep", x$keepX, x$ncomp)
  } else {
    res <- x$keepX
  }
  res
}


