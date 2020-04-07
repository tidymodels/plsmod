
#' @export
translate.pls <- function(x, engine = x$engine, ...) {
  x <- parsnip::translate.default(x, engine, ...)
  x <- is_da(x)
  x <- is_sparse(x)

  x
}


is_da <- function(x) {
  if (x$mode == "classification") {
    x$method$fit$func["fun"] <- paste0(x$method$fit$func["fun"], "da")
  }
  x
}

is_sparse <- function(x) {
  if (is.null(x$args$num_terms) ||
      all(rlang::expr_text(x$args$num_terms) == "~NULL")) {
    x$method$fit$func["fun"] <- gsub("^s", "", x$method$fit$func["fun"])
  }
  x
}

expand_keepx <- function(x) {
  x
}
