maybe_pls_multivariate <- function(results, object) {

  if (isTRUE(ncol(results) > 1)) {
    nms <- colnames(results)
    results <- tibble::as_tibble(results, .name_repair = "minimal")
    if (length(nms) == 0 && length(object$preproc$y_var) == ncol(results)) {
      names(results) <- object$preproc$y_var
    }
  }  else {
    results <- unname(results[, 1])
  }
  results
}
