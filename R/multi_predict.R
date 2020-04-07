#' Model predictions across many sub-models
#' @param num_comp An integer vector for the number of PLS terms to retain.
#' @export
multi_predict._mixo_spls <-
  function(object, new_data, type = NULL, num_comp = NULL, ...) {
    if (any(names(rlang::enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")


  }
