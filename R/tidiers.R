#' Tidy methods for pls and spls objects
#'
#' @param x An object with class `mixo_pls` or `mixo_spls`.
#' @param ... Not currently used.
#' @return A tibble with columns `terms` (the predictor names), `value` (the
#' loadings), `type` (either "predictors" or "outcomes"), and `component` (the
#' component number).
#' @export
tidy.mixo_pls <- function(x, ...) {
  loads <- loadings(x)
  nms <- purrr::map(loads, rownames)
  loads <- purrr::map(loads, tibble::as_tibble)
  loads <- purrr::map2(loads, nms, \(.x, .y) .x |> dplyr::mutate(term = .y))
  loads$X$type <- "predictors"
  loads$Y$type <- "outcomes"

  loads <- dplyr::bind_rows(loads)
  loads <-
    tidyr::pivot_longer(
      loads,
      cols = c(-term, -type),
      names_to = "component",
      values_to = "value"
    )

  loads <- dplyr::select(loads, term, value, type, component)
  loads$component <- as.numeric(gsub("^comp", "", loads$component))
  loads
}

#' @export
#' @rdname tidy.mixo_pls
tidy.mixo_spls <- function(x, ...) {
  loads <- loadings(x)
  nms <- purrr::map(loads, rownames)
  loads <- purrr::map(loads, tibble::as_tibble)
  loads <- purrr::map2(loads, nms, \(.x, .y) .x |> dplyr::mutate(term = .y))
  loads$X$type <- "predictors"
  loads$Y$type <- "outcomes"

  loads <- dplyr::bind_rows(loads)
  loads <-
    tidyr::pivot_longer(
      loads,
      cols = c(-term, -type),
      names_to = "component",
      values_to = "value"
    )

  loads <- dplyr::select(loads, term, value, type, component)
  loads$component <- as.numeric(gsub("^comp", "", loads$component))
  loads
}
