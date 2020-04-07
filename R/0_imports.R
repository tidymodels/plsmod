#' @importFrom rlang enquo
#' @importFrom purrr map_lgl
#' @importFrom tibble is_tibble as_tibble
#' @importFrom parsnip set_new_model
#' @importFrom stats predict loadings
#' @importFrom mixOmics spls

# ------------------------------------------------------------------------------

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# ------------------------------------------------------------------------------

#' @importFrom utils globalVariables
utils::globalVariables(
  c("component", "loadings", "term", "type", "value")
)

# ------------------------------------------------------------------------------

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  # This defines pls in the model database
  make_pls_mixOmics()
}
