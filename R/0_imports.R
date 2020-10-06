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

#' @importFrom parsnip multi_predict
#' @export
parsnip::multi_predict

# ------------------------------------------------------------------------------

#' @importFrom utils globalVariables
utils::globalVariables(
  c("component", "loadings", "term", "type", "value")
)

# ------------------------------------------------------------------------------


# nocov start
s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  caller <- parent.frame()

  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    } else {
      caller
    }
  }
  get_method <- function(method, env) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    } else {
      method
    }
  }

  method_fn <- get_method(method)
  stopifnot(is.function(method_fn))

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(package, "onLoad"),
    function(...) {
      ns <- asNamespace(package)

      # Refresh the method, it might have been updated by [devtools::load_all()]
      method_fn <- get_method(method)

      registerS3method(generic, class, method_fn, envir = ns)
    }
  )

  # Avoid registration failures during loading (pkgload or regular)
  if (!isNamespaceLoaded(package)) {
    return(invisible())
  }

  envir <- asNamespace(package)

  # Only register if generic can be accessed
  if (exists(generic, envir)) {
    registerS3method(generic, class, method_fn, envir = envir)
  }

  invisible()
}

# nocov end

## -----------------------------------------------------------------------------

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  # This defines pls in the model database
  make_pls_mixOmics()
  # lazily register multipredict methods
  s3_register("parsnip::multi_predict", "_mixo_pls")
  s3_register("parsnip::multi_predict", "_mixo_plsda")
  s3_register("parsnip::multi_predict", "_mixo_spls")
  s3_register("parsnip::multi_predict", "_mixo_splsda")
}
