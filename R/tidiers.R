tidy.mixo_spls <- function(x, ...) {
  loads <- loadings(x)
  nms <- purrr::map(loads, rownames)
  loads <- purrr::map(loads, tibble::as_tibble)
  loads <- purrr::map2(loads, nms, ~ .x %>% mutate(term = .y))
  loads$X$type <- "predictors"
  loads$Y$type <- "outcomes"

  loads <- dplyr::bind_rows(loads)
  loads <-
    tidyr::pivot_longer(
      loads,
      cols = c(-term,-type),
      names_to = "component",
      values_to = "value"
    )

  loads <- dplyr::select(loads, term, value, type, component)
  loads$component <- as.numeric(gsub("^comp", "", loads$component))
  loads
}

# library("scales")
# loads <-
#   tidy(toxicity.spls) %>%
#   filter(type == "predictors")
#
# rng <- extendrange(loads$value)
# sl <- c(rng[1], 0, rng[2])
#
#
# loads %>%
#   ggplot(aes(x = term, y = component, fill = value)) +
#   geom_tile() +
#   scale_fill_gradientn(
#     colours = c("blue", "white", "red"),
#     values = rescale(sl),
#     guide = "colorbar",
#     limits = rng
#   )
#

