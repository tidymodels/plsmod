suppressPackageStartupMessages(library(mixOmics))

if (rlang::is_installed("modeldata")) {
  penguins <- na.omit(modeldata::penguins)

  pen_for_test <- 1:10
  pen_y_tr <- penguins$species[-pen_for_test]

  pen_x_vars <- c(
    "bill_length_mm",
    "bill_depth_mm",
    "flipper_length_mm",
    "body_mass_g"
  )
  pen_x_tr <- penguins[-pen_for_test, pen_x_vars]
  pen_x_te <- penguins[pen_for_test, pen_x_vars]

  species_lvls <- levels(penguins$species)

  ###

  meats_for_test <- 1:10
  meats_y_tr <- modeldata::meats[-meats_for_test, c("water", "fat", "protein")]
  meats_y_te <- modeldata::meats[meats_for_test, c("water", "fat", "protein")]

  meats_x_tr <- modeldata::meats[-meats_for_test, 1:100]
  meats_x_te <- modeldata::meats[meats_for_test, 1:100]
}
