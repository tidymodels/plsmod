# plsmod (development version)

* Transition from the magrittr pipe to the base R pipe.

# plsmod 1.0.0

* Small release to correct HTML tags for CRAN.

# plsmod 0.2.0

* Model definition functions (e.g. `pls()`) were moved to the parsnip package.

# plsmod 0.1.2

 * Release to stop CRAN failures on r-devel from mixOmics. 
 
# plsmod 0.1.1

 * Small updates so that `plsmod` can be run in parallel using psock clusters 
 
 * Updates for encoding requirements related to current version of `parsnip`. 


# plsmod 0.1.0

* The `num_terms` argument was removed in favor of `predictor_prop`, which is the maximum _proportion_ of original predictors that can have non-zero coefficients for each PLS component. This makes it easier to tune and consistent with `recipes::step_pls()`. 

* Fixed a bug that could prevent `multi_predict()` from being invoked. 

# plsmod 0.0.1

* Added a `NEWS.md` file to track changes to the package.
