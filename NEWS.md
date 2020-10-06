# plsmod 0.1.0

* The `num_terms` argument was removed in favor of `predictor_prop`, which is the maximum _proportion_ of original predictors that can have non-zero coefficients for each PLS component. This makes it easier to tune and consistent with `recipes::step_pls()`. 

* Fixed a bug that could prevent `multi_predict()` from being invoked. 

# plsmod 0.0.1

* Added a `NEWS.md` file to track changes to the package.
