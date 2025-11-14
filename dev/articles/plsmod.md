# plsmod

The plsmod package serves as an extension to the parsnip package that
enables tiydmodels users to fit several types of [partial least squares
models](https://scholar.google.com/scholar?hl=en&as_sdt=0%2C7&q=%22partial+least+squares+models%22&btnG=)
(PLS). If you are unfamiliar with tidymodels, please see the [beginning
help pages as tidymodels.org](https://www.tidymodels.org/start/).

With tidymodels, there are possible *engines* that can be used to fit a
particular model. For PLS, the possible models are:

``` r
library(plsmod)
```

    ## Loading required package: parsnip

``` r
show_engines("pls")
```

    ## # A tibble: 2 × 2
    ##   engine   mode          
    ##   <chr>    <chr>         
    ## 1 mixOmics classification
    ## 2 mixOmics regression

## An example

For a demonstration of regression modeling, well use the [Tecator
data](https://modeldata.tidymodels.org/reference/meats.html) in the
modeldata package:

``` r
library(dplyr)
data(meats, package = "modeldata")
```

Note that using `tidymodels_prefer()` will resulting getting
[`parsnip::pls()`](https://parsnip.tidymodels.org/reference/pls.html)
instead of
[`mixOmics::pls()`](https://rdrr.io/pkg/mixOmics/man/pls.html) when
simply running
[`pls()`](https://parsnip.tidymodels.org/reference/pls.html).

Although plsmod can fit multivariate models, we’ll concentration on a
univariate model that predicts the percentage of protein in the samples.

``` r
meats <- meats |> select(-water, -fat)
```

We define a sparse PLS model by setting the `predictor_prop` argument to
a value less than one. This allows the model fitting process to set
certain loadings to zero via regularization.

``` r
sparse_pls_spec <- 
  pls(num_comp = 10, predictor_prop = 1 / 3) |> 
  set_engine("mixOmics") |> 
  set_mode("regression")
```

The model is fit either with a formula or by passing the predictors and
outcomes separately:

``` r
form_fit <- 
  sparse_pls_spec |> 
  fit(protein ~ ., data = meats)
form_fit
```

    ## parsnip model object
    ## 
    ## 
    ## Call:
    ##  mixOmics::spls(X = x, Y = y, ncomp = ncomp, keepX = keepX) 
    ## 
    ##  sPLS with a 'regression' mode with 10 sPLS components. 
    ##  You entered data X of dimensions: 215 100 
    ##  You entered data Y of dimensions: 215 1 
    ## 
    ##  Selection of [34] [34] [34] [34] [34] [34] [34] [34] [34] [34] variables on each of the sPLS components on the X data set. 
    ##  Selection of [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] variables on each of the sPLS components on the Y data set. 
    ## 
    ##  Main numerical outputs: 
    ##  -------------------- 
    ##  loading vectors: see object$loadings 
    ##  variates: see object$variates 
    ##  variable names: see object$names 
    ## 
    ##  Functions to visualise samples: 
    ##  -------------------- 
    ##  plotIndiv, plotArrow 
    ## 
    ##  Functions to visualise variables: 
    ##  -------------------- 
    ##  plotVar, plotLoadings, network, cim

``` r
# or 

sparse_pls_spec |> 
  fit_xy(x = meats |> select(-protein), y = meats$protein)
```

    ## parsnip model object
    ## 
    ## 
    ## Call:
    ##  mixOmics::spls(X = x, Y = y, ncomp = ncomp, keepX = keepX) 
    ## 
    ##  sPLS with a 'regression' mode with 10 sPLS components. 
    ##  You entered data X of dimensions: 215 100 
    ##  You entered data Y of dimensions: 215 1 
    ## 
    ##  Selection of [34] [34] [34] [34] [34] [34] [34] [34] [34] [34] variables on each of the sPLS components on the X data set. 
    ##  Selection of [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] variables on each of the sPLS components on the Y data set. 
    ## 
    ##  Main numerical outputs: 
    ##  -------------------- 
    ##  loading vectors: see object$loadings 
    ##  variates: see object$variates 
    ##  variable names: see object$names 
    ## 
    ##  Functions to visualise samples: 
    ##  -------------------- 
    ##  plotIndiv, plotArrow 
    ## 
    ##  Functions to visualise variables: 
    ##  -------------------- 
    ##  plotVar, plotLoadings, network, cim

The [`pls()`](https://parsnip.tidymodels.org/reference/pls.html)
function can also be used with categorical outcomes, provided that the
outcome column is an R factor vector.

The number of components and the amount of sparsity can be optimized
using the tune package. See Chapter 17 of [*Tidy Models with
R*](https://www.tmwr.org/tuning.html) for more information and examples
on how to tune model hyperparameters using tidymodels.

For prediction, the basic
[`predict()`](https://rdrr.io/r/stats/predict.html) method can be used:

``` r
predict(form_fit, head(meats))
```

    ## # A tibble: 6 × 1
    ##   .pred
    ##   <dbl>
    ## 1  17.0
    ## 2  13.9
    ## 3  19.8
    ## 4  21.0
    ## 5  16.8
    ## 6  13.4
