
<!-- README.md is generated from README.Rmd. Please edit that file -->

# brmstools

brmstools is an R package [available on
GitHub](https://github.com/mvuorre/brmstools).

brmstools provides convenient plotting and post-processing functions for
brmsfit objects (bayesian regression models fitted with the [brms R
package](https://github.com/paul-buerkner/brms)).

Install brmstools from GitHub with
[devtools](https://cran.r-project.org/package=devtools):

``` r
# install.packages("devtools")
devtools::install_github("mvuorre/brmstools")
```

Loading brmstools also loads the brms package:

``` r
library(brmstools)
#> Loading required package: ggplot2
#> Loading required package: brms
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.0.1). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> Run theme_set(theme_default()) to use the default bayesplot theme.
```

brmstools is in beta version so will probably break down with some
inputs: [Suggestions for improvements and bug
reports](https://github.com/mvuorre/brmstools/issues) are warmly
welcomed.

[brms](https://github.com/paul-buerkner/brms) makes it easy to fit
(complex) models with Stan. The flexible `brms::marginal_effects()` and
`brms::plot()` methods allow various visualizations of fitted brms
models. However, I found myself repeatedly writing code for spaghetti,
forest, and panel plots, all of which are difficult to create with
built-in brms functions. This package provides functions for easily
creating these types of figures, and other miscellaneous helper
functions for post-processing brmsfit models.

# Forest plots

Notably, `forest()` draws forest plots for meta-analytic and multilevel
models:

``` r
forest(fit_rem)
#> Picking joint bandwidth of 0.0227
```

<img src="man/figures/README-forest-1.png" width="100%" />

# Panel plots

`panels()` draws panel plots:

``` r
panels(fit_ml, xvar = "Days")
```

<img src="man/figures/README-panels-1.png" width="100%" />

# Spaghetti plots

`spaghetti()` draws spaghetti plots:

``` r
spaghetti(fit_ml, xvar = "Days")
```

<img src="man/figures/README-spaghetti-1.png" width="100%" />

# Coefficient plots

`coefplot()` draws coefficient plots:

``` r
coefplot(fit_ml)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

# More information

Examples and instructions can be found at
<https://mvuorre.github.io/brmstools>.
