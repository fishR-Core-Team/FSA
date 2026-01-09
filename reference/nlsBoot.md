# Associated S3 methods for nlsBoot from nlstools.

Provides S3 methods to construct non-parametric bootstrap confidence
intervals and hypothesis tests for parameter values and predicted values
of the response variable for a
[`nlsBoot`](https://rdrr.io/pkg/nlstools/man/nlsBoot.html) object from
the nlstools package.

## Usage

``` r
# S3 method for class 'nlsBoot'
confint(
  object,
  parm = NULL,
  level = conf.level,
  conf.level = 0.95,
  plot = FALSE,
  err.col = "black",
  err.lwd = 2,
  rows = NULL,
  cols = NULL,
  ...
)

# S3 method for class 'nlsBoot'
predict(object, FUN, conf.level = 0.95, digits = NULL, ...)

htest(object, ...)

# S3 method for class 'nlsBoot'
htest(
  object,
  parm = NULL,
  bo = 0,
  alt = c("two.sided", "less", "greater"),
  plot = FALSE,
  ...
)
```

## Arguments

- object:

  An object saved from `nlsBoot()`.

- parm:

  An integer that indicates which parameter to compute the confidence
  interval or hypothesis test for. The confidence interval Will be
  computed for all parameters if `NULL`.

- level:

  Same as `conf.level`. Used for compatibility with the main `confint`.

- conf.level:

  A level of confidence as a proportion.

- plot:

  A logical that indicates whether a plot should be constructed. If
  `confint`, then a histogram of the `parm` parameters from the
  bootstrap samples with error bars that illustrate the bootstrapped
  confidence intervals will be constructed. If `htest`, then a histogram
  of the `parm` parameters with a vertical lines illustrating the
  `bo`value will be constructed.

- err.col:

  A single numeric or character that identifies the color for the error
  bars on the plot.

- err.lwd:

  A single numeric that identifies the line width for the error bars on
  the plot.

- rows:

  A numeric that contains the number of rows to use on the graphic.

- cols:

  A numeric that contains the number of columns to use on the graphic.

- ...:

  Additional arguments to functions.

- FUN:

  The function to be applied for the prediction. See the examples.

- digits:

  A single numeric that indicates the number of digits for the result.

- bo:

  The null hypothesized parameter value.

- alt:

  A string that identifies the “direction” of the alternative
  hypothesis. See details.

## Value

`confint` returns a matrix with as many rows as columns (i.e., parameter
estimates) in the `object$coefboot` data frame and two columns of the
quantiles that correspond to the approximate confidence interval.

`htest` returns a matrix with two columns. The first column contains the
hypothesized value sent to this function and the second column is the
corresponding p-value.

`predict` returns a matrix with one row and three columns, with the
first column holding the predicted value (i.e., the median prediction)
and the last two columns holding the approximate confidence interval.

## Details

`confint` finds the two quantiles that have the proportion
(1-`conf.level`)/2 of the bootstrapped parameter estimates below and
above. This is an approximate 100`conf.level`% confidence interval.

In `htest` the “direction” of the alternative hypothesis is identified
by a string in the `alt=` argument. The strings may be `"less"` for a
“less than” alternative, `"greater"` for a “greater than” alternative,
or `"two.sided"` for a “not equals” alternative (the DEFAULT). In the
one-tailed alternatives the p-value is the proportion of bootstrapped
parameter estimates in `object$coefboot` that are extreme of the null
hypothesized parameter value in `bo`. In the two-tailed alternative the
p-value is twice the smallest of the proportion of bootstrapped
parameter estimates above or below the null hypothesized parameter value
in `bo`.

In `predict`, a user-supplied function is applied to each row of the
`coefBoot` object in a `nlsBoot` object and then finds the median and
the two quantiles that have the proportion (1-`conf.level`)/2 of the
bootstrapped predictions below and above. The median is returned as the
predicted value and the quantiles are returned as an approximate
100`conf.level`% confidence interval for that prediction.

## See also

[`Boot`](https://rdrr.io/pkg/car/man/Boot.html) and related methods in
car and
`summary.`[`nlsBoot`](https://rdrr.io/pkg/nlstools/man/nlsBoot.html) in
nlstools.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
fnx <- function(days,B1,B2,B3) {
  if (length(B1) > 1) {
    B2 <- B1[2]
    B3 <- B1[3]
    B1 <- B1[1]
  }
  B1/(1+exp(B2+B3*days))
}
nl1 <- nls(cells~fnx(days,B1,B2,B3),data=Ecoli,
           start=list(B1=6,B2=7.2,B3=-1.45))
if (require(nlstools)) {
  nl1.bootn <-  nlstools::nlsBoot(nl1,niter=99) # too few to be useful
  confint(nl1.bootn,"B1")
  confint(nl1.bootn,c(2,3))
  confint(nl1.bootn,conf.level=0.90)
  confint(nl1.bootn,plot=TRUE)
  predict(nl1.bootn,fnx,days=3)
  predict(nl1.bootn,fnx,days=1:3)
  htest(nl1.bootn,1,bo=6,alt="less")
}
#> Loading required package: nlstools
#> 
#> 'nlstools' has been loaded.
#> IMPORTANT NOTICE: Most nonlinear regression models and data set examples
#> related to predictive microbiolgy have been moved to the package 'nlsMicrobio'
#> Error in nlstools::nlsBoot(nl1, niter = 99): Procedure aborted: the fit only converged in 1 % during bootstrapping
```
