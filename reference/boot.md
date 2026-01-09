# Associated S3 methods for bootstrap results from car::Boot.

S3 methods are provided to construct non-parametric bootstrap confidence
intervals, predictions with non-parametric confidence intervals,
hypothesis tests, and plots of the parameter estimates for objects
returned from [`Boot`](https://rdrr.io/pkg/car/man/Boot.html) from car.

## Usage

``` r
# S3 method for class 'boot'
confint(
  object,
  parm = NULL,
  level = conf.level,
  conf.level = 0.95,
  type = c("bca", "norm", "basic", "perc"),
  plot = FALSE,
  err.col = "black",
  err.lwd = 2,
  rows = NULL,
  cols = NULL,
  ...
)

# S3 method for class 'boot'
htest(
  object,
  parm = NULL,
  bo = 0,
  alt = c("two.sided", "less", "greater"),
  plot = FALSE,
  ...
)

# S3 method for class 'boot'
predict(object, FUN, conf.level = 0.95, digits = NULL, ...)

# S3 method for class 'boot'
hist(
  x,
  same.ylim = TRUE,
  ymax = NULL,
  rows = round(sqrt(ncol(x$t))),
  cols = ceiling(sqrt(ncol(x$t))),
  ...
)
```

## Arguments

- object, x:

  An object of class `boot` from
  [`Boot`](https://rdrr.io/pkg/car/man/Boot.html).

- parm:

  A number or string that indicates which column of `object` contains
  the parameter estimates to use for the confidence interval or
  hypothesis test.

- level:

  Same as `conf.level`.

- conf.level:

  A level of confidence as a proportion.

- type:

  Confidence interval type; types implemented are the "percentile"
  method, which uses the function quantile to return the appropriate
  quantiles for the confidence limit specified, the default bca which
  uses the bias-corrected and accelerated method presented by Efron and
  Tibshirani (1993, Chapter 14). For the other types, see the
  documentation for [`boot`](https://rdrr.io/pkg/boot/man/boot.html).

- plot:

  A logical that indicates whether a plot should be constructed. If
  `confint` then a histogram of the `parm` parameters from the bootstrap
  samples with error bars that illustrate the bootstrapped confidence
  intervals will be constructed. If `htest` then a histogram of the
  `parm` parameters with a vertical line illustrating the `bo` value
  will be constructed.

- err.col:

  A single numeric or character that identifies the color for the error
  bars on the plot.

- err.lwd:

  A single numeric that identifies the line width for the error bars on
  the plot.

- rows:

  A single numeric that contains the number of rows to use on the
  graphic.

- cols:

  A single numeric that contains the number of columns to use on the
  graphic.

- ...:

  Additional items to send to functions. See details.

- bo:

  The null hypothesized parameter value.

- alt:

  A string that indicates the “direction” of the alternative hypothesis.
  See details.

- FUN:

  The function to be applied for the prediction. See the examples.

- digits:

  A single numeric that indicates the number of digits for the result.

- same.ylim:

  A logical that indicates whether the same limits for the y-axis should
  be used on each histogram. Defaults to `TRUE`. Ignored if `ylmts` is
  non-null.

- ymax:

  A single value that sets the maximum y-axis limit for each histogram
  or a vector of length equal to the number of groups that sets the
  maximum y-axis limit for each histogram separately.

- col:

  A named color for the histogram bars.

## Value

If `object` is a matrix, then `confint` returns a matrix with as many
rows as columns (i.e., parameter estimates) in `object` and two columns
of the quantiles that correspond to the approximate confidence interval.
If `object` is a vector, then `confint` returns a vector with the two
quantiles that correspond to the approximate confidence interval.

`htest` returns a two-column matrix with the first column containing the
hypothesized value sent to this function and the second column
containing the corresponding p-value.

`hist` constructs histograms of the bootstrapped parameter estimates.

`plot` constructs scatterplots of all pairs of bootstrapped parameter
estimates.

`predict` returns a matrix with one row and three columns, with the
first column holding the predicted value (i.e., the median prediction)
and the last two columns holding the approximate confidence interval.

## Details

`confint` is largely a wrapper for
[`Confint`](https://rdrr.io/pkg/car/man/S.html) from car (see its manual
page).

`predict` applies a user-supplied function to each row of `object` and
then finds the median and the two quantiles that have the proportion
(1-`conf.level`)/2 of the bootstrapped predictions below and above. The
median is returned as the predicted value and the quantiles are returned
as an approximate 100`conf.level`% confidence interval for that
prediction. Values for the independent variable in `FUN` must be a named
argument sent in the ... argument (see examples). Note that if other
arguments are needed in `FUN` besides values for the independent
variable, then these are included in the ... argument AFTER the values
for the independent variable.

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

## References

S. Weisberg (2005). *Applied Linear Regression*, third edition. New
York: Wiley, Chapters 4 and 11.

## See also

[`Boot`](https://rdrr.io/pkg/car/man/Boot.html) in car.

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

if (require(car)) {
  nl1.bootc <- car::Boot(nl1,f=coef,R=99)  # R=99 too few to be useful
  confint(nl1.bootc,"B1")
  confint(nl1.bootc,c(2,3))
  confint(nl1.bootc,conf.level=0.90)
  confint(nl1.bootc,"B1",plot=TRUE)
  htest(nl1.bootc,1,bo=6,alt="less")
  htest(nl1.bootc,1,bo=6,alt="less",plot=TRUE)
  predict(nl1.bootc,fnx,days=1:3)
  predict(nl1.bootc,fnx,days=3)
  hist(nl1.bootc)
}
#> Loading required package: car
#> Loading required package: carData
#> 
#> Attaching package: ‘car’
#> The following object is masked from ‘package:FSA’:
#> 
#>     bootCase
#> Error in fnx(days, B1, B2, B3): could not find function "fnx"
```
