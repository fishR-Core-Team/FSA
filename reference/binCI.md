# Confidence intervals for binomial probability of success.

Uses one of three methods to compute a confidence interval for the
probability of success (p) in a binomial distribution.

## Usage

``` r
binCI(
  x,
  n,
  conf.level = 0.95,
  type = c("wilson", "exact", "asymptotic"),
  verbose = FALSE
)
```

## Arguments

- x:

  A single or vector of numbers that contains the number of observed
  successes.

- n:

  A single or vector of numbers that contains the sample size.

- conf.level:

  A single number that indicates the level of confidence (default is
  `0.95`).

- type:

  A string that identifies the type of method to use for the
  calculations. See details.

- verbose:

  A logical that indicates whether `x`, `n`, and `x/n` should be
  included in the returned matrix (`=TRUE`) or not (`=FALSE`; DEFAULT).

## Value

A \#x2 matrix that contains the lower and upper confidence interval
bounds as columns and, if `verbose=TRUE` `x`, `n`, and `x/n` .

## Details

This function will compute confidence interval for three possible
methods chosen with the `type` argument.

|                     |                                                                                                                                                                                                           |
|---------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `type="wilson"`     | Wilson's (Journal of the American Statistical Association, 1927) confidence interval for a proportion. This is the score CI, based on inverting the asymptotic normal test using the null standard error. |
| `type="exact"`      | Computes the Clopper/Pearson exact CI for a binomial success probability.                                                                                                                                 |
| `type="asymptotic"` | This uses the normal distribution approximation.                                                                                                                                                          |

Note that Agresti and Coull (2000) suggest that the Wilson interval is
the preferred method and is, thus, the default `type`.

## References

Agresti, A. and B.A. Coull. 1998. Approximate is better than “exact” for
interval estimation of binomial proportions. American Statistician,
52:119-126.

## See also

See [`binom.test`](https://rdrr.io/r/stats/binom.test.html); `binconf`
in Hmisc; and functions in binom.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>, though this is largely based on
`binom.exact`, `binom.wilson`, and `binom.approx` from the old epitools
package.

## Examples

``` r
## All types at once
binCI(7,20)
#>              95% LCI   95% UCI
#> Exact      0.1539092 0.5921885
#> Wilson     0.1811918 0.5671457
#> Asymptotic 0.1409627 0.5590373

## Individual types
binCI(7,20,type="wilson")
#>    95% LCI   95% UCI
#>  0.1811918 0.5671457
binCI(7,20,type="exact")
#>    95% LCI   95% UCI
#>  0.1539092 0.5921885
binCI(7,20,type="asymptotic")
#>    95% LCI   95% UCI
#>  0.1409627 0.5590373
binCI(7,20,type="asymptotic",verbose=TRUE)
#>            x  n proportion   95% LCI   95% UCI
#> Asymptotic 7 20       0.35 0.1409627 0.5590373

## Multiple types
binCI(7,20,type=c("exact","asymptotic"))
#>              95% LCI   95% UCI
#> Exact      0.1539092 0.5921885
#> Asymptotic 0.1409627 0.5590373
binCI(7,20,type=c("exact","asymptotic"),verbose=TRUE)
#>            x  n proportion   95% LCI   95% UCI
#> Exact      7 20       0.35 0.1539092 0.5921885
#> Asymptotic 7 20       0.35 0.1409627 0.5590373

## Use with multiple inputs
binCI(c(7,10),c(20,30),type="wilson")
#>    95% LCI   95% UCI
#>  0.1811918 0.5671457
#>  0.1923050 0.5121995
binCI(c(7,10),c(20,30),type="wilson",verbose=TRUE)
#>       x  n proportion   95% LCI   95% UCI
#> [1,]  7 20  0.3500000 0.1811918 0.5671457
#> [2,] 10 30  0.3333333 0.1923050 0.5121995
```
