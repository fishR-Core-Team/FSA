# Confidence interval for Poisson counts.

Computes a confidence interval for the Poisson counts.

## Usage

``` r
poiCI(
  x,
  conf.level = 0.95,
  type = c("exact", "daly", "byar", "asymptotic"),
  verbose = FALSE
)
```

## Arguments

- x:

  A single number or vector that represents the number of observed
  successes.

- conf.level:

  A number that indicates the level of confidence to use for
  constructing confidence intervals (default is `0.95`).

- type:

  A string that identifies the type of method to use for the
  calculations. See details.

- verbose:

  A logical that indicates whether `x` should be included in the
  returned matrix (`=TRUE`) or not (`=FALSE`; DEFAULT).

## Value

A \#x2 matrix that contains the lower and upper confidence interval
bounds as columns and, if `verbose=TRUE` `x`.

## Details

Computes a CI for the Poisson counts using the `exact`, gamma
distribution (`daly`\`), Byar's (`byar`), or normal approximation
(`asymptotic`) methods.

The `pois.daly` function gives essentially identical answers to the
`pois.exact` function except when x=0. When x=0, for the upper
confidence limit `pois.exact` returns 3.689 and `pois.daly` returns
2.996.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>, though this is largely based on
`pois.exact`, `pois.daly`, `pois.byar`, and `pois.approx` from the old
epitools package.

## Examples

``` r
## Demonstrates using all types at once
poiCI(12)
#>             95% LCI  95% UCI
#> Exact      6.200603 20.96156
#> Daly       6.200575 20.96159
#> Byar       6.552977 20.32447
#> Asymptotic 5.210486 18.78951

## Selecting types
poiCI(12,type="daly")
#>   95% LCI  95% UCI
#>  6.200575 20.96159
poiCI(12,type="byar")
#>   95% LCI  95% UCI
#>  6.552977 20.32447
poiCI(12,type="asymptotic")
#>   95% LCI  95% UCI
#>  5.210486 18.78951
poiCI(12,type="asymptotic",verbose=TRUE)
#>             x  95% LCI  95% UCI
#> Asymptotic 12 5.210486 18.78951
poiCI(12,type=c("exact","daly"))
#>        95% LCI  95% UCI
#> Exact 6.200603 20.96156
#> Daly  6.200575 20.96159
poiCI(12,type=c("exact","daly"),verbose=TRUE)
#>        x  95% LCI  95% UCI
#> Exact 12 6.200603 20.96156
#> Daly  12 6.200575 20.96159

## Demonstrates use with multiple inputs
poiCI(c(7,10),type="exact")
#>   95% LCI  95% UCI
#>  2.814358 14.42268
#>  4.795389 18.39036
poiCI(c(7,10),type="exact",verbose=TRUE)
#>       x  95% LCI  95% UCI
#> [1,]  7 2.814358 14.42268
#> [2,] 10 4.795389 18.39036
```
