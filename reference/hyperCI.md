# Confidence interval for population size (N) in hypergeometric distribution.

Computes a confidence interval for population size (N) in hypergeometric
distribution.

## Usage

``` r
hyperCI(M, n, m, conf.level = 0.95)
```

## Arguments

- M:

  Number of successes in the population.

- n:

  Number of observations in the sample.

- m:

  Number of observed successes in the sample.

- conf.level:

  Level of confidence to use for constructing confidence intervals
  (default is `0.95`).

## Value

A 1x2 matrix that contains the lower and upper confidence interval
bounds.

## Details

This is an inefficient brute-force algorithm. The algorithm computes the
`conf.level` range of possible values for `m`, as if it was unknown, for
a large range of values of N. It then finds all possible values of N for
which `m` was in the `conf.level` range. The smallest and largest values
of N for which `m` was in the `conf.level` range are the CI endpoints.

## Note

This algorithm is experimental at this point.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
hyperCI(50,25,10)
#>      95% LCI 95% UCI
#> [1,]      86     228
```
