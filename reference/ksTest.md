# Kolmogorov-Smirnov Tests.

Performs a one- or two-sample Kolmogorov-Smirnov test. Includes the
option to perform the two-sample test using the formula notation.

## Usage

``` r
ksTest(x, ...)

# Default S3 method
ksTest(
  x,
  y,
  ...,
  alternative = c("two.sided", "less", "greater"),
  exact = NULL
)

# S3 method for class 'formula'
ksTest(
  x,
  data = NULL,
  ...,
  alternative = c("two.sided", "less", "greater"),
  exact = NULL
)
```

## Arguments

- x:

  A numeric vector of data values or a formula (see details).

- ...:

  Parameters of the distribution specified (as a character string) by
  `y`.

- y:

  A numeric vector of data values, a character string naming a
  cumulative distribution function, or an actual cumulative distribution
  function. See [`ks.test`](https://rdrr.io/r/stats/ks.test.html).

- alternative:

  A string that indicates the alternative hypothesis. See
  [`ks.test`](https://rdrr.io/r/stats/ks.test.html).

- exact:

  `NULL` or a logical that indicates whether an exact p-value should be
  computed. See [`ks.test`](https://rdrr.io/r/stats/ks.test.html). Not
  available if ties are present, nor for the one-sided two-sample case.

- data:

  A data frame that contains the variables in the formula for `x`.

## Value

See [`ks.test`](https://rdrr.io/r/stats/ks.test.html).

## Details

This is exactly [`ks.test`](https://rdrr.io/r/stats/ks.test.html) except
that a formula may be used for the two-sample situation. The default
version is simply a pass through to
[`ks.test`](https://rdrr.io/r/stats/ks.test.html). See
[`ks.test`](https://rdrr.io/r/stats/ks.test.html) for more details.

## See also

[`ks.test`](https://rdrr.io/r/stats/ks.test.html).

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
## see ks.test for other examples
x <- rnorm(50)
y <- runif(30)
df <- data.frame(dat=c(x,y),
                 grp=factor(rep(c("x","y"),c(50,30))),
                 stringsAsFactors=FALSE)

## one-sample (from ks.test) still works
ksTest(x+2, "pgamma", 3, 2)
#> 
#>  Exact one-sample Kolmogorov-Smirnov test
#> 
#> data:  x
#> D = 0.18447, p-value = 0.05816
#> alternative hypothesis: two-sided
#> 
ks.test(x+2, "pgamma", 3, 2)
#> 
#>  Exact one-sample Kolmogorov-Smirnov test
#> 
#> data:  x + 2
#> D = 0.18447, p-value = 0.05816
#> alternative hypothesis: two-sided
#> 

## first two-sample example in ?ks.test
ksTest(x,y)
#> 
#>  Exact two-sample Kolmogorov-Smirnov test
#> 
#> data:  x and y
#> D = 0.62, p-value = 2.964e-07
#> alternative hypothesis: two-sided
#> 
ks.test(x,y)
#> 
#>  Exact two-sample Kolmogorov-Smirnov test
#> 
#> data:  x and y
#> D = 0.62, p-value = 2.964e-07
#> alternative hypothesis: two-sided
#> 

## same as above but using data.frame and formula
ksTest(dat~grp,data=df)
#> 
#>  Exact two-sample Kolmogorov-Smirnov test
#> 
#> data:  x and y
#> D = 0.62, p-value = 2.964e-07
#> alternative hypothesis: two-sided
#> 
```
