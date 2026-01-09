# Dunn's Kruskal-Wallis Multiple Comparisons.

Performs Dunn's (1964) test of multiple comparisons following a
significant Kruskal-Wallis test, possibly with a correction to control
the experimentwise error rate. This is largely a wrapper for the
[`dunn.test`](https://rdrr.io/pkg/dunn.test/man/dunn.test.html) function
in dunn.test. Please see and cite that package.

## Usage

``` r
dunnTest(x, ...)

# Default S3 method
dunnTest(
  x,
  g,
  method = dunn.test::p.adjustment.methods[c(4, 2:3, 5:8, 1)],
  two.sided = TRUE,
  altp = two.sided,
  ...
)

# S3 method for class 'formula'
dunnTest(
  x,
  data = NULL,
  method = dunn.test::p.adjustment.methods[c(4, 2:3, 5:8, 1)],
  two.sided = TRUE,
  altp = two.sided,
  ...
)

# S3 method for class 'dunnTest'
print(x, dunn.test.results = FALSE, ...)
```

## Arguments

- x:

  A numeric vector of data values or a formula of the form x~g.

- ...:

  Not yet used.

- g:

  A factor vector or a (non-numeric) vector that can be coerced to a
  factor vector.

- method:

  A single string that identifies the method used to control the
  experimentwise error rate. See the list of methods in
  `p.adjustment.methods` (documented with
  [`dunn.test`](https://rdrr.io/pkg/dunn.test/man/dunn.test.html)) in
  dunn.test.

- two.sided:

  A single logical that indicates whether a two-sided p-value should be
  returned (`TRUE`; default) or not. See details.

- altp:

  Same as `two.sided`. Allows similar code with the
  [`dunn.test`](https://rdrr.io/pkg/dunn.test/man/dunn.test.html)
  function in dunn.test. `two.sided` is maintained because it pre-dates
  `altp`.

- data:

  A data.frame that minimally contains `x` and `g`.

- dunn.test.results:

  A single logical that indicates whether the results that would have
  been printed by
  [`dunn.test`](https://rdrr.io/pkg/dunn.test/man/dunn.test.html)
  function in dunn.test are shown.

## Value

A list with three items – `method` is the long name of the method used
to control the experimentwise error rate, `dtres` is the strings that
would have been printed by the
[`dunn.test`](https://rdrr.io/pkg/dunn.test/man/dunn.test.html) function
in dunn.test, and `res` is a data.frame with the following variables:

- Comparison: Labels for each pairwise comparison.

- Z: Values for the Z test statistic for each comparison.

- P.unadj: Unadjusted p-values for each comparison.

- P.adj: Adjusted p-values for each comparison.

## Details

This function performs “Dunn's” test of multiple comparisons following a
Kruskal-Wallis test. Unadjusted one- or two-sided p-values for each
pairwise comparison among groups are computed following Dunn's
description as implemented in the
[`dunn.test`](https://rdrr.io/pkg/dunn.test/man/dunn.test.html) function
from dunn.test. These p-values may be adjusted using methods in the
`p.adjustment.methods` function in dunn.test.

This function is largely a wrapper for the
[`dunn.test`](https://rdrr.io/pkg/dunn.test/man/dunn.test.html) function
in dunn.test. Changes here are the possible use of formula notation,
results not printed by the main function (but are printed in a more
useful format (in my opinion) by the `print` function), the p-values are
adjusted by default with the “holm” method, and two-sided p-values are
returned by default. See
[`dunn.test`](https://rdrr.io/pkg/dunn.test/man/dunn.test.html) function
in dunn.test for more details underlying these computations.

## Note

The data.frame will be reduced to only those rows that are complete
cases for `x` and `g`. In other words, rows with missing data for either
`x` or `g` are removed from the analysis and a warning will be issued.

There are a number of functions in other packages that do similar
analyses.

The results from `DunnTest` match the results (in a different format)
from the [`dunn.test`](https://rdrr.io/pkg/dunn.test/man/dunn.test.html)
function from dunn.test.

The `pairw.kw` function from the asbio package performs the Dunn test
with the Bonferroni correction. The `pairw.kw` also provides a
confidence interval for the difference in mean ranks between pairs of
groups. The p-value results from `DunnTest` match the results from
`pairw.kw`.

The `posthoc.kruskal.nemenyi.test` function from the PMCMR package uses
the “Nemenyi” (1963) method of multiple comparisons.

The `kruskalmc` function from the pgirmess package uses the method
described by Siegel and Castellan (1988).

It is not clear which method `kruskal` from the agricolae package uses.
It does not seem to output p-values but it does allow for a wide variety
of methods to control the experimentwise error rate.

## References

Dunn, O.J. 1964. Multiple comparisons using rank sums. Technometrics
6:241-252.

## See also

See `kruskal.test`,
[`dunn.test`](https://rdrr.io/pkg/dunn.test/man/dunn.test.html) in
dunn.test, `posthoc.kruskal.nemenyi.test` in PMCMR, `kruskalmc` in
pgirmess, and `kruskal` in agricolae.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>, but this is largely a wrapper
(see details) for
[`dunn.test`](https://rdrr.io/pkg/dunn.test/man/dunn.test.html) in
dunn.test written by Alexis Dinno.

## Examples

``` r
## pH in four ponds data from Zar (2010)
ponds <- data.frame(pond=as.factor(rep(1:4,each=8)),
                    pH=c(7.68,7.69,7.70,7.70,7.72,7.73,7.73,7.76,
                         7.71,7.73,7.74,7.74,7.78,7.78,7.80,7.81,
                         7.74,7.75,7.77,7.78,7.80,7.81,7.84,NA,
                         7.71,7.71,7.74,7.79,7.81,7.85,7.87,7.91))
ponds2 <- ponds[complete.cases(ponds),]

# non-formula usage (default "holm" method)
dunnTest(ponds2$pH,ponds2$pond)
#> Dunn (1964) Kruskal-Wallis multiple comparison
#>   p-values adjusted with the Holm method.
#>   Comparison           Z     P.unadj      P.adj
#> 1      1 - 2 -2.13700630 0.032597479 0.13038992
#> 2      1 - 3 -2.94934889 0.003184443 0.01592221
#> 3      2 - 3 -0.88480467 0.376261991 1.00000000
#> 4      1 - 4 -2.99180882 0.002773299 0.01663979
#> 5      2 - 4 -0.85480252 0.392660483 0.78532097
#> 6      3 - 4  0.05898698 0.952962480 0.95296248

# formula usage (default "holm" method)
dunnTest(pH~pond,data=ponds2)
#> Dunn (1964) Kruskal-Wallis multiple comparison
#>   p-values adjusted with the Holm method.
#>   Comparison           Z     P.unadj      P.adj
#> 1      1 - 2 -2.13700630 0.032597479 0.13038992
#> 2      1 - 3 -2.94934889 0.003184443 0.01592221
#> 3      2 - 3 -0.88480467 0.376261991 1.00000000
#> 4      1 - 4 -2.99180882 0.002773299 0.01663979
#> 5      2 - 4 -0.85480252 0.392660483 0.78532097
#> 6      3 - 4  0.05898698 0.952962480 0.95296248

# other methods
dunnTest(pH~pond,data=ponds2,method="bonferroni")
#> Dunn (1964) Kruskal-Wallis multiple comparison
#>   p-values adjusted with the Bonferroni method.
#>   Comparison           Z     P.unadj      P.adj
#> 1      1 - 2 -2.13700630 0.032597479 0.19558488
#> 2      1 - 3 -2.94934889 0.003184443 0.01910666
#> 3      2 - 3 -0.88480467 0.376261991 1.00000000
#> 4      1 - 4 -2.99180882 0.002773299 0.01663979
#> 5      2 - 4 -0.85480252 0.392660483 1.00000000
#> 6      3 - 4  0.05898698 0.952962480 1.00000000
dunnTest(pH~pond,data=ponds2,method="bh")
#> Dunn (1964) Kruskal-Wallis multiple comparison
#>   p-values adjusted with the Benjamini-Hochberg method.
#>   Comparison           Z     P.unadj       P.adj
#> 1      1 - 2 -2.13700630 0.032597479 0.065194958
#> 2      1 - 3 -2.94934889 0.003184443 0.009553328
#> 3      2 - 3 -0.88480467 0.376261991 0.564392987
#> 4      1 - 4 -2.99180882 0.002773299 0.016639793
#> 5      2 - 4 -0.85480252 0.392660483 0.471192580
#> 6      3 - 4  0.05898698 0.952962480 0.952962480
dunnTest(pH~pond,data=ponds2,method="none")
#> Dunn (1964) Kruskal-Wallis multiple comparison
#>   with no adjustment for p-values.
#>   Comparison           Z     P.unadj       P.adj
#> 1      1 - 2 -2.13700630 0.032597479 0.032597479
#> 2      1 - 3 -2.94934889 0.003184443 0.003184443
#> 3      2 - 3 -0.88480467 0.376261991 0.376261991
#> 4      1 - 4 -2.99180882 0.002773299 0.002773299
#> 5      2 - 4 -0.85480252 0.392660483 0.392660483
#> 6      3 - 4  0.05898698 0.952962480 0.952962480

# one-sided
dunnTest(pH~pond,data=ponds2,two.sided=FALSE)
#> Dunn (1964) Kruskal-Wallis multiple comparison
#>   p-values adjusted with the Holm method.
#>   Comparison           Z     P.unadj       P.adj
#> 1      1 - 2 -2.13700630 0.016298740 0.065194958
#> 2      1 - 3 -2.94934889 0.001592221 0.007961106
#> 3      2 - 3 -0.88480467 0.188130996 0.564392987
#> 4      1 - 4 -2.99180882 0.001386649 0.008319896
#> 5      2 - 4 -0.85480252 0.196330241 0.392660483
#> 6      3 - 4  0.05898698 0.476481240 0.476481240

# warning message if incomplete cases were removed
dunnTest(pH~pond,data=ponds)
#> Warning: Some rows deleted from 'x' and 'g' because missing data.
#> Dunn (1964) Kruskal-Wallis multiple comparison
#>   p-values adjusted with the Holm method.
#>   Comparison           Z     P.unadj      P.adj
#> 1      1 - 2 -2.13700630 0.032597479 0.13038992
#> 2      1 - 3 -2.94934889 0.003184443 0.01592221
#> 3      2 - 3 -0.88480467 0.376261991 1.00000000
#> 4      1 - 4 -2.99180882 0.002773299 0.01663979
#> 5      2 - 4 -0.85480252 0.392660483 0.78532097
#> 6      3 - 4  0.05898698 0.952962480 0.95296248

# print dunn.test results
tmp <- dunnTest(pH~pond,data=ponds2)
print(tmp,dunn.test.results=TRUE)
#>   Kruskal-Wallis rank sum test 
#>   
#>  data: x and g 
#>  Kruskal-Wallis chi-squared = 11.9435, df = 3, p-value = 0.01 
#>   
#>   
#>                               Comparison of x by g                               
#>                                      (Holm)                                      
#>  Col Mean-| 
#>  Row Mean |          1          2          3 
#>  ---------+--------------------------------- 
#>         2 |  -2.137006 
#>           |     0.1304 
#>           | 
#>         3 |  -2.949348  -0.884804 
#>           |    0.0159*     1.0000 
#>           | 
#>         4 |  -2.991808  -0.854802   0.058986 
#>           |    0.0166*     0.7853     0.9530 
#>   
#>  alpha = 0.05 
#>  Reject Ho if p <= alpha 
```
