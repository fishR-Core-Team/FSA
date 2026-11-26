# Summary statistics for a numeric variable.

Summary statistics for a single numeric variable, possibly separated by
the levels of a factor variable or variables. This function is very
similar to [`summary`](https://rdrr.io/r/base/summary.html) for a
numeric variable.

## Usage

``` r
Summarize(object, ...)

# Default S3 method
Summarize(
  object,
  digits = getOption("digits"),
  na.rm = TRUE,
  exclude = NULL,
  nvalid = c("different", "always", "never"),
  percZero = c("different", "always", "never"),
  ...
)

# S3 method for class 'formula'
Summarize(
  object,
  data = NULL,
  digits = getOption("digits"),
  na.rm = TRUE,
  exclude = NULL,
  nvalid = c("different", "always", "never"),
  percZero = c("different", "always", "never"),
  ...
)
```

## Arguments

- object:

  A vector of numeric data.

- ...:

  Not implemented.

- digits:

  A single numeric that indicates the number of decimals to round the
  numeric summaries.

- na.rm:

  A logical that indicates whether numeric missing values (`NA`) should
  be removed (`=TRUE`, default) or not.

- exclude:

  A string that contains the level that should be excluded from a factor
  variable.

- nvalid:

  A string that indicates how the “validn” result will be handled. If
  `"always"` then “validn” will always be shown and if `"never"` then
  “validn” will never be shown. However, if `"different"` (DEFAULT),
  then “validn” will only be shown if it differs from “n” (or if at
  least one group differs from “n” when summarized by multiple groups).

- percZero:

  A string that indicates how the “percZero” result will be handled. If
  `"always"` then “percZero” will always be shown and if `"never"` then
  “percZero” will never be shown. However, if `"different"` (DEFAULT),
  then “percZero” will only be shown if it is greater than zero (or if
  at least one group is greater than zero when summarized by multiple
  groups).

- data:

  A data.frame that contains the variables in `formula`.

## Value

A named vector or data frame (when a quantitative variable is separated
by one or two factor variables) of summary statistics for numeric data.

## Details

This function is primarily used with formulas of the following types
(where `quant` and `factor` generically represent quantitative/numeric
and factor variables, respectively):

|  |  |
|----|----|
| Formula | Description of Summary |
| `~quant` | Numerical summaries (see below) of `quant`. |
| `quant~factor` | Summaries of `quant` separated by levels in `factor`. |
| `quant~factor1*factor2` | Summaries of `quant` separated by the combined levels in `factor1` and `factor2`. |

Numerical summaries include all results from
[`summary`](https://rdrr.io/r/base/summary.html) (min, Q1, mean, median,
Q3, and max) and the sample size, valid sample size (sample size minus
number of `NA`s), and standard deviation (i.e., `sd`). `NA` values are
removed from the calculations with `na.rm=TRUE` (the DEFAULT). The
number of digits in the returned results are controlled with `digits=`.

## Note

Students often need to examine basic statistics of a quantitative
variable separated for different levels of a categorical variable. These
results may be obtained with
[`tapply`](https://rdrr.io/r/base/tapply.html),
[`by`](https://rdrr.io/r/base/by.html), or
[`aggregate`](https://rdrr.io/r/stats/aggregate.html) (or with functions
in other packages), but the use of these functions is not obvious to
newbie students or return results in a format that is not obvious to
newbie students. Thus, the formula method to `Summarize` allows newbie
students to use a common notation (i.e., formula) to easily compute
summary statistics for a quantitative variable separated by the levels
of a factor.

## See also

See [`summary`](https://rdrr.io/r/base/summary.html) for related one
dimensional functionality. See
[`tapply`](https://rdrr.io/r/base/tapply.html), `summaryBy` in doBy,
`describe` in psych, `describe` in prettyR, and `basicStats` in fBasics
for similar “by” functionality.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
## Create a data.frame of "data"
n <- 102
d <- data.frame(y=c(0,0,NA,NA,NA,runif(n-5)),
                w=sample(7:9,n,replace=TRUE),
                v=sample(0:2,n,replace=TRUE),
                g1=factor(sample(c("A","B","C",NA),n,replace=TRUE)),
                g2=factor(sample(c("male","female","UNKNOWN"),n,replace=TRUE)),
                g3=sample(c("a","b","c","d"),n,replace=TRUE),
                stringsAsFactors=FALSE)

# typical output of summary() for a numeric variable
summary(d$y)   
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
#>  0.0000  0.2334  0.4660  0.4649  0.6844  0.9971       3 

# this function           
Summarize(d$y,digits=3)
#>        n   nvalid     mean       sd      min       Q1   median       Q3 
#>  102.000   99.000    0.465    0.279    0.000    0.233    0.466    0.684 
#>      max percZero 
#>    0.997    2.020 
Summarize(~y,data=d,digits=3)
#>        n   nvalid     mean       sd      min       Q1   median       Q3 
#>  102.000   99.000    0.465    0.279    0.000    0.233    0.466    0.684 
#>      max percZero 
#>    0.997    2.020 
Summarize(y~1,data=d,digits=3)
#>        n   nvalid     mean       sd      min       Q1   median       Q3 
#>  102.000   99.000    0.465    0.279    0.000    0.233    0.466    0.684 
#>      max percZero 
#>    0.997    2.020 

# note that nvalid is not shown if there are no NAs and
#   percZero is not shown if there are no zeros
Summarize(~w,data=d,digits=3)
#>       n    mean      sd     min      Q1  median      Q3     max 
#> 102.000   7.902   0.839   7.000   7.000   8.000   9.000   9.000 
Summarize(~v,data=d,digits=3)
#>        n     mean       sd      min       Q1   median       Q3      max 
#>  102.000    1.098    0.815    0.000    0.000    1.000    2.000    2.000 
#> percZero 
#>   28.431 

# note that the nvalid and percZero results can be forced to be shown
Summarize(~w,data=d,digits=3,nvalid="always",percZero="always")
#>        n   nvalid     mean       sd      min       Q1   median       Q3 
#>  102.000  102.000    7.902    0.839    7.000    7.000    8.000    9.000 
#>      max percZero 
#>    9.000    0.000 

## Numeric vector by levels of a factor variable
Summarize(y~g1,data=d,digits=3)
#>   g1  n nvalid  mean    sd   min    Q1 median    Q3   max percZero
#> 1  A 23     23 0.445 0.269 0.000 0.215  0.498 0.688 0.846    4.348
#> 2  B 32     32 0.442 0.314 0.000 0.175  0.434 0.663 0.997    3.125
#> 3  C 19     18 0.592 0.197 0.315 0.414  0.564 0.735 0.976    0.000
Summarize(y~g2,data=d,digits=3)
#>        g2  n nvalid  mean    sd   min    Q1 median    Q3   max percZero
#> 1 UNKNOWN 29     27 0.525 0.273 0.096 0.270  0.479 0.727 0.981    0.000
#> 2  female 35     35 0.432 0.267 0.000 0.205  0.404 0.601 0.948    2.857
#> 3    male 38     37 0.452 0.296 0.000 0.226  0.466 0.678 0.997    2.703
Summarize(y~g2,data=d,digits=3,exclude="UNKNOWN")
#>       g2  n nvalid  mean    sd min    Q1 median    Q3   max percZero
#> 1 female 35     35 0.432 0.267   0 0.205  0.404 0.601 0.948    2.857
#> 2   male 38     37 0.452 0.296   0 0.226  0.466 0.678 0.997    2.703

## Numeric vector by levels of two factor variables
Summarize(y~g1+g2,data=d,digits=3)
#>   g1      g2  n nvalid  mean    sd   min    Q1 median    Q3   max percZero
#> 1  A UNKNOWN  4      4 0.610 0.116 0.479 0.530  0.621 0.701 0.718    0.000
#> 2  B UNKNOWN  8      8 0.444 0.298 0.175 0.197  0.364 0.594 0.981    0.000
#> 3  C UNKNOWN  9      8 0.624 0.208 0.389 0.433  0.642 0.745 0.976    0.000
#> 4  A  female 10     10 0.313 0.251 0.000 0.157  0.267 0.497 0.779   10.000
#> 5  B  female  7      7 0.354 0.346 0.034 0.055  0.290 0.547 0.948    0.000
#> 6  C  female  7      7 0.588 0.192 0.336 0.466  0.532 0.720 0.875    0.000
#> 7  A    male  9      9 0.518 0.285 0.031 0.290  0.560 0.713 0.846    0.000
#> 8  B    male 17     17 0.477 0.319 0.000 0.290  0.480 0.672 0.997    5.882
#> 9  C    male  3      3 0.517 0.231 0.315 0.391  0.466 0.618 0.770    0.000
Summarize(y~g1+g2,data=d,digits=3,exclude="UNKNOWN")
#>   g1     g2  n  mean    sd   min    Q1 median    Q3   max percZero
#> 1  A female 10 0.313 0.251 0.000 0.157  0.267 0.497 0.779   10.000
#> 2  B female  7 0.354 0.346 0.034 0.055  0.290 0.547 0.948    0.000
#> 3  C female  7 0.588 0.192 0.336 0.466  0.532 0.720 0.875    0.000
#> 4  A   male  9 0.518 0.285 0.031 0.290  0.560 0.713 0.846    0.000
#> 5  B   male 17 0.477 0.319 0.000 0.290  0.480 0.672 0.997    5.882
#> 6  C   male  3 0.517 0.231 0.315 0.391  0.466 0.618 0.770    0.000

## What happens if RHS of formula is not a factor
Summarize(y~w,data=d,digits=3)
#>   w  n nvalid  mean    sd   min    Q1 median    Q3   max percZero
#> 1 7 41     41 0.388 0.271 0.000 0.196  0.354 0.530 0.976    2.439
#> 2 8 30     27 0.462 0.262 0.000 0.235  0.499 0.676 0.948    3.704
#> 3 9 31     31 0.569 0.280 0.051 0.389  0.547 0.802 0.997    0.000

## Summarizing multiple variables in a data.frame (must reduce to numerics)
lapply(as.list(d[,1:3]),Summarize,digits=4)
#> $y
#>        n   nvalid     mean       sd      min       Q1   median       Q3 
#> 102.0000  99.0000   0.4649   0.2793   0.0000   0.2334   0.4660   0.6844 
#>      max percZero 
#>   0.9971   2.0202 
#> 
#> $w
#>        n     mean       sd      min       Q1   median       Q3      max 
#> 102.0000   7.9020   0.8385   7.0000   7.0000   8.0000   9.0000   9.0000 
#> 
#> $v
#>        n     mean       sd      min       Q1   median       Q3      max 
#> 102.0000   1.0980   0.8146   0.0000   0.0000   1.0000   2.0000   2.0000 
#> percZero 
#>  28.4314 
#> 
```
