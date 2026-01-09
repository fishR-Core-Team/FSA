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

|                         |                                                                                   |
|-------------------------|-----------------------------------------------------------------------------------|
| Formula                 | Description of Summary                                                            |
| `~quant`                | Numerical summaries (see below) of `quant`.                                       |
| `quant~factor`          | Summaries of `quant` separated by levels in `factor`.                             |
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
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>  0.0000  0.2762  0.5002  0.5034  0.7158  0.9971       3 

# this function           
Summarize(d$y,digits=3)
#>        n   nvalid     mean       sd      min       Q1   median       Q3 
#>  102.000   99.000    0.503    0.276    0.000    0.276    0.500    0.716 
#>      max percZero 
#>    0.997    2.020 
Summarize(~y,data=d,digits=3)
#>        n   nvalid     mean       sd      min       Q1   median       Q3 
#>  102.000   99.000    0.503    0.276    0.000    0.276    0.500    0.716 
#>      max percZero 
#>    0.997    2.020 
Summarize(y~1,data=d,digits=3)
#>        n   nvalid     mean       sd      min       Q1   median       Q3 
#>  102.000   99.000    0.503    0.276    0.000    0.276    0.500    0.716 
#>      max percZero 
#>    0.997    2.020 

# note that nvalid is not shown if there are no NAs and
#   percZero is not shown if there are no zeros
Summarize(~w,data=d,digits=3)
#>       n    mean      sd     min      Q1  median      Q3     max 
#> 102.000   8.020   0.867   7.000   7.000   8.000   9.000   9.000 
Summarize(~v,data=d,digits=3)
#>        n     mean       sd      min       Q1   median       Q3      max 
#>  102.000    1.029    0.777    0.000    0.000    1.000    2.000    2.000 
#> percZero 
#>   28.431 

# note that the nvalid and percZero results can be forced to be shown
Summarize(~w,data=d,digits=3,nvalid="always",percZero="always")
#>        n   nvalid     mean       sd      min       Q1   median       Q3 
#>  102.000  102.000    8.020    0.867    7.000    7.000    8.000    9.000 
#>      max percZero 
#>    9.000    0.000 

## Numeric vector by levels of a factor variable
Summarize(y~g1,data=d,digits=3)
#>   g1  n nvalid  mean    sd   min    Q1 median    Q3   max percZero
#> 1  A 25     24 0.462 0.302 0.000 0.204  0.465 0.786 0.867    8.333
#> 2  B 32     32 0.569 0.239 0.165 0.383  0.537 0.785 0.991    0.000
#> 3  C 21     20 0.439 0.310 0.011 0.194  0.427 0.675 0.996    0.000
Summarize(y~g2,data=d,digits=3)
#>        g2  n nvalid  mean    sd  min    Q1 median    Q3   max percZero
#> 1 UNKNOWN 25     24 0.433 0.306 0.00 0.190  0.425 0.722 0.948    4.167
#> 2  female 33     33 0.460 0.289 0.00 0.213  0.418 0.713 0.991    3.030
#> 3    male 44     42 0.578 0.234 0.02 0.411  0.546 0.751 0.997    0.000
Summarize(y~g2,data=d,digits=3,exclude="UNKNOWN")
#>       g2  n nvalid  mean    sd  min    Q1 median    Q3   max percZero
#> 1 female 33     33 0.460 0.289 0.00 0.213  0.418 0.713 0.991     3.03
#> 2   male 44     42 0.578 0.234 0.02 0.411  0.546 0.751 0.997     0.00

## Numeric vector by levels of two factor variables
Summarize(y~g1+g2,data=d,digits=3)
#>   g1      g2  n nvalid  mean    sd   min    Q1 median    Q3   max percZero
#> 1  A UNKNOWN  5      5 0.300 0.351 0.000 0.028  0.175 0.461 0.835       20
#> 2  B UNKNOWN  9      9 0.614 0.248 0.315 0.403  0.494 0.814 0.948        0
#> 3  C UNKNOWN  6      5 0.318 0.328 0.011 0.065  0.195 0.547 0.770        0
#> 4  A  female 10     10 0.408 0.325 0.000 0.165  0.340 0.712 0.857       10
#> 5  B  female  9      9 0.521 0.294 0.165 0.290  0.542 0.713 0.991        0
#> 6  C  female  9      9 0.409 0.247 0.096 0.249  0.279 0.584 0.770        0
#> 7  A    male 10      9 0.611 0.191 0.401 0.469  0.519 0.806 0.867        0
#> 8  B    male 14     14 0.572 0.205 0.274 0.417  0.546 0.640 0.920        0
#> 9  C    male  6      6 0.583 0.375 0.020 0.390  0.580 0.893 0.996        0
Summarize(y~g1+g2,data=d,digits=3,exclude="UNKNOWN")
#>   g1     g2  n nvalid  mean    sd   min    Q1 median    Q3   max percZero
#> 1  A female 10     10 0.408 0.325 0.000 0.165  0.340 0.712 0.857       10
#> 2  B female  9      9 0.521 0.294 0.165 0.290  0.542 0.713 0.991        0
#> 3  C female  9      9 0.409 0.247 0.096 0.249  0.279 0.584 0.770        0
#> 4  A   male 10      9 0.611 0.191 0.401 0.469  0.519 0.806 0.867        0
#> 5  B   male 14     14 0.572 0.205 0.274 0.417  0.546 0.640 0.920        0
#> 6  C   male  6      6 0.583 0.375 0.020 0.390  0.580 0.893 0.996        0

## What happens if RHS of formula is not a factor
Summarize(y~w,data=d,digits=3)
#>   w  n nvalid  mean    sd  min    Q1 median    Q3   max percZero
#> 1 7 37     35 0.535 0.286 0.00 0.325  0.508 0.810 0.949    2.857
#> 2 8 26     26 0.504 0.260 0.02 0.340  0.521 0.600 0.997    0.000
#> 3 9 39     38 0.474 0.283 0.00 0.217  0.475 0.669 0.996    2.632

## Summarizing multiple variables in a data.frame (must reduce to numerics)
lapply(as.list(d[,1:3]),Summarize,digits=4)
#> $y
#>        n   nvalid     mean       sd      min       Q1   median       Q3 
#> 102.0000  99.0000   0.5034   0.2764   0.0000   0.2762   0.5002   0.7158 
#>      max percZero 
#>   0.9971   2.0202 
#> 
#> $w
#>        n     mean       sd      min       Q1   median       Q3      max 
#> 102.0000   8.0196   0.8672   7.0000   7.0000   8.0000   9.0000   9.0000 
#> 
#> $v
#>        n     mean       sd      min       Q1   median       Q3      max 
#> 102.0000   1.0294   0.7766   0.0000   0.0000   1.0000   2.0000   2.0000 
#> percZero 
#>  28.4314 
#> 
```
