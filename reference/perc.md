# Computes the percentage of values in a vector less than or greater than (and equal to) some value.

Computes the percentage of values in a vector less than or greater than
(and equal to) a user-supplied value.

## Usage

``` r
perc(
  x,
  val,
  dir = c("geq", "gt", "leq", "lt"),
  na.rm = TRUE,
  digits = getOption("digits")
)
```

## Arguments

- x:

  A numeric vector.

- val:

  A single numeric value.

- dir:

  A string that indicates whether the percentage is for values in `x`
  that are “greater than and equal” `"geq"`, “greater than” `"gt"`,
  “less than and equal” `"leq"`, “less than” `"lt"` the value in `val`.

- na.rm:

  A logical that indicates whether `NA` values should be removed
  (DEFAULT) from `x` or not.

- digits:

  A single numeric that indicates the number of decimals the percentage
  should be rounded to.

## Value

A single numeric that is the percentage of values in `x` that meet the
criterion in `dir` relative to `val`.

## Details

This function is most useful when used with an apply-type of function.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
## vector of values
( tmp <- c(1:8,NA,NA) )
#>  [1]  1  2  3  4  5  6  7  8 NA NA

## percentages excluding NA values
perc(tmp,5)
#> [1] 50
perc(tmp,5,"gt")
#> [1] 37.5
perc(tmp,5,"leq")
#> [1] 62.5
perc(tmp,5,"lt")
#> [1] 50

## percentages including NA values
perc(tmp,5,na.rm=FALSE)
#> [1] 40
perc(tmp,5,"gt",na.rm=FALSE)
#> [1] 30
perc(tmp,5,"leq",na.rm=FALSE)
#> [1] 50
perc(tmp,5,"lt",na.rm=FALSE)
#> [1] 40
```
