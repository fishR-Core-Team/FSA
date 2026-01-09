# Computes standard error of the mean.

Computes the standard error of the mean (i.e., standard deviation
divided by the square root of the sample size).

## Usage

``` r
se(x, na.rm = TRUE)
```

## Arguments

- x:

  A numeric vector.

- na.rm:

  A logical that indicates whether missing values should be removed
  before computing the standard error.

## Value

A single numeric that is the standard error of the mean of `x`.

## Details

The standard error of the value in vector `x` is simply the standard
deviation of `x` divided by the square root of the number of valid items
in `x`

## See also

See `se` in sciplot for similar functionality.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
# example vector
x <- 1:20
se(x)
#> [1] 1.322876
sd(x)/sqrt(length(x))   ## matches
#> [1] 1.322876

# all return NA if missing values are not removed
x2 <- c(x,NA)
sd(x2)/sqrt(length(x2))
#> [1] NA

# Better if missing values are removed
se(x2)              ## Default behavior
#> [1] 1.322876
sd(x2,na.rm=TRUE)/sqrt(length(x2[complete.cases(x2)]))  ## Matches
#> [1] 1.322876
se(x2,na.rm=FALSE)  ## Result from not removing NAs
#> [1] NA
```
