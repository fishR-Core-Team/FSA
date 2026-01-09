# Extract the coefficient of determination from a linear model object.

Extracts the coefficient of determination (i.e., “r-squared”) from a
linear model (i.e., `lm`) object.

## Usage

``` r
rSquared(object, ...)

# Default S3 method
rSquared(object, ...)

# S3 method for class 'lm'
rSquared(object, digits = getOption("digits"), percent = FALSE, ...)
```

## Arguments

- object:

  An object saved from `lm`.

- ...:

  Additional arguments for methods.

- digits:

  A single number that is the number of digits to round the returned
  result to.

- percent:

  A logical that indicates if the result should be returned as a
  percentage (`=TRUE`) or as a proportion (`=FALSE`; default).

## Value

A numeric, as either a proportion or percentage, that is the coefficient
of determination for a linear model.

## Details

This is a convenience function to extract the `r.squared` part from
`summary(lm)`.

## Examples

``` r
lm1 <- lm(mirex~weight, data=Mirex)
rSquared(lm1)
#> [1] 0.1812022
rSquared(lm1,digits=3)
#> [1] 0.181
rSquared(lm1,digits=1,percent=TRUE)
#> [1] 18.1

## rSquared only works with lm objects
if (FALSE) { # \dontrun{
nls1 <- nls(mirex~a*weight^b,data=Mirex,start=list(a=1,b=1))
rSquared(nls1)
} # }
```
