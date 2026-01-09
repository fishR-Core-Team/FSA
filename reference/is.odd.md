# Determine if a number is odd or even.

Determine if a number is odd or even.

## Usage

``` r
is.odd(x)

is.even(x)
```

## Arguments

- x:

  A numeric vector.

## Value

A logical vector of the same length as x.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
## Individual values
is.odd(1)
#> [1] TRUE
is.odd(2)
#> [1] FALSE
is.even(3)
#> [1] FALSE
is.even(4)
#> [1] TRUE

## Vector of values
d <- 1:8
data.frame(d,odd=is.odd(d),even=is.even(d))
#>   d   odd  even
#> 1 1  TRUE FALSE
#> 2 2 FALSE  TRUE
#> 3 3  TRUE FALSE
#> 4 4 FALSE  TRUE
#> 5 5  TRUE FALSE
#> 6 6 FALSE  TRUE
#> 7 7  TRUE FALSE
#> 8 8 FALSE  TRUE
```
